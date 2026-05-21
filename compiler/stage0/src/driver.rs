use std::collections::BTreeMap;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr, bail};
use tempfile::TempDir;

use crate::actor::StacklessActorLowerer;
use crate::ast::{AstDecl, AstProgram};
use crate::backend::llvm::{
    CodegenMode, emit_assembly_text, emit_executable_object, emit_llvm_text, emit_module_object,
};
use crate::checker::{CheckMode, Checker};
use crate::cli::{Cli, Command, CompileArgs, CompileLibArgs, EmitArgs, EmitPass, ObjectMapping};
use crate::lambda::LambdaSimplifier;
use crate::linker::link_executable;
use crate::parser::parse_source;
use crate::runtime::{build_runtime, find_repo_root};
use crate::signature::{ImportedSignatures, ModuleName, resolve_rsig, write_rsig};

pub(crate) fn run(cli: Cli) -> miette::Result<()> {
    Stage0Driver::new().run(cli)
}

#[derive(Debug, Default)]
pub(crate) struct Stage0Driver {
    source_loader: SourceLoader,
}

impl Stage0Driver {
    pub(crate) fn new() -> Self {
        Self {
            source_loader: SourceLoader::new(),
        }
    }

    pub(crate) fn run(&self, cli: Cli) -> miette::Result<()> {
        match cli.command {
            Command::Compile(args) => self.compile(args),
            Command::CompileLib(args) => self.compile_lib(args),
            Command::Emit(args) => self.emit(args),
        }
    }

    fn compile(&self, args: CompileArgs) -> miette::Result<()> {
        let units = self.source_loader.parse_units(&args.inputs)?;
        if units.len() > 1 && args.emit_llvm.is_some() {
            bail!("--emit-llvm is only supported with a single compile input for now");
        }
        let graph = SourceGraph::new(&units);
        let main_index = if units.len() == 1 {
            0
        } else {
            graph.executable_unit_index()?
        };
        if units.len() > 1 {
            graph.ensure_no_module_depends_on_entrypoint(main_index)?;
        }
        let order = graph.topological_order()?;

        let repo = find_repo_root()?;
        let runtime = build_runtime(&repo)?;
        let temps = TempDir::new()
            .into_diagnostic()
            .wrap_err("failed to create stage0 temp directory")?;
        let temp_dir = Utf8PathBuf::from_path_buf(temps.path().to_path_buf())
            .map_err(|path| miette::miette!("temp path is not utf-8: {}", path.display()))?;
        let mut compiled = ImportedSignatures::new();
        let mut compiled_objects = BTreeMap::new();

        for index in order {
            if index == main_index {
                continue;
            }
            let artifact = self.compile_module_unit(
                &units[index],
                &compiled,
                &args.sig_dirs,
                &temp_dir,
                None,
            )?;
            compiled_objects.insert(units[index].module_name.clone(), artifact.object);
            compiled.insert(units[index].module_name.clone(), artifact.signature);
        }

        let main_imports = ImportResolver::new(&args.sig_dirs, &compiled)
            .resolve_source(&units[main_index].path, &units[main_index].ast)?;
        let checked = Checker::new(
            &units[main_index].path,
            &units[main_index].source,
            units[main_index].module_name.clone(),
            &main_imports,
            CheckMode::Executable,
        )
        .check(units[main_index].ast.clone())?;
        let rir = LambdaSimplifier::new().simplify(checked.typed_tree);

        let object = temp_dir.join("main.o");
        emit_executable_object(&rir, &main_imports, &object, args.emit_llvm.as_deref())?;

        let imported_objects = ImportedObjectResolver::new(
            &main_imports,
            &compiled_objects,
            &args.objects,
            &args.object_dirs,
        )
        .resolve(&rir.uses)?;
        let imported_objects = Self::with_compiled_objects(imported_objects, &compiled_objects);
        link_executable(&object, &imported_objects, &runtime, &args.output)?;

        Ok(())
    }

    fn with_compiled_objects(
        mut objects: Vec<Utf8PathBuf>,
        compiled_objects: &BTreeMap<ModuleName, Utf8PathBuf>,
    ) -> Vec<Utf8PathBuf> {
        for object in compiled_objects.values() {
            if !objects.iter().any(|existing| existing == object) {
                objects.push(object.clone());
            }
        }
        objects
    }

    fn compile_lib(&self, args: CompileLibArgs) -> miette::Result<()> {
        let units = self.source_loader.parse_units(&args.inputs)?;
        if units.len() > 1 && args.emit_llvm.is_some() {
            bail!("--emit-llvm is only supported with a single compile-lib input for now");
        }
        std::fs::create_dir_all(args.out_dir.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("failed to create {}", args.out_dir))?;
        let graph = SourceGraph::new(&units);
        let order = graph.topological_order()?;
        let mut compiled = ImportedSignatures::new();
        for index in order {
            let artifact = self.compile_module_unit(
                &units[index],
                &compiled,
                &args.sig_dirs,
                &args.out_dir,
                args.emit_llvm.as_deref(),
            )?;
            compiled.insert(units[index].module_name.clone(), artifact.signature);
        }

        Ok(())
    }

    fn compile_module_unit(
        &self,
        unit: &SourceUnit,
        available: &ImportedSignatures,
        sig_dirs: &[Utf8PathBuf],
        out_dir: &Utf8Path,
        emit_llvm: Option<&Utf8Path>,
    ) -> miette::Result<CompiledModule> {
        let imports = ImportResolver::new(sig_dirs, available).resolve(unit)?;
        let checked = Checker::new(
            &unit.path,
            &unit.source,
            unit.module_name.clone(),
            &imports,
            CheckMode::Interface,
        )
        .check(unit.ast.clone())?;
        let rir = LambdaSimplifier::new().simplify(checked.typed_tree);
        let rsig_path = out_dir.join(format!("{}.rsig", unit.module_name));
        let object_path = out_dir.join(format!("{}.o", unit.module_name));
        write_rsig(&rsig_path, &checked.signature)?;
        emit_module_object(&rir, &imports, &object_path, emit_llvm)?;

        Ok(CompiledModule {
            signature: checked.signature,
            object: object_path,
        })
    }

    fn emit(&self, args: EmitArgs) -> miette::Result<()> {
        let source = self.source_loader.load(&args.input)?;
        let ast = parse_source(&args.input, &source)?;
        let empty_imports = ImportedSignatures::new();
        let imports = ImportResolver::new(&args.sig_dirs, &empty_imports)
            .resolve_source(&args.input, &ast)?;
        let module_name = self.source_loader.module_name_from_path(&args.input)?;
        let checked = Checker::new(
            &args.input,
            &source,
            module_name,
            &imports,
            CheckMode::Interface,
        )
        .check(ast.clone())?;

        match args.pass {
            EmitPass::Cst => write_text(args.output.as_deref(), &format!("{ast:#?}")),
            EmitPass::Typed => write_text(
                args.output.as_deref(),
                &format!("{:#?}", checked.typed_tree),
            ),
            EmitPass::Rsig => {
                let output = args
                    .output
                    .unwrap_or_else(|| args.input.with_extension("rsig"));
                write_rsig(&output, &checked.signature)
            }
            EmitPass::Ir => {
                let rir = LambdaSimplifier::new().simplify(checked.typed_tree);
                write_text(args.output.as_deref(), &format!("{rir:#?}"))
            }
            EmitPass::ActorIr => {
                let rir = LambdaSimplifier::new().simplify(checked.typed_tree);
                let actor_ir = StacklessActorLowerer::new(&imports).lower(&rir);
                write_text(args.output.as_deref(), &format!("{actor_ir:#?}"))
            }
            EmitPass::Llvm => {
                let rir = LambdaSimplifier::new().simplify(checked.typed_tree);
                write_text(
                    args.output.as_deref(),
                    &emit_llvm_text(&rir, &imports, Self::emit_mode_for(&rir))?,
                )
            }
            EmitPass::Assembly => {
                let rir = LambdaSimplifier::new().simplify(checked.typed_tree);
                write_text(
                    args.output.as_deref(),
                    &emit_assembly_text(&rir, &imports, Self::emit_mode_for(&rir))?,
                )
            }
            EmitPass::Object => {
                let rir = LambdaSimplifier::new().simplify(checked.typed_tree);
                let output = args
                    .output
                    .unwrap_or_else(|| args.input.with_extension("o"));
                emit_module_object(&rir, &imports, &output, None)
            }
            EmitPass::All => {
                let rir = LambdaSimplifier::new().simplify(checked.typed_tree.clone());
                let actor_ir = StacklessActorLowerer::new(&imports).lower(&rir);
                let mut text = String::new();
                text.push_str("== cst ==\n");
                text.push_str(&format!("{ast:#?}\n"));
                text.push_str("== typed ==\n");
                text.push_str(&format!("{:#?}\n", checked.typed_tree));
                text.push_str("== rsig ==\n");
                text.push_str(&checked.signature.canonical_text());
                text.push_str("== ir ==\n");
                text.push_str(&format!("{rir:#?}\n"));
                text.push_str("== actor-ir ==\n");
                text.push_str(&format!("{actor_ir:#?}\n"));
                text.push_str("== llvm ==\n");
                text.push_str(&emit_llvm_text(&rir, &imports, Self::emit_mode_for(&rir))?);
                write_text(args.output.as_deref(), &text)
            }
        }
    }

    fn emit_mode_for(rir: &crate::lambda::ir::RirProgram) -> CodegenMode {
        if rir.functions.iter().any(|function| function.name == "main") {
            CodegenMode::Executable
        } else {
            CodegenMode::Module
        }
    }
}

#[derive(Clone, Debug)]
struct SourceUnit {
    path: Utf8PathBuf,
    source: String,
    ast: AstProgram,
    module_name: ModuleName,
}

struct CompiledModule {
    signature: crate::signature::Rsig,
    object: Utf8PathBuf,
}

#[derive(Debug, Default)]
struct SourceLoader;

impl SourceLoader {
    fn new() -> Self {
        Self
    }

    fn load(&self, input: &Utf8Path) -> miette::Result<String> {
        std::fs::read_to_string(input.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("failed to read {input}"))
    }

    fn parse_units(&self, inputs: &[Utf8PathBuf]) -> miette::Result<Vec<SourceUnit>> {
        if inputs.is_empty() {
            bail!("expected at least one source file");
        }
        let mut units = Vec::new();
        let mut modules = BTreeMap::new();
        for input in inputs {
            let source = self.load(input)?;
            let ast = parse_source(input, &source)?;
            let module_name = self.module_name_from_path(input)?;
            if let Some(previous) = modules.insert(module_name.clone(), input.clone()) {
                bail!(
                    "duplicate module {module_name}\n{} and {} both compile to module {module_name}",
                    previous,
                    input
                );
            }
            units.push(SourceUnit {
                path: input.clone(),
                source,
                ast,
                module_name,
            });
        }
        Ok(units)
    }

    fn module_name_from_path(&self, path: &Utf8Path) -> miette::Result<ModuleName> {
        let stem = path.file_stem().unwrap_or("Main");
        if !Self::is_lower_module_stem(stem) {
            bail!(
                "invalid module file name `{stem}`\nfile stems must be lowercase, for example `hello.ml` or `hello_world.ml`"
            );
        }
        let mut output = String::new();
        let mut capitalize = true;
        for ch in stem.chars() {
            if ch == '_' || ch == '-' {
                capitalize = true;
            } else if ch.is_ascii_alphanumeric() {
                if output.is_empty() && ch.is_ascii_digit() {
                    output.push('M');
                }
                if capitalize && ch.is_ascii_lowercase() {
                    output.push(ch.to_ascii_uppercase());
                } else {
                    output.push(ch);
                }
                capitalize = false;
            } else {
                capitalize = true;
            }
        }
        if output.is_empty() {
            Ok(ModuleName::new("Main"))
        } else {
            Ok(ModuleName::new(output))
        }
    }

    fn is_lower_module_stem(stem: &str) -> bool {
        if stem.is_empty()
            || stem.starts_with('_')
            || stem.starts_with('-')
            || stem.ends_with('_')
            || stem.ends_with('-')
        {
            return false;
        }
        let mut previous_separator = false;
        for ch in stem.chars() {
            match ch {
                'a'..='z' | '0'..='9' => previous_separator = false,
                '_' | '-' if !previous_separator => previous_separator = true,
                _ => return false,
            }
        }
        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Unvisited,
    Visiting,
    Visited,
}

#[derive(Debug)]
struct SourceGraph<'a> {
    units: &'a [SourceUnit],
    modules: BTreeMap<ModuleName, usize>,
}

impl<'a> SourceGraph<'a> {
    fn new(units: &'a [SourceUnit]) -> Self {
        let modules = units
            .iter()
            .enumerate()
            .map(|(index, unit)| (unit.module_name.clone(), index))
            .collect();
        Self { units, modules }
    }

    fn executable_unit_index(&self) -> miette::Result<usize> {
        let main_units = self
            .units
            .iter()
            .enumerate()
            .filter(|(_, unit)| Self::unit_has_main(unit))
            .map(|(index, _)| index)
            .collect::<Vec<_>>();
        match main_units.as_slice() {
            [index] => Ok(*index),
            [] => bail!("stage0 compile with multiple inputs requires one module defining main"),
            _ => {
                bail!(
                    "stage0 compile with multiple inputs found more than one module defining main"
                )
            }
        }
    }

    fn ensure_no_module_depends_on_entrypoint(&self, main_index: usize) -> miette::Result<()> {
        let main_module = &self.units[main_index].module_name;
        for unit in self.units {
            if &unit.module_name == main_module {
                continue;
            }
            if Self::dependencies(&unit.ast)
                .iter()
                .any(|dependency| dependency == main_module)
            {
                bail!(
                    "module {} depends on executable module {main_module}; split shared code into a library module",
                    unit.module_name
                );
            }
        }
        Ok(())
    }

    fn topological_order(&self) -> miette::Result<Vec<usize>> {
        let mut state = vec![VisitState::Unvisited; self.units.len()];
        let mut stack = Vec::new();
        let mut order = Vec::new();

        for index in 0..self.units.len() {
            self.visit_source_unit(index, &mut state, &mut stack, &mut order)?;
        }

        Ok(order)
    }

    fn visit_source_unit(
        &self,
        index: usize,
        state: &mut [VisitState],
        stack: &mut Vec<usize>,
        order: &mut Vec<usize>,
    ) -> miette::Result<()> {
        match state[index] {
            VisitState::Visited => return Ok(()),
            VisitState::Visiting => {
                let cycle_start = stack
                    .iter()
                    .position(|candidate| *candidate == index)
                    .unwrap_or(0);
                let mut cycle = stack[cycle_start..]
                    .iter()
                    .map(|candidate| self.units[*candidate].module_name.to_string())
                    .collect::<Vec<_>>();
                cycle.push(self.units[index].module_name.to_string());
                bail!("module graph contains a cycle: {}", cycle.join(" -> "));
            }
            VisitState::Unvisited => {}
        }

        state[index] = VisitState::Visiting;
        stack.push(index);
        for dependency in Self::dependencies(&self.units[index].ast) {
            if let Some(dependency_index) = self.modules.get(&dependency) {
                self.visit_source_unit(*dependency_index, state, stack, order)?;
            }
        }
        stack.pop();
        state[index] = VisitState::Visited;
        order.push(index);
        Ok(())
    }

    fn dependencies(ast: &AstProgram) -> Vec<ModuleName> {
        ast.decls
            .iter()
            .filter_map(|decl| match decl {
                AstDecl::Use(use_) => Some(ModuleName::new(use_.name.clone())),
                AstDecl::Module(module) => Some(ModuleName::new(module.name.clone())),
                AstDecl::Include(include) => Some(ModuleName::new(include.name.clone())),
                AstDecl::External(_) | AstDecl::Type(_) | AstDecl::Function(_) => None,
            })
            .collect()
    }

    fn unit_has_main(unit: &SourceUnit) -> bool {
        unit.ast
            .decls
            .iter()
            .any(|decl| matches!(decl, AstDecl::Function(function) if function.name == "main"))
    }
}

#[derive(Debug)]
struct ImportResolver<'a> {
    sig_dirs: &'a [Utf8PathBuf],
    available: &'a ImportedSignatures,
}

impl<'a> ImportResolver<'a> {
    fn new(sig_dirs: &'a [Utf8PathBuf], available: &'a ImportedSignatures) -> Self {
        Self {
            sig_dirs,
            available,
        }
    }

    fn resolve(&self, unit: &SourceUnit) -> miette::Result<ImportedSignatures> {
        self.resolve_source(&unit.path, &unit.ast)
    }

    fn resolve_source(
        &self,
        source_path: &Utf8Path,
        ast: &AstProgram,
    ) -> miette::Result<ImportedSignatures> {
        let source_dir = source_path.parent();
        let mut imports = BTreeMap::new();
        for decl in &ast.decls {
            if let AstDecl::Use(use_) = decl {
                let rsig = if let Some(rsig) = self.available.get(use_.name.as_str()) {
                    rsig.clone()
                } else {
                    match resolve_rsig(&use_.name, source_dir, self.sig_dirs) {
                        Ok((_path, rsig)) => rsig,
                        Err(error) => {
                            if let Some(rsig) = crate::stdlib::std_signature(&use_.name)? {
                                rsig
                            } else {
                                return Err(error);
                            }
                        }
                    }
                };
                imports.insert(ModuleName::new(use_.name.clone()), rsig);
            }
        }
        Ok(imports)
    }
}

#[derive(Debug)]
struct ImportedObjectResolver<'a> {
    imports: &'a ImportedSignatures,
    available_objects: &'a BTreeMap<ModuleName, Utf8PathBuf>,
    mappings: &'a [ObjectMapping],
    object_dirs: &'a [Utf8PathBuf],
}

impl<'a> ImportedObjectResolver<'a> {
    fn new(
        imports: &'a ImportedSignatures,
        available_objects: &'a BTreeMap<ModuleName, Utf8PathBuf>,
        mappings: &'a [ObjectMapping],
        object_dirs: &'a [Utf8PathBuf],
    ) -> Self {
        Self {
            imports,
            available_objects,
            mappings,
            object_dirs,
        }
    }

    fn resolve(&self, uses: &[ModuleName]) -> miette::Result<Vec<Utf8PathBuf>> {
        let explicit = self
            .mappings
            .iter()
            .map(|mapping| (mapping.module.as_str(), mapping.path.clone()))
            .collect::<BTreeMap<_, _>>();
        let mut objects = Vec::new();
        for module in uses {
            if self
                .imports
                .get(module)
                .is_some_and(crate::stdlib::is_std_signature)
            {
                continue;
            }
            if let Some(path) = self.available_objects.get(module) {
                objects.push(path.clone());
                continue;
            }
            if let Some(path) = explicit.get(module.as_str()) {
                objects.push(path.clone());
                continue;
            }
            let mut candidates = Vec::new();
            for dir in self.object_dirs {
                candidates.push(dir.join(format!("{module}.o")));
            }
            if let Some(found) = candidates.iter().find(|path| path.exists()) {
                objects.push(found.clone());
                continue;
            }
            bail!(
                "missing object for imported module {module}\nsearched:\n{}",
                candidates
                    .iter()
                    .map(|path| format!("  - {path}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }
        Ok(objects)
    }
}

fn write_text(path: Option<&Utf8Path>, text: &str) -> miette::Result<()> {
    if let Some(path) = path {
        std::fs::write(path.as_std_path(), text)
            .into_diagnostic()
            .wrap_err_with(|| format!("failed to write {path}"))
    } else {
        print!("{text}");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{SourceGraph, SourceLoader, SourceUnit};
    use crate::parser::parse_source;
    use camino::Utf8PathBuf;

    #[test]
    fn source_order_tracks_use_mod_and_include_edges() {
        let units = vec![
            source_unit(
                "root.ml",
                "use Used\npub mod Child\ninclude Shared\nfn root() { 0 }\n",
            ),
            source_unit("shared.ml", "fn shared() { 1 }\n"),
            source_unit("used.ml", "fn used() { 2 }\n"),
            source_unit("child.ml", "fn child() { 3 }\n"),
        ];

        let order = SourceGraph::new(&units).topological_order().unwrap();
        let ordered_modules = order
            .iter()
            .map(|index| units[*index].module_name.as_str())
            .collect::<Vec<_>>();
        let root = position(&ordered_modules, "Root");

        assert!(position(&ordered_modules, "Used") < root);
        assert!(position(&ordered_modules, "Child") < root);
        assert!(position(&ordered_modules, "Shared") < root);
    }

    #[test]
    fn source_order_rejects_cycles() {
        let units = vec![
            source_unit("a.ml", "use B\nfn a() { 0 }\n"),
            source_unit("b.ml", "use A\nfn b() { 0 }\n"),
        ];

        let error = SourceGraph::new(&units)
            .topological_order()
            .unwrap_err()
            .to_string();

        assert!(error.contains("module graph contains a cycle"));
        assert!(error.contains("A -> B -> A"));
    }

    fn source_unit(path: &str, source: &str) -> SourceUnit {
        let path = Utf8PathBuf::from(path);
        let source_loader = SourceLoader::new();
        SourceUnit {
            module_name: source_loader.module_name_from_path(&path).unwrap(),
            ast: parse_source(&path, source).unwrap(),
            source: source.to_owned(),
            path,
        }
    }

    fn position(modules: &[&str], module: &str) -> usize {
        modules
            .iter()
            .position(|candidate| *candidate == module)
            .unwrap()
    }
}
