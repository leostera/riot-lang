use std::collections::BTreeMap;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr, bail};
use tempfile::TempDir;

use crate::ast::{AstDecl, AstProgram};
use crate::checker::{CheckMode, typecheck};
use crate::cli::{Cli, Command, CompileArgs, CompileLibArgs, EmitArgs, EmitPass, ObjectMapping};
use crate::codegen::{
    CodegenMode, emit_assembly_text, emit_executable_object, emit_llvm_text, emit_module_object,
};
use crate::ir::{lower_rir_to_actor_ir, lower_typed_to_rir};
use crate::linker::link_executable;
use crate::parser::parse_source;
use crate::runtime::{build_runtime, find_repo_root};
use crate::signature::{ImportedSignatures, ModuleName, resolve_rsig, write_rsig};

pub(crate) fn run(cli: Cli) -> miette::Result<()> {
    match cli.command {
        Command::Compile(args) => compile(args),
        Command::CompileLib(args) => compile_lib(args),
        Command::Emit(args) => emit(args),
    }
}

fn compile(args: CompileArgs) -> miette::Result<()> {
    let source = load_source(&args.input)?;
    let ast = parse_source(&args.input, &source)?;
    let imports = resolve_imports(&args.input, &ast, &args.sig_dirs)?;
    let module_name = module_name_from_path(&args.input)?;
    let checked = typecheck(
        &args.input,
        &source,
        module_name,
        ast,
        &imports,
        CheckMode::Executable,
    )?;
    let rir = lower_typed_to_rir(checked.typed_tree);

    let repo = find_repo_root()?;
    let runtime = build_runtime(&repo)?;
    let temps = TempDir::new()
        .into_diagnostic()
        .wrap_err("failed to create stage0 temp directory")?;
    let object = Utf8PathBuf::from_path_buf(temps.path().join("main.o"))
        .map_err(|path| miette::miette!("temp path is not utf-8: {}", path.display()))?;
    emit_executable_object(&rir, &imports, &object, args.emit_llvm.as_deref())?;

    let imported_objects = resolve_imported_objects(&rir.uses, &args.objects, &args.object_dirs)?;
    link_executable(&object, &imported_objects, &runtime, &args.output)?;

    Ok(())
}

fn compile_lib(args: CompileLibArgs) -> miette::Result<()> {
    let source = load_source(&args.input)?;
    let ast = parse_source(&args.input, &source)?;
    let imports = resolve_imports(&args.input, &ast, &args.sig_dirs)?;
    let module_name = module_name_from_path(&args.input)?;
    let checked = typecheck(
        &args.input,
        &source,
        module_name.clone(),
        ast,
        &imports,
        CheckMode::Interface,
    )?;
    let rir = lower_typed_to_rir(checked.typed_tree);

    std::fs::create_dir_all(args.out_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to create {}", args.out_dir))?;
    let rsig_path = args.out_dir.join(format!("{module_name}.rsig"));
    let object_path = args.out_dir.join(format!("{module_name}.o"));
    write_rsig(&rsig_path, &checked.signature)?;
    emit_module_object(&rir, &imports, &object_path, args.emit_llvm.as_deref())?;

    Ok(())
}

fn emit(args: EmitArgs) -> miette::Result<()> {
    let source = load_source(&args.input)?;
    let ast = parse_source(&args.input, &source)?;
    let imports = resolve_imports(&args.input, &ast, &args.sig_dirs)?;
    let module_name = module_name_from_path(&args.input)?;
    let checked = typecheck(
        &args.input,
        &source,
        module_name,
        ast.clone(),
        &imports,
        CheckMode::Interface,
    )?;

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
            let rir = lower_typed_to_rir(checked.typed_tree);
            write_text(args.output.as_deref(), &format!("{rir:#?}"))
        }
        EmitPass::ActorIr => {
            let rir = lower_typed_to_rir(checked.typed_tree);
            let actor_ir = lower_rir_to_actor_ir(&rir, &imports);
            write_text(args.output.as_deref(), &format!("{actor_ir:#?}"))
        }
        EmitPass::Llvm => {
            let rir = lower_typed_to_rir(checked.typed_tree);
            write_text(
                args.output.as_deref(),
                &emit_llvm_text(&rir, &imports, emit_mode_for(&rir))?,
            )
        }
        EmitPass::Assembly => {
            let rir = lower_typed_to_rir(checked.typed_tree);
            write_text(
                args.output.as_deref(),
                &emit_assembly_text(&rir, &imports, emit_mode_for(&rir))?,
            )
        }
        EmitPass::Object => {
            let rir = lower_typed_to_rir(checked.typed_tree);
            let output = args
                .output
                .unwrap_or_else(|| args.input.with_extension("o"));
            emit_module_object(&rir, &imports, &output, None)
        }
        EmitPass::All => {
            let rir = lower_typed_to_rir(checked.typed_tree.clone());
            let actor_ir = lower_rir_to_actor_ir(&rir, &imports);
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
            text.push_str(&emit_llvm_text(&rir, &imports, emit_mode_for(&rir))?);
            write_text(args.output.as_deref(), &text)
        }
    }
}

fn emit_mode_for(rir: &crate::ir::RirProgram) -> CodegenMode {
    if rir.functions.iter().any(|function| function.name == "main") {
        CodegenMode::Executable
    } else {
        CodegenMode::Module
    }
}

fn load_source(input: &Utf8Path) -> miette::Result<String> {
    std::fs::read_to_string(input.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read {input}"))
}

fn resolve_imports(
    source_path: &Utf8Path,
    ast: &AstProgram,
    sig_dirs: &[Utf8PathBuf],
) -> miette::Result<ImportedSignatures> {
    let source_dir = source_path.parent();
    let mut imports = BTreeMap::new();
    for decl in &ast.decls {
        if let AstDecl::Use(use_) = decl {
            let (_path, rsig) = resolve_rsig(&use_.name, source_dir, sig_dirs)?;
            imports.insert(ModuleName::new(use_.name.clone()), rsig);
        }
    }
    Ok(imports)
}

fn resolve_imported_objects(
    uses: &[String],
    mappings: &[ObjectMapping],
    object_dirs: &[Utf8PathBuf],
) -> miette::Result<Vec<Utf8PathBuf>> {
    let explicit = mappings
        .iter()
        .map(|mapping| (mapping.module.as_str(), mapping.path.clone()))
        .collect::<BTreeMap<_, _>>();
    let mut objects = Vec::new();
    for module in uses {
        if let Some(path) = explicit.get(module.as_str()) {
            objects.push(path.clone());
            continue;
        }
        let mut candidates = Vec::new();
        for dir in object_dirs {
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

pub(crate) fn module_name_from_path(path: &Utf8Path) -> miette::Result<String> {
    let stem = path.file_stem().unwrap_or("Main");
    if !is_lower_module_stem(stem) {
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
        Ok("Main".to_owned())
    } else {
        Ok(output)
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
