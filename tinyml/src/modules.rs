//! Source-module discovery for TinyML.
//!
//! The resolver turns a root source file and its `use` declarations into a
//! deterministic, dependency-first graph. It owns path policy and source loading;
//! type checking is performed separately over the resulting graph.

use std::{
    collections::HashMap,
    fs, io,
    path::{Path, PathBuf},
};

use thiserror::Error;

use crate::parser::{
    ast::{self, UseDecl},
    ident::Ident,
    lexer::Span,
};

const BUNDLED_LIBRARY: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/lib");
const BUNDLED_STANDARD_LIBRARY: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/lib/std");
const BUNDLED_HOST_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/lib/host.ml");
const BUNDLED_STRING_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/lib/std/string.ml");
const HOST_SOURCE: &str = include_str!("../lib/host.ml");
const STRING_SOURCE: &str = include_str!("../lib/std/string.ml");

const BUNDLED_SOURCES: &[(&str, &str)] = &[
    ("std/prelude.ml", include_str!("../lib/std/prelude.ml")),
    ("std/list.ml", include_str!("../lib/std/list.ml")),
    ("std/option.ml", include_str!("../lib/std/option.ml")),
    ("std/result.ml", include_str!("../lib/std/result.ml")),
    ("std/string.ml", include_str!("../lib/std/string.ml")),
];

/// A dotted TinyML module name, normalized into its source path components.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    components: Vec<String>,
}

impl ModuleName {
    pub fn from_ident(ident: &Ident) -> Result<Self, ModuleError> {
        let mut components = Vec::new();
        flatten_ident(ident, &mut components);
        for component in &components {
            if !is_upper_camel_component(component) {
                return Err(ModuleError::InvalidName {
                    name: ident_path(ident),
                    span: ident.span(),
                });
            }
        }
        Ok(Self { components })
    }

    pub fn display(&self) -> String {
        self.components.join(".")
    }

    /// Convert `Compiler.Parser` to `compiler/parser.ml`.
    pub fn source_path(&self) -> PathBuf {
        let mut path = PathBuf::new();
        for component in &self.components {
            path.push(snake_case(component));
        }
        path.set_extension("ml");
        path
    }

    pub fn to_ident(&self) -> Ident {
        let mut components = self.components.iter().rev();
        let mut ident = Ident::from_string(components.next().expect("module name has a component"));
        for component in components {
            ident = Ident::Qualified {
                name: component.clone(),
                rest: Box::new(ident),
                span: Span::new(0, 0),
            };
        }
        ident
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleKind {
    Source,
    Intrinsic(IntrinsicModule),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicModule {
    Host,
    String,
}

/// One parsed module in a dependency-first source graph.
#[derive(Debug, Clone)]
pub struct SourceModule {
    /// Name used by importers. The root source has no required external name.
    pub name: Option<ModuleName>,
    /// Canonical file path, or the stable bundled-library path when embedded.
    pub path: PathBuf,
    pub kind: ModuleKind,
    pub source: String,
    pub ast: ast::Module,
    /// Indices of direct dependencies in this graph's dependency-first order.
    pub dependencies: Vec<usize>,
}

/// The modules reachable from one root, ordered dependencies before importers.
#[derive(Debug, Clone)]
pub struct ModuleGraph {
    pub modules: Vec<SourceModule>,
    pub root: usize,
}

impl ModuleGraph {
    /// Load and parse a root module and all transitively imported sources.
    ///
    /// Search order for each import is: the importing file's directory, ordered
    /// include roots, bundled `lib/`, then bundled `lib/std/`.
    pub fn load(root: &Path, include_dirs: &[PathBuf]) -> Result<Self, ModuleError> {
        let root_path = fs::canonicalize(root).map_err(|source| ModuleError::Io {
            path: root.to_path_buf(),
            source,
        })?;
        let mut builder = Builder::new(include_dirs)?;
        let root_index = builder.visit(
            SourceFile {
                path: root_path,
                kind: ModuleKind::Source,
                embedded_source: None,
            },
            None,
            None,
        )?;
        Ok(builder.finish(root_index))
    }
}

#[derive(Debug, Error)]
pub enum ModuleError {
    #[error("could not read module source `{path}`: {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("could not parse module source `{path}`: {source}")]
    Parse {
        path: PathBuf,
        #[source]
        source: crate::parser::Error,
    },
    #[error("module `{module}` imported by `{importer}` was not found")]
    MissingModule {
        module: String,
        importer: PathBuf,
        span: Span,
    },
    #[error("invalid module name `{name}`; components must be UpperCamelCase without acronym runs")]
    InvalidName { name: String, span: Span },
    #[error("module name `{module}` resolves to both `{first}` and `{second}`")]
    DuplicateName {
        module: String,
        first: PathBuf,
        second: PathBuf,
        span: Span,
    },
    #[error("source `{path}` is imported as both `{first}` and `{second}`")]
    DuplicateSourceIdentity {
        path: PathBuf,
        first: String,
        second: String,
        span: Span,
    },
    #[error("module dependency cycle: {chain}")]
    DependencyCycle { chain: String, span: Span },
}

#[derive(Clone)]
struct SourceFile {
    path: PathBuf,
    kind: ModuleKind,
    embedded_source: Option<&'static str>,
}

struct Builder {
    include_dirs: Vec<PathBuf>,
    modules: Vec<Option<SourceModule>>,
    indices_by_path: HashMap<PathBuf, usize>,
    names_by_path: HashMap<PathBuf, ModuleName>,
    paths_by_name: HashMap<ModuleName, PathBuf>,
    active: Vec<(PathBuf, Option<ModuleName>, Span)>,
    order: Vec<usize>,
}

impl Builder {
    fn new(include_dirs: &[PathBuf]) -> Result<Self, ModuleError> {
        let mut canonical_include_dirs = Vec::with_capacity(include_dirs.len());
        for dir in include_dirs {
            let path = fs::canonicalize(dir).map_err(|source| ModuleError::Io {
                path: dir.clone(),
                source,
            })?;
            if !path.is_dir() {
                return Err(ModuleError::Io {
                    path: dir.clone(),
                    source: io::Error::new(io::ErrorKind::NotADirectory, "not a directory"),
                });
            }
            canonical_include_dirs.push(path);
        }
        Ok(Self {
            include_dirs: canonical_include_dirs,
            modules: Vec::new(),
            indices_by_path: HashMap::new(),
            names_by_path: HashMap::new(),
            paths_by_name: HashMap::new(),
            active: Vec::new(),
            order: Vec::new(),
        })
    }

    fn visit(
        &mut self,
        source_file: SourceFile,
        name: Option<ModuleName>,
        import_span: Option<Span>,
    ) -> Result<usize, ModuleError> {
        if let Some(position) = self
            .active
            .iter()
            .position(|(path, _, _)| path == &source_file.path)
        {
            let mut chain = self.active[position..]
                .iter()
                .map(|(path, name, _)| {
                    name.as_ref()
                        .map(ModuleName::display)
                        .unwrap_or_else(|| path.display().to_string())
                })
                .collect::<Vec<_>>();
            chain.push(
                name.as_ref()
                    .map(ModuleName::display)
                    .unwrap_or_else(|| source_file.path.display().to_string()),
            );
            return Err(ModuleError::DependencyCycle {
                chain: chain.join(" -> "),
                span: import_span.unwrap_or(Span::new(0, 0)),
            });
        }

        if let Some(name) = &name {
            if let Some(previous_path) = self.paths_by_name.get(name)
                && previous_path != &source_file.path
            {
                return Err(ModuleError::DuplicateName {
                    module: name.display(),
                    first: previous_path.clone(),
                    second: source_file.path,
                    span: import_span.unwrap_or(Span::new(0, 0)),
                });
            }
            if let Some(previous_name) = self.names_by_path.get(&source_file.path)
                && previous_name != name
            {
                return Err(ModuleError::DuplicateSourceIdentity {
                    path: source_file.path,
                    first: previous_name.display(),
                    second: name.display(),
                    span: import_span.unwrap_or(Span::new(0, 0)),
                });
            }
            self.paths_by_name
                .insert(name.clone(), source_file.path.clone());
            self.names_by_path
                .insert(source_file.path.clone(), name.clone());
        }

        if let Some(index) = self.indices_by_path.get(&source_file.path) {
            return Ok(*index);
        }

        let source = match source_file.embedded_source {
            Some(source) => source.to_string(),
            None => fs::read_to_string(&source_file.path).map_err(|source| ModuleError::Io {
                path: source_file.path.clone(),
                source,
            })?,
        };
        let ast = crate::parser::parse(&source).map_err(|source| ModuleError::Parse {
            path: source_file.path.clone(),
            source,
        })?;

        let index = self.modules.len();
        self.indices_by_path.insert(source_file.path.clone(), index);
        self.modules.push(Some(SourceModule {
            name: name.clone(),
            path: source_file.path.clone(),
            kind: source_file.kind,
            source,
            ast: ast.clone(),
            dependencies: Vec::new(),
        }));
        self.active.push((
            source_file.path.clone(),
            name.clone(),
            import_span.unwrap_or(Span::new(0, 0)),
        ));

        let imports = ast
            .items
            .iter()
            .filter_map(|item| match item {
                ast::ModuleItem::UseDecl(import) => Some(import.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        let mut dependencies = Vec::with_capacity(imports.len());
        for import in imports {
            let (module_ident, span) = match &import {
                UseDecl::Concrete { module, span, .. } | UseDecl::Wildcard { module, span } => {
                    (module, *span)
                }
            };
            let module_name = ModuleName::from_ident(module_ident)?;
            let dependency_path = self
                .resolve_import(&source_file.path, &module_name)?
                .ok_or_else(|| ModuleError::MissingModule {
                    module: module_name.display(),
                    importer: source_file.path.clone(),
                    span,
                })?;
            let dependency = self.visit(dependency_path, Some(module_name), Some(span))?;
            if !dependencies.contains(&dependency) {
                dependencies.push(dependency);
            }
        }
        self.active.pop();
        self.modules[index]
            .as_mut()
            .expect("module was inserted before visiting dependencies")
            .dependencies = dependencies;
        self.order.push(index);
        Ok(index)
    }

    fn resolve_import(
        &self,
        importer: &Path,
        name: &ModuleName,
    ) -> Result<Option<SourceFile>, ModuleError> {
        if name.components.as_slice() == ["Host"] {
            return Ok(Some(SourceFile {
                path: PathBuf::from(BUNDLED_HOST_PATH),
                kind: ModuleKind::Intrinsic(IntrinsicModule::Host),
                embedded_source: Some(HOST_SOURCE),
            }));
        }
        if name.components.as_slice() == ["String"] {
            return Ok(Some(SourceFile {
                path: PathBuf::from(BUNDLED_STRING_PATH),
                kind: ModuleKind::Intrinsic(IntrinsicModule::String),
                embedded_source: Some(STRING_SOURCE),
            }));
        }

        let relative = name.source_path();
        let mut roots = Vec::with_capacity(1 + self.include_dirs.len() + 2);
        if let Some(directory) = importer.parent() {
            roots.push(SearchRoot::Filesystem(directory.to_path_buf()));
        }
        roots.extend(
            self.include_dirs
                .iter()
                .cloned()
                .map(SearchRoot::Filesystem),
        );
        roots.push(SearchRoot::BundledLibrary(PathBuf::from(BUNDLED_LIBRARY)));
        roots.push(SearchRoot::BundledStandardLibrary(PathBuf::from(
            BUNDLED_STANDARD_LIBRARY,
        )));

        for root in roots {
            let candidate = root.path().join(&relative);
            if candidate.is_file() {
                let path = fs::canonicalize(&candidate).map_err(|source| ModuleError::Io {
                    path: candidate,
                    source,
                })?;
                return Ok(Some(SourceFile {
                    path,
                    kind: ModuleKind::Source,
                    embedded_source: None,
                }));
            }
            if let Some(source) = root.embedded_source(&relative) {
                return Ok(Some(SourceFile {
                    path: candidate,
                    kind: ModuleKind::Source,
                    embedded_source: Some(source),
                }));
            }
        }
        Ok(None)
    }

    fn finish(mut self, root: usize) -> ModuleGraph {
        let mut new_indices = vec![usize::MAX; self.modules.len()];
        let mut modules = Vec::with_capacity(self.order.len());
        for old_index in self.order {
            new_indices[old_index] = modules.len();
            modules.push(
                self.modules[old_index]
                    .take()
                    .expect("completed modules are present"),
            );
        }
        for module in &mut modules {
            for dependency in &mut module.dependencies {
                *dependency = new_indices[*dependency];
            }
        }
        ModuleGraph {
            modules,
            root: new_indices[root],
        }
    }
}

#[derive(Clone)]
enum SearchRoot {
    Filesystem(PathBuf),
    BundledLibrary(PathBuf),
    BundledStandardLibrary(PathBuf),
}

impl SearchRoot {
    fn path(&self) -> &Path {
        match self {
            Self::Filesystem(path)
            | Self::BundledLibrary(path)
            | Self::BundledStandardLibrary(path) => path,
        }
    }

    fn embedded_source(&self, relative: &Path) -> Option<&'static str> {
        let relative = match self {
            Self::BundledLibrary(_) => relative.to_path_buf(),
            Self::BundledStandardLibrary(_) => Path::new("std").join(relative),
            Self::Filesystem(_) => return None,
        };
        let relative = relative.to_str()?;
        BUNDLED_SOURCES
            .iter()
            .find_map(|(path, source)| (*path == relative).then_some(*source))
    }
}

fn flatten_ident(ident: &Ident, components: &mut Vec<String>) {
    match ident {
        Ident::Name { name, .. } => components.push(name.clone()),
        Ident::Qualified { name, rest, .. } => {
            components.push(name.clone());
            flatten_ident(rest, components);
        }
    }
}

fn ident_path(ident: &Ident) -> String {
    match ident {
        Ident::Name { name, .. } => name.clone(),
        Ident::Qualified { name, rest, .. } => format!("{name}.{}", ident_path(rest)),
    }
}

fn is_upper_camel_component(component: &str) -> bool {
    let mut chars = component.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_uppercase() {
        return false;
    }

    let mut previous_was_uppercase = true;
    for character in chars {
        if !character.is_ascii_alphanumeric()
            || character.is_ascii_uppercase() && previous_was_uppercase
        {
            return false;
        }
        previous_was_uppercase = character.is_ascii_uppercase();
    }
    true
}

fn snake_case(component: &str) -> String {
    let mut path = String::new();
    for (index, character) in component.chars().enumerate() {
        if character.is_ascii_uppercase() {
            if index != 0 {
                path.push('_');
            }
            path.push(character.to_ascii_lowercase());
        } else {
            path.push(character);
        }
    }
    path
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_dir(label: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock is after epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!(
            "tinyml-modules-{label}-{}-{nonce}",
            std::process::id()
        ));
        fs::create_dir_all(&dir).expect("create temporary directory");
        dir
    }

    fn write_module(dir: &Path, name: &str, source: &str) -> PathBuf {
        let path = dir.join(name);
        fs::write(&path, source).expect("write module source");
        path
    }

    #[test]
    fn resolves_sources_local_first_and_returns_dependency_order() {
        let dir = temp_dir("local-first");
        let root = write_module(&dir, "main.ml", "use Helper.{value}\nlet result = value\n");
        let local = write_module(&dir, "helper.ml", "let value = 1\n");
        let include_dir = dir.join("include");
        fs::create_dir_all(&include_dir).expect("create include root");
        write_module(&include_dir, "helper.ml", "let value = 2\n");

        let graph = ModuleGraph::load(&root, &[include_dir]).expect("load module graph");
        assert_eq!(graph.modules.len(), 2);
        assert_eq!(
            graph.modules[graph.root].path,
            fs::canonicalize(root).unwrap()
        );
        let dependency = graph.modules[graph.root].dependencies[0];
        assert_eq!(
            graph.modules[dependency].path,
            fs::canonicalize(local).unwrap()
        );
        assert!(dependency < graph.root, "dependencies are emitted first");
    }

    #[test]
    fn resolves_include_roots_in_command_line_order() {
        let dir = temp_dir("include-order");
        let root = write_module(&dir, "main.ml", "use Helper.{value}\nlet result = value\n");
        let first = dir.join("first");
        let second = dir.join("second");
        fs::create_dir_all(&first).expect("create first include root");
        fs::create_dir_all(&second).expect("create second include root");
        write_module(&first, "helper.ml", "let value = 1\n");
        write_module(&second, "helper.ml", "let value = 2\n");

        let graph = ModuleGraph::load(&root, &[first.clone(), second]).expect("load graph");
        let dependency = graph.modules[graph.root].dependencies[0];
        assert_eq!(
            graph.modules[dependency].path,
            fs::canonicalize(first.join("helper.ml")).unwrap()
        );
    }

    #[test]
    fn reports_missing_modules_and_dependency_cycles() {
        let missing_dir = temp_dir("missing");
        let missing_root = write_module(
            &missing_dir,
            "main.ml",
            "use Absent.{value}\nlet result = value\n",
        );
        assert!(matches!(
            ModuleGraph::load(&missing_root, &[]),
            Err(ModuleError::MissingModule { .. })
        ));

        let cycle_dir = temp_dir("cycle");
        let root = write_module(
            &cycle_dir,
            "main.ml",
            "use Helper.{value}\nlet result = value\n",
        );
        write_module(
            &cycle_dir,
            "helper.ml",
            "use Main.{result}\nlet value = result\n",
        );
        assert!(matches!(
            ModuleGraph::load(&root, &[]),
            Err(ModuleError::DependencyCycle { .. })
        ));
    }

    #[test]
    fn module_paths_follow_snake_case_contract() {
        let ident = Ident::qualified("Compiler", "Parser");
        let name = ModuleName::from_ident(&ident).expect("valid module name");
        assert_eq!(name.source_path(), PathBuf::from("compiler/parser.ml"));
        assert_eq!(name.display(), "Compiler.Parser");
    }

    #[test]
    fn rejects_acronym_runs_in_module_names() {
        let ident = Ident::from_string("HTTPServer");
        let error = ModuleName::from_ident(&ident).expect_err("acronym run is invalid");
        assert!(matches!(error, ModuleError::InvalidName { .. }));
    }
}
