//! TinyML command-line interface.
//!
//! This module keeps the binary entrypoint thin while giving the CLI pipeline a
//! small home of its own.

use std::{
    fs,
    path::{Path, PathBuf},
};

use clap::{Parser as ClapParser, Subcommand, ValueEnum};
use miette::{Diagnostic, IntoDiagnostic, LabeledSpan, NamedSource, Result, SourceSpan, miette};
use thiserror::Error;

use tinyml::{
    backend, checker, lambda,
    parser::{self, lexer::Lexer, parse::Parser},
};

#[derive(Debug, ClapParser)]
#[command(name = "tinyml")]
#[command(about = "TinyML compiler frontend")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Print the token stream.
    Lex { file: PathBuf },
    /// Parse and print the AST.
    Parse { file: PathBuf },
    /// Resolve and typecheck a source module and all of its dependencies.
    Check {
        /// Search path for imported TinyML source modules.
        #[arg(short = 'I', value_name = "DIR")]
        include_dirs: Vec<PathBuf>,
        file: PathBuf,
    },
    /// Emit one or more compiler pipeline artifacts next to the source file.
    Emit {
        /// Pipeline stage to emit. Can be passed multiple times.
        #[arg(long = "stage", value_enum)]
        stages: Vec<EmitStage>,
        /// Output target. Stage 0 currently emits JavaScript.
        #[arg(long = "target", value_enum)]
        targets: Vec<EmitTarget>,
        /// Search path for imported TinyML source modules.
        #[arg(short = 'I', value_name = "DIR")]
        include_dirs: Vec<PathBuf>,
        file: PathBuf,
    },
    /// Compile a TinyML file to the Stage 0 JavaScript target.
    Compile {
        /// Compilation target. Only `js` is supported by Stage 0.
        #[arg(long, default_value = "js")]
        target: String,
        /// Output path. JavaScript defaults to a `.mjs` module.
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Search path for imported TinyML source modules.
        #[arg(short = 'I', value_name = "DIR")]
        include_dirs: Vec<PathBuf>,
        file: PathBuf,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum EmitStage {
    Ast,
    Tir,
    Lambda,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum EmitTarget {
    Js,
}

struct CheckedModule {
    source: tinyml::modules::SourceModule,
    summary: checker::ModuleSummary,
}

struct CheckedPipeline {
    modules: Vec<CheckedModule>,
    root: usize,
}

impl CheckedPipeline {
    fn root(&self) -> &CheckedModule {
        &self.modules[self.root]
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("{diagnostic}")]
struct CliCheckDiagnostic {
    diagnostic: checker::CheckDiagnostic,
    #[source_code]
    src: NamedSource<String>,
    #[label(collection)]
    labels: Vec<LabeledSpan>,
}

pub fn run() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Lex { file } => lex_command(&file),
        Command::Parse { file } => parse_command(&file),
        Command::Check { include_dirs, file } => check_command(&file, &include_dirs),
        Command::Emit {
            stages,
            targets,
            include_dirs,
            file,
        } => emit_command(&file, &stages, &targets, &include_dirs),
        Command::Compile {
            target,
            output,
            include_dirs,
            file,
        } => compile_command(&file, &target, output.as_deref(), &include_dirs),
    }
}

fn lex_command(file: &Path) -> Result<()> {
    let src = fs::read_to_string(file).into_diagnostic()?;
    let lexer = Lexer::new(&src)?;
    for token in lexer.tokens() {
        println!(
            "{:?} @ {}..{}",
            token.kind, token.span.start, token.span.end
        );
    }
    Ok(())
}

fn parse_command(file: &Path) -> Result<()> {
    let ast = parse_file(file)?;
    println!("{ast:#?}");
    Ok(())
}

fn check_command(file: &Path, include_dirs: &[PathBuf]) -> Result<()> {
    let _pipeline = checked_pipeline_with_imports(file, include_dirs)?;
    println!("ok");
    Ok(())
}

fn emit_command(
    file: &Path,
    stages: &[EmitStage],
    targets: &[EmitTarget],
    include_dirs: &[PathBuf],
) -> Result<()> {
    if stages.is_empty() && targets.is_empty() {
        return Err(miette!(
            "emit requires at least one --stage <ast|tir|lambda> or --target js"
        ));
    }

    let pipeline = checked_pipeline_with_imports(file, include_dirs)?;
    let root = pipeline.root();
    let lambda = lambda::translate(&root.summary.tir);

    for stage in stages {
        match stage {
            EmitStage::Ast => {
                write_artifact(file, "ast", format!("{:#?}", root.source.ast))?;
            }
            EmitStage::Tir => {
                write_artifact(file, "tir", format!("{:#?}", root.summary.tir))?;
            }
            EmitStage::Lambda => {
                write_artifact(file, "lambda", format!("{lambda:#?}"))?;
            }
        }
    }

    if targets.contains(&EmitTarget::Js) {
        emit_graph_javascript(
            &pipeline,
            &default_compile_output(file).with_extension("mjs"),
        )?;
    }

    Ok(())
}

fn compile_command(
    file: &Path,
    target: &str,
    output: Option<&Path>,
    include_dirs: &[PathBuf],
) -> Result<()> {
    for dir in include_dirs {
        if !dir.is_dir() {
            return Err(miette!(
                "include directory does not exist: {}",
                dir.display()
            ));
        }
    }

    if target != "js" {
        return Err(miette!(
            "unsupported compile target `{target}`; only `js` is supported"
        ));
    }
    compile_js(file, output, include_dirs)
}

fn parse_file(file: &Path) -> Result<parser::Ast> {
    let source = fs::read_to_string(file).into_diagnostic()?;
    let lexer = Lexer::new(&source)?;
    Ok(Parser::new(lexer).parse_module()?)
}

fn print_check_diagnostics(
    source_name: &str,
    source: &str,
    diagnostics: &[checker::CheckDiagnostic],
) {
    for diagnostic in diagnostics {
        let span = diagnostic.span();
        let report = CliCheckDiagnostic {
            diagnostic: diagnostic.clone(),
            src: NamedSource::new(source_name.to_string(), source.to_string()),
            labels: vec![LabeledSpan::new_with_span(
                Some(diagnostic.label().to_string()),
                SourceSpan::from((span.start, span.end.saturating_sub(span.start))),
            )],
        };
        eprintln!("{:?}", miette::Report::new(report));
    }
}

fn checked_pipeline_with_imports(file: &Path, include_dirs: &[PathBuf]) -> Result<CheckedPipeline> {
    let graph = tinyml::modules::ModuleGraph::load(file, include_dirs)
        .map_err(|error| miette!("module loading failed: {error}"))?;
    let mut checked_modules: Vec<CheckedModule> = Vec::with_capacity(graph.modules.len());

    for source in graph.modules {
        let imported = checker::interface::ModuleInterface {
            modules: source
                .dependencies
                .iter()
                .map(|dependency| {
                    let module = &checked_modules[*dependency];
                    let name = module
                        .source
                        .name
                        .as_ref()
                        .expect("imported module has a resolved name")
                        .to_ident();
                    checker::interface::ModuleDescription {
                        name,
                        interface: Box::new(module.summary.interface.clone()),
                    }
                })
                .collect(),
            ..checker::interface::ModuleInterface::empty()
        };
        let summary = checker::Checker::new().check_module_with_interface(&source.ast, &imported);
        if !summary.diagnostics.is_empty() {
            print_check_diagnostics(
                &source.path.display().to_string(),
                &source.source,
                &summary.diagnostics,
            );
            return Err(miette!("typecheck failed in {}", source.path.display()));
        }
        checked_modules.push(CheckedModule { source, summary });
    }

    Ok(CheckedPipeline {
        modules: checked_modules,
        root: graph.root,
    })
}

fn compile_js(file: &Path, output: Option<&Path>, include_dirs: &[PathBuf]) -> Result<()> {
    let checked = checked_pipeline_with_imports(file, include_dirs)?;
    let output = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| default_compile_output(file).with_extension("mjs"));
    emit_graph_javascript(&checked, &output)
}

fn emit_graph_javascript(pipeline: &CheckedPipeline, root_output: &Path) -> Result<()> {
    let output_dir = root_output.parent().unwrap_or_else(|| Path::new("."));
    let mut artifacts = Vec::with_capacity(pipeline.modules.len());

    for (index, module) in pipeline.modules.iter().enumerate() {
        let output = if index == pipeline.root {
            root_output.to_path_buf()
        } else {
            module_output_path(module, output_dir)?
        };
        let imports = source_imports(&module.source.ast);
        let lambda = lambda::translate(&module.summary.tir);
        let js = backend::js::lower_with_imports(&module.summary.interface, &lambda, &imports);
        let javascript = backend::js::emit_js_string(&js);
        artifacts.push((output, javascript));
    }

    for (output, javascript) in artifacts {
        fs::write(output, javascript).into_diagnostic()?;
    }
    Ok(())
}

fn module_output_path(module: &CheckedModule, output_dir: &Path) -> Result<PathBuf> {
    let name = module.source.name.as_ref().ok_or_else(|| {
        miette!(
            "dependency `{}` has no module name",
            module.source.path.display()
        )
    })?;
    Ok(output_dir.join(format!("{}.mjs", js_module_name(&name.to_ident()))))
}

fn source_imports(module: &parser::Ast) -> Vec<parser::ast::UseDecl> {
    module
        .items
        .iter()
        .filter_map(|item| match item {
            parser::ast::ModuleItem::UseDecl(import) => Some(import.clone()),
            _ => None,
        })
        .collect()
}

fn js_module_name(ident: &parser::ident::Ident) -> String {
    match ident {
        parser::ident::Ident::Name { name, .. } => name.clone(),
        parser::ident::Ident::Qualified { name, rest, .. } => {
            format!("{}_{}", name, js_module_name(rest))
        }
    }
}

fn write_artifact(file: &Path, extension: &str, contents: String) -> Result<PathBuf> {
    let output = invocation_root_path(file, extension)?;
    fs::write(&output, contents).into_diagnostic()?;
    Ok(output)
}

fn default_compile_output(file: &Path) -> PathBuf {
    invocation_root_path(file, "").unwrap_or_else(|_| file.with_extension(""))
}

fn invocation_root_path(file: &Path, extension: &str) -> Result<PathBuf> {
    let stem = file
        .file_stem()
        .ok_or_else(|| miette!("input path `{}` has no file stem", file.display()))?;
    let mut output = std::env::current_dir().into_diagnostic()?.join(stem);
    if !extension.is_empty() {
        output.set_extension(extension);
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_compile_output_uses_invocation_root() {
        assert_eq!(
            default_compile_output(Path::new("fixtures/hello.ml")),
            std::env::current_dir().unwrap().join("hello")
        );
    }

    #[test]
    fn artifact_paths_use_invocation_root() {
        assert_eq!(
            invocation_root_path(Path::new("fixtures/hello.ml"), "lambda").unwrap(),
            std::env::current_dir().unwrap().join("hello.lambda")
        );
    }
}
