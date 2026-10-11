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
    /// Parse and typecheck a file.
    Check { file: PathBuf },
    /// Emit one or more compiler pipeline artifacts next to the source file.
    Emit {
        /// Pipeline stage to emit. Can be passed multiple times.
        #[arg(long = "stage", value_enum)]
        stages: Vec<EmitStage>,
        /// Output target. Stage 0 currently emits JavaScript.
        #[arg(long = "target", value_enum)]
        targets: Vec<EmitTarget>,
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
        /// Search path for compiled module interfaces.
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

struct CheckedPipeline {
    ast: parser::Ast,
    summary: checker::ModuleSummary,
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

struct EmitPipeline {
    checked: CheckedPipeline,
    lambda: lambda::Lambda,
}

pub fn run() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Lex { file } => lex_command(&file),
        Command::Parse { file } => parse_command(&file),
        Command::Check { file } => check_command(&file),
        Command::Emit {
            stages,
            targets,
            file,
        } => emit_command(&file, &stages, &targets),
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

fn check_command(file: &Path) -> Result<()> {
    let _pipeline = checked_pipeline(file)?;
    println!("ok");
    Ok(())
}

fn emit_command(file: &Path, stages: &[EmitStage], targets: &[EmitTarget]) -> Result<()> {
    if stages.is_empty() && targets.is_empty() {
        return Err(miette!(
            "emit requires at least one --stage <ast|tir|lambda> or --target js"
        ));
    }

    let pipeline = emit_pipeline(file)?;

    for stage in stages {
        match stage {
            EmitStage::Ast => {
                write_artifact(file, "ast", format!("{:#?}", pipeline.checked.ast))?;
            }
            EmitStage::Tir => {
                write_artifact(file, "tir", format!("{:#?}", pipeline.checked.summary.tir))?;
            }
            EmitStage::Lambda => {
                write_artifact(file, "lambda", format!("{:#?}", pipeline.lambda))?;
            }
        }
    }

    if !targets.is_empty() {
        let imports = pipeline
            .checked
            .ast
            .items
            .iter()
            .filter_map(|item| match item {
                parser::ast::ModuleItem::UseDecl(import) => Some(import.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        let js = backend::js::lower_with_imports(
            &pipeline.checked.summary.interface,
            &pipeline.lambda,
            &imports,
        );
        write_artifact(file, "mjs", backend::js::emit_js_string(&js))?;
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

struct ParsedSource {
    source_name: String,
    source: String,
    ast: parser::Ast,
}

fn parse_file(file: &Path) -> Result<parser::Ast> {
    Ok(parse_source_file(file)?.ast)
}

fn parse_source_file(file: &Path) -> Result<ParsedSource> {
    let source = fs::read_to_string(file).into_diagnostic()?;
    let lexer = Lexer::new(&source)?;
    let ast = Parser::new(lexer).parse_module()?;
    Ok(ParsedSource {
        source_name: file.display().to_string(),
        source,
        ast,
    })
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

fn checked_pipeline(file: &Path) -> Result<CheckedPipeline> {
    checked_pipeline_with_imports(file, &[])
}

fn checked_pipeline_with_imports(file: &Path, include_dirs: &[PathBuf]) -> Result<CheckedPipeline> {
    let parsed = parse_source_file(file)?;
    let imported = load_interfaces(&parsed.ast, include_dirs)?;
    let summary = checker::Checker::new().check_module_with_interface(&parsed.ast, &imported);
    if !summary.diagnostics.is_empty() {
        print_check_diagnostics(&parsed.source_name, &parsed.source, &summary.diagnostics);
        return Err(miette!("typecheck failed"));
    }
    Ok(CheckedPipeline {
        ast: parsed.ast,
        summary,
    })
}

fn compile_js(file: &Path, output: Option<&Path>, include_dirs: &[PathBuf]) -> Result<()> {
    let checked = checked_pipeline_with_imports(file, include_dirs)?;
    let imports = checked
        .ast
        .items
        .iter()
        .filter_map(|item| match item {
            parser::ast::ModuleItem::UseDecl(import) => Some(import.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();
    let lambda = lambda::translate(&checked.summary.tir);
    let js = tinyml::backend::js::lower_with_imports(&checked.summary.interface, &lambda, &imports);
    let output = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| default_compile_output(file).with_extension("mjs"));
    fs::write(&output, tinyml::backend::js::emit_js_string(&js)).into_diagnostic()?;
    let import_names = imports
        .iter()
        .map(|import| match import {
            parser::ast::UseDecl::Concrete { module, .. }
            | parser::ast::UseDecl::Wildcard { module, .. } => js_module_name(module),
        })
        .collect::<Vec<_>>();
    write_interface_artifact(&output, &checked.summary.interface, &import_names)?;
    Ok(())
}

fn load_interfaces(
    module: &parser::ast::Module,
    include_dirs: &[PathBuf],
) -> Result<checker::interface::ModuleInterface> {
    let modules = module
        .items
        .iter()
        .filter_map(|item| match item {
            parser::ast::ModuleItem::UseDecl(parser::ast::UseDecl::Concrete { module, .. })
            | parser::ast::ModuleItem::UseDecl(parser::ast::UseDecl::Wildcard { module, .. }) => {
                Some(js_module_name(module))
            }
            _ => None,
        })
        .collect::<std::collections::HashSet<_>>();
    let mut imported = checker::interface::ModuleInterface::empty();
    for dir in include_dirs {
        for entry in fs::read_dir(dir).into_diagnostic()? {
            let path = entry.into_diagnostic()?.path();
            if path.extension().and_then(|ext| ext.to_str()) != Some("tmi")
                || !modules.contains(
                    &path
                        .file_stem()
                        .and_then(|stem| stem.to_str())
                        .unwrap_or_default()
                        .to_string(),
                )
            {
                continue;
            }
            let bytes = fs::read(path).into_diagnostic()?;
            let interface = checker::interface::decode_interface(&bytes)
                .map_err(|error| miette!("malformed interface artifact: {error}"))?;
            imported.values.extend(interface.values);
            imported.types.extend(interface.types);
            imported.constructors.extend(interface.constructors);
            imported.record_fields.extend(interface.record_fields);
        }
    }
    Ok(imported)
}

fn js_module_name(ident: &parser::ident::Ident) -> String {
    match ident {
        parser::ident::Ident::Name { name, .. } => name.clone(),
        parser::ident::Ident::Qualified { name, rest, .. } => {
            format!("{}_{}", name, js_module_name(rest))
        }
    }
}

fn write_interface_artifact(
    output: &Path,
    interface: &checker::interface::ModuleInterface,
    imports: &[String],
) -> Result<()> {
    let artifact = output.with_extension("tmi");
    let contents = checker::interface::encode_interface_with_imports(interface, imports)
        .map_err(|error| miette!("could not encode interface: {error}"))?;
    fs::write(artifact, contents).into_diagnostic()
}

fn emit_pipeline(file: &Path) -> Result<EmitPipeline> {
    let parsed = parse_source_file(file)?;
    let summary = checker::check_module(&parsed.ast);
    let checked = CheckedPipeline {
        ast: parsed.ast,
        summary,
    };
    let lambda = lambda::translate(&checked.summary.tir);
    Ok(EmitPipeline { checked, lambda })
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
