//! TinyML command-line interface.
//!
//! This module keeps the binary entrypoint thin while giving the CLI pipeline a
//! small home of its own.

use std::{
    fs,
    path::{Path, PathBuf},
    process::Command as ProcessCommand,
};

use clap::{Parser as ClapParser, Subcommand, ValueEnum};
use miette::{Diagnostic, IntoDiagnostic, LabeledSpan, NamedSource, Result, SourceSpan, miette};
use thiserror::Error;

use tinyml::{
    backend::{self, native},
    checker, lambda,
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
        /// Native target artifact to emit. Can be passed multiple times.
        #[arg(long = "target", value_enum)]
        targets: Vec<EmitTarget>,
        file: PathBuf,
    },
    /// Run the TinyML language server over stdio.
    Lsp,
    /// Compile and link a TinyML file.
    Compile {
        /// Compilation target triple. Only aarch64-apple-darwin is supported for now.
        #[arg(long, default_value = "aarch64-apple-darwin")]
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
    Nir,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum EmitTarget {
    Aarch64,
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

struct NativePipeline {
    checked: CheckedPipeline,
    lambda: lambda::Lambda,
    ssa: native::ssa::SsaProgram,
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
        Command::Lsp => tinyml::lsp::run_stdio(),
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
            "emit requires at least one --stage <ast|tir|lambda|nir> or --target <aarch64>"
        ));
    }

    let pipeline = native_pipeline_allow_diagnostics(file)?;

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
            EmitStage::Nir => {
                write_artifact(file, "nml", pipeline.ssa.to_string())?;
            }
        }
    }

    for target in targets {
        match target {
            EmitTarget::Aarch64 => {
                let mir = native::aarch64::select(&pipeline.ssa);
                write_artifact(file, "S", mir.to_asm_string())?;
            }
            EmitTarget::Js => {
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
        }
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
            return Err(miette!("include directory does not exist: {}", dir.display()));
        }
    }

    if target == "js" {
        return compile_js(file, output, include_dirs);
    }

    if target != "aarch64-apple-darwin" {
        return Err(miette!(
            "unsupported compile target `{target}`; only aarch64-apple-darwin is supported"
        ));
    }

    let pipeline = native_pipeline(file)?;
    let mir = native::aarch64::select(&pipeline.ssa);
    let output = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| default_compile_output(file));

    let temp_dir = std::env::temp_dir();
    let unique = format!(
        "tinyml-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|duration| duration.as_nanos())
            .unwrap_or(0)
    );
    let asm_path = temp_dir.join(format!("{unique}.S"));
    let runtime_path = temp_dir.join(format!("{unique}_rt.c"));

    fs::write(&asm_path, mir.to_asm_string()).into_diagnostic()?;
    fs::write(&runtime_path, runtime_source()).into_diagnostic()?;

    let status = ProcessCommand::new("cc")
        .arg("-target")
        .arg("aarch64-apple-darwin")
        .arg(&asm_path)
        .arg(&runtime_path)
        .arg("-o")
        .arg(&output)
        .status()
        .into_diagnostic()?;

    let _ = fs::remove_file(&asm_path);
    let _ = fs::remove_file(&runtime_path);

    if !status.success() {
        return Err(miette!("linker failed with status {status}"));
    }

    Ok(())
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

fn checked_pipeline_with_imports(
    file: &Path,
    include_dirs: &[PathBuf],
) -> Result<CheckedPipeline> {
    let parsed = parse_source_file(file)?;
    let imported_values = load_interface_values(include_dirs)?;
    let summary = checker::Checker::new()
        .check_module_with_imported_values(&parsed.ast, &imported_values);
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
    let js = tinyml::backend::js::lower_with_imports(
        &checked.summary.interface,
        &lambda,
        &imports,
    );
    let output = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| default_compile_output(file).with_extension("mjs"));
    fs::write(&output, tinyml::backend::js::emit_js_string(&js)).into_diagnostic()?;
    write_interface_artifact(&output, &checked.summary.interface)?;
    Ok(())
}

fn load_interface_values(include_dirs: &[PathBuf]) -> Result<Vec<String>> {
    let mut values = Vec::new();
    for dir in include_dirs {
        for entry in fs::read_dir(dir).into_diagnostic()? {
            let path = entry.into_diagnostic()?.path();
            if path.extension().and_then(|ext| ext.to_str()) != Some("tmi") {
                continue;
            }
            let text = fs::read_to_string(path).into_diagnostic()?;
            for line in text.lines() {
                if let Some(name) = line.strip_prefix("value ") {
                    values.push(name.to_string());
                }
            }
        }
    }
    values.sort();
    values.dedup();
    Ok(values)
}

fn write_interface_artifact(
    output: &Path,
    interface: &checker::interface::ModuleInterface,
) -> Result<()> {
    let artifact = output.with_extension("tmi");
    let mut contents = String::from("tinyml-interface-v1\n");
    for value in &interface.values {
        if let Some(name) = value.name.as_name() {
            contents.push_str("value ");
            contents.push_str(name);
            contents.push('\n');
        }
    }
    for ty in &interface.types {
        if let Some(name) = ty.name.as_name() {
            contents.push_str("type ");
            contents.push_str(name);
            contents.push('\n');
        }
    }
    fs::write(artifact, contents).into_diagnostic()
}

fn native_pipeline(file: &Path) -> Result<NativePipeline> {
    let checked = checked_pipeline(file)?;
    native_pipeline_from_checked(checked)
}

fn native_pipeline_allow_diagnostics(file: &Path) -> Result<NativePipeline> {
    let parsed = parse_source_file(file)?;
    let summary = checker::check_module(&parsed.ast);
    native_pipeline_from_checked(CheckedPipeline {
        ast: parsed.ast,
        summary,
    })
}

fn native_pipeline_from_checked(checked: CheckedPipeline) -> Result<NativePipeline> {
    let lambda = lambda::translate(&checked.summary.tir);
    let ssa = native::lower(&lambda).optimize();
    Ok(NativePipeline {
        checked,
        lambda,
        ssa,
    })
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

fn runtime_source() -> &'static str {
    r#"
#include <stdint.h>

uintptr_t tinyml_rt_block_new(uintptr_t tag, uintptr_t arity, ...) {
  (void)tag;
  (void)arity;
  return 0;
}

uintptr_t tinyml_rt_block_get(uintptr_t block, uintptr_t index) {
  (void)block;
  (void)index;
  return 0;
}

uintptr_t tinyml_rt_block_tag(uintptr_t block) {
  (void)block;
  return 0;
}

uintptr_t tinyml_rt_string_new(void) { return 0; }
uintptr_t tinyml_rt_float_new(void) { return 0; }
uintptr_t tinyml_rt_match_failure(uintptr_t scrutinee) {
  (void)scrutinee;
  return 0;
}
uintptr_t tinyml_rt_closure_new(void) { return 0; }
uintptr_t tinyml_rt_closure_call(void) { return 0; }

__attribute__((weak)) int main(void) { return 0; }
"#
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
            invocation_root_path(Path::new("fixtures/hello.ml"), "nml").unwrap(),
            std::env::current_dir().unwrap().join("hello.nml")
        );
    }
}
