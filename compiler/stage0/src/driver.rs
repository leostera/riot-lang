use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr};
use tempfile::TempDir;

use crate::checker::typecheck;
use crate::cli::Cli;
use crate::codegen::emit_object;
use crate::ir::lower_to_rir;
use crate::linker::link_executable;
use crate::parser::parse_source;
use crate::runtime::{build_runtime, find_repo_root};

pub(crate) fn run(cli: Cli) -> miette::Result<()> {
    let source = std::fs::read_to_string(cli.input.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read {}", cli.input))?;

    let ast = parse_source(&cli.input, &source)?;
    let typed = typecheck(&cli.input, &source, ast)?;
    let rir = lower_to_rir(typed);
    let output = cli
        .output
        .unwrap_or_else(|| default_output_path(&cli.input));

    let repo = find_repo_root()?;
    let runtime = build_runtime(&repo)?;
    let temps = TempDir::new()
        .into_diagnostic()
        .wrap_err("failed to create stage0 temp directory")?;
    let object = Utf8PathBuf::from_path_buf(temps.path().join("main.o"))
        .map_err(|path| miette::miette!("temp path is not utf-8: {}", path.display()))?;

    emit_object(&rir, &object, cli.emit_llvm.as_deref())?;
    link_executable(&object, &runtime, &output)?;

    Ok(())
}

fn default_output_path(input: &Utf8Path) -> Utf8PathBuf {
    let stem = input.file_stem().unwrap_or("main");
    Utf8PathBuf::from(".").join(stem)
}
