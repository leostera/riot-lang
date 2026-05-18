use std::process::Command;

use miette::{IntoDiagnostic, WrapErr, bail};

pub(crate) fn run_command(mut command: Command, context: &'static str) -> miette::Result<()> {
    let output = command
        .output()
        .into_diagnostic()
        .wrap_err_with(|| format!("{context}: could not start command"))?;

    if output.status.success() {
        return Ok(());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    bail!(
        "{context}\nstatus: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        stdout.trim(),
        stderr.trim()
    )
}
