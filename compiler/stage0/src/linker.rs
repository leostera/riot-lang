use std::process::Command;

use camino::Utf8Path;
use miette::{IntoDiagnostic, WrapErr};

use crate::command::CommandRunner;

#[derive(Debug, Default)]
pub(crate) struct Linker {
    command_runner: CommandRunner,
}

impl Linker {
    pub(crate) fn new() -> Self {
        Self {
            command_runner: CommandRunner::new(),
        }
    }

    pub(crate) fn link_executable(
        &self,
        object_path: &Utf8Path,
        imported_objects: &[camino::Utf8PathBuf],
        runtime_lib: &Utf8Path,
        output: &Utf8Path,
    ) -> miette::Result<()> {
        let linker = which::which("clang")
            .or_else(|_| which::which("cc"))
            .into_diagnostic()
            .wrap_err("failed to find clang or cc in PATH")?;

        let mut command = Command::new(linker);
        command.arg(object_path.as_std_path());
        for object in imported_objects {
            command.arg(object.as_std_path());
        }
        command
            .arg(runtime_lib.as_std_path())
            .arg("-o")
            .arg(output.as_std_path());

        if cfg!(target_os = "macos") {
            command.arg("-Wl,-dead_strip");
        } else if cfg!(target_os = "linux") {
            command.arg("-Wl,--gc-sections");
            command.args(["-lpthread", "-ldl", "-lm"]);
        }

        self.command_runner
            .run(command, "failed to link executable")
    }
}
