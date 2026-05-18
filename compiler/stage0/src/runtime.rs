use std::process::Command;
use std::time::SystemTime;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr, bail};

use crate::command::run_command;

pub(crate) fn find_repo_root() -> miette::Result<Utf8PathBuf> {
    let current_dir = std::env::current_dir()
        .into_diagnostic()
        .wrap_err("failed to read current directory")?;
    let mut dir = Utf8PathBuf::from_path_buf(current_dir)
        .map_err(|path| miette::miette!("current directory is not utf-8: {}", path.display()))?;

    loop {
        if dir.join("compiler/rt/Cargo.toml").exists() {
            return Ok(dir);
        }

        if !dir.pop() {
            bail!("could not find repo root containing compiler/rt/Cargo.toml");
        }
    }
}

pub(crate) fn build_runtime(repo: &Utf8Path) -> miette::Result<Utf8PathBuf> {
    let lib = repo.join("compiler/rt/target/release/libriot_rt.a");
    if runtime_library_is_current(repo, &lib)? {
        return Ok(lib);
    }

    let cargo = which::which("cargo")
        .into_diagnostic()
        .wrap_err("failed to find cargo in PATH")?;
    let manifest = repo.join("compiler/rt/Cargo.toml");

    let mut command = Command::new(cargo);
    command.args(["build", "--release", "--manifest-path", manifest.as_str()]);
    run_command(command, "failed to build Riot runtime")?;

    if !lib.exists() {
        bail!("runtime build did not produce {}", lib);
    }

    Ok(lib)
}

fn runtime_library_is_current(repo: &Utf8Path, lib: &Utf8Path) -> miette::Result<bool> {
    let lib_modified = match modified_time(lib)? {
        Some(time) => time,
        None => return Ok(false),
    };

    for path in [
        repo.join("compiler/rt/Cargo.toml"),
        repo.join("compiler/rt/Cargo.lock"),
    ] {
        if let Some(time) = modified_time(&path)? {
            if time > lib_modified {
                return Ok(false);
            }
        }
    }

    Ok(!directory_has_file_newer_than(
        &repo.join("compiler/rt/src"),
        lib_modified,
    )?)
}

fn directory_has_file_newer_than(dir: &Utf8Path, cutoff: SystemTime) -> miette::Result<bool> {
    for entry in std::fs::read_dir(dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read {}", dir))?
    {
        let entry = entry
            .into_diagnostic()
            .wrap_err_with(|| format!("failed to read entry in {}", dir))?;
        let path = Utf8PathBuf::from_path_buf(entry.path()).map_err(|path| {
            miette::miette!("runtime source path is not utf-8: {}", path.display())
        })?;
        let file_type = entry
            .file_type()
            .into_diagnostic()
            .wrap_err_with(|| format!("failed to read file type for {}", path))?;

        if file_type.is_dir() {
            if directory_has_file_newer_than(&path, cutoff)? {
                return Ok(true);
            }
        } else if file_type.is_file()
            && let Some(time) = modified_time(&path)?
            && time > cutoff
        {
            return Ok(true);
        }
    }

    Ok(false)
}

fn modified_time(path: &Utf8Path) -> miette::Result<Option<SystemTime>> {
    match std::fs::metadata(path.as_std_path()) {
        Ok(metadata) => metadata
            .modified()
            .map(Some)
            .into_diagnostic()
            .wrap_err_with(|| format!("failed to read modified time for {}", path)),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(error)
            .into_diagnostic()
            .wrap_err_with(|| format!("failed to stat {}", path)),
    }
}
