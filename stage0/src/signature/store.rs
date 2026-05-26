use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr, bail};

use super::{
    Rsig,
    binary::{decode_rsig, encode_rsig},
};

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct RsigStore;

impl RsigStore {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn write(&self, path: &Utf8Path, rsig: &Rsig) -> miette::Result<()> {
        write_rsig(path, rsig)
    }

    pub(crate) fn read(&self, path: &Utf8Path) -> miette::Result<Rsig> {
        read_rsig(path)
    }

    pub(crate) fn resolve(
        &self,
        module: &str,
        source_dir: Option<&Utf8Path>,
        sig_dirs: &[Utf8PathBuf],
    ) -> miette::Result<(Utf8PathBuf, Rsig)> {
        resolve_rsig(module, source_dir, sig_dirs)
    }
}

fn write_rsig(path: &Utf8Path, rsig: &Rsig) -> miette::Result<()> {
    std::fs::write(path.as_std_path(), encode_rsig(rsig))
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to write {}", path))
}

fn read_rsig(path: &Utf8Path) -> miette::Result<Rsig> {
    let bytes = std::fs::read(path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read {}", path))?;
    decode_rsig(&bytes).wrap_err_with(|| format!("failed to decode {}", path))
}

fn resolve_rsig(
    module: &str,
    source_dir: Option<&Utf8Path>,
    sig_dirs: &[Utf8PathBuf],
) -> miette::Result<(Utf8PathBuf, Rsig)> {
    let filename = format!("{module}.rsig");
    let mut candidates = Vec::new();
    if let Some(source_dir) = source_dir {
        candidates.push(source_dir.join(&filename));
    }
    candidates.extend(sig_dirs.iter().map(|dir| dir.join(&filename)));

    for candidate in &candidates {
        if candidate.exists() {
            let rsig = RsigStore::new().read(candidate)?;
            if rsig.module.as_str() != module {
                bail!(
                    "signature {} declares module {}, but `use {}` requested {}",
                    candidate,
                    rsig.module,
                    module,
                    module
                );
            }
            return Ok((candidate.clone(), rsig));
        }
    }

    bail!(
        "missing signature for module {module}\nsearched:\n{}",
        candidates
            .iter()
            .map(|path| format!("  - {path}"))
            .collect::<Vec<_>>()
            .join("\n")
    )
}
