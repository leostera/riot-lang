use camino::Utf8PathBuf;
use clap::Parser as ClapParser;

#[derive(Debug, ClapParser)]
#[command(name = "stage0")]
#[command(about = "Riot ML stage0 compiler")]
pub(crate) struct Cli {
    /// Riot ML .ml source file to compile.
    pub(crate) input: Utf8PathBuf,

    /// Output executable path. Defaults to ./<input-stem>.
    #[arg(short, long)]
    pub(crate) output: Option<Utf8PathBuf>,

    /// Also write the generated LLVM IR to this path.
    #[arg(long)]
    pub(crate) emit_llvm: Option<Utf8PathBuf>,
}
