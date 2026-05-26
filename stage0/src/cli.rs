use camino::Utf8PathBuf;
use clap::{Parser as ClapParser, Subcommand, ValueEnum};

use crate::signature::ModuleName;

#[derive(Debug, ClapParser)]
#[command(name = "stage0")]
#[command(about = "Riot ML stage0 compiler")]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) command: Command,
}

#[derive(Debug, Subcommand)]
pub(crate) enum Command {
    /// Compile one or more Riot ML source files to a native executable.
    Compile(CompileArgs),
    /// Compile one or more Riot ML source files to module .rsig and .o artifacts.
    CompileLib(CompileLibArgs),
    /// Emit one compiler pipeline artifact.
    Emit(EmitArgs),
    /// Compare two binary .rsig interface artifacts or two directories of artifacts for review.
    InterfaceDiff(InterfaceDiffArgs),
}

#[derive(Debug, ClapParser)]
pub(crate) struct CompileArgs {
    /// Riot ML .ml source file(s) to compile.
    #[arg(required = true)]
    pub(crate) inputs: Vec<Utf8PathBuf>,

    /// Output executable path.
    #[arg(short, long)]
    pub(crate) output: Utf8PathBuf,

    /// Directory to search for imported .rsig files.
    #[arg(long = "sig-dir")]
    pub(crate) sig_dirs: Vec<Utf8PathBuf>,

    /// Directory to search for imported module object files.
    #[arg(long = "object-dir")]
    pub(crate) object_dirs: Vec<Utf8PathBuf>,

    /// Imported module object mapping, for example Math=build/Math.o.
    #[arg(long = "object", value_parser = parse_object_mapping)]
    pub(crate) objects: Vec<ObjectMapping>,

    /// Also write the generated LLVM IR to this path.
    #[arg(long)]
    pub(crate) emit_llvm: Option<Utf8PathBuf>,
}

#[derive(Debug, ClapParser)]
pub(crate) struct CompileLibArgs {
    /// Riot ML .ml source file(s) to compile as module artifacts.
    #[arg(required = true)]
    pub(crate) inputs: Vec<Utf8PathBuf>,

    /// Directory that receives <Module>.rsig and <Module>.o.
    #[arg(long = "out-dir")]
    pub(crate) out_dir: Utf8PathBuf,

    /// Directory to search for imported .rsig files.
    #[arg(long = "sig-dir")]
    pub(crate) sig_dirs: Vec<Utf8PathBuf>,

    /// Also write the generated LLVM IR to this path.
    #[arg(long)]
    pub(crate) emit_llvm: Option<Utf8PathBuf>,
}

#[derive(Debug, ClapParser)]
pub(crate) struct InterfaceDiffArgs {
    /// Previous binary .rsig interface artifact, or a directory of .rsig artifacts.
    pub(crate) before: Utf8PathBuf,

    /// Current binary .rsig interface artifact, or a directory of .rsig artifacts.
    pub(crate) after: Utf8PathBuf,

    /// Output path. Defaults to stdout.
    #[arg(short, long)]
    pub(crate) output: Option<Utf8PathBuf>,
}

#[derive(Debug, ClapParser)]
pub(crate) struct EmitArgs {
    /// Compiler pass to emit.
    #[arg(value_enum)]
    pub(crate) pass: EmitPass,

    /// Riot ML .ml source file to inspect, or a binary .rsig file for `emit interface`.
    pub(crate) input: Utf8PathBuf,

    /// Output path. Defaults to stdout for text passes and interface text,
    /// <input>.rsig for rsig, and <input>.o for object.
    #[arg(short, long)]
    pub(crate) output: Option<Utf8PathBuf>,

    /// Directory to search for imported .rsig files.
    #[arg(long = "sig-dir")]
    pub(crate) sig_dirs: Vec<Utf8PathBuf>,
}

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq, Eq)]
pub(crate) enum EmitPass {
    Cst,
    Typed,
    Rsig,
    Interface,
    Ir,
    ActorIr,
    Llvm,
    Assembly,
    Object,
    All,
}

#[derive(Debug, Clone)]
pub(crate) struct ObjectMapping {
    pub(crate) module: ModuleName,
    pub(crate) path: Utf8PathBuf,
}

fn parse_object_mapping(value: &str) -> Result<ObjectMapping, String> {
    let Some((module, path)) = value.split_once('=') else {
        return Err("expected NAME=PATH".to_owned());
    };
    if module.is_empty() {
        return Err("object mapping module name cannot be empty".to_owned());
    }
    Ok(ObjectMapping {
        module: ModuleName::new(module),
        path: Utf8PathBuf::from(path),
    })
}

#[cfg(test)]
mod tests {
    use super::parse_object_mapping;

    #[test]
    fn parses_object_mapping_module_name_as_typed_identity() {
        let mapping = parse_object_mapping("Palette=/tmp/Palette.o").unwrap();

        assert_eq!(mapping.module.as_str(), "Palette");
    }
}
