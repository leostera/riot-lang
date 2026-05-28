mod parser;

use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, Result};

use parser::{lexer, parse};

#[derive(Debug, Parser)]
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
    /// Typecheck a file. Not implemented yet.
    Check { file: PathBuf },
    /// Build a file. Not implemented yet.
    Build { file: PathBuf },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Lex { file } => {
            let src = fs::read_to_string(&file).into_diagnostic()?;
            let tokens = lexer::lex(&src)?;
            for token in tokens {
                println!(
                    "{:?} @ {}..{}",
                    token.kind, token.span.start, token.span.end
                );
            }
        }
        Command::Parse { file } => {
            let src = fs::read_to_string(&file).into_diagnostic()?;
            let tokens = lexer::lex(&src)?;
            let module = parse::parse_module(&src, tokens)?;
            println!("{module:#?}");
        }
        Command::Check { file } => {
            println!("check not implemented yet: {}", file.display());
        }
        Command::Build { file } => {
            println!("build not implemented yet: {}", file.display());
        }
    }

    Ok(())
}
