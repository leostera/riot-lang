mod ast;
mod checker;
mod cli;
mod codegen;
mod command;
mod diagnostic;
mod driver;
mod ir;
mod lexer;
mod linker;
mod parser;
mod runtime;

use clap::Parser as _;

fn main() -> miette::Result<()> {
    let cli = cli::Cli::parse();
    driver::run(cli)
}
