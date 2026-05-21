mod ast;
mod actor;
mod backend;
mod checker;
mod cli;
mod command;
mod diagnostic;
mod driver;
mod infer;
mod lambda;
mod ir;
mod lexer;
mod linker;
mod parser;
mod runtime;
mod signature;
mod stdlib;

pub fn run_cli() -> miette::Result<()> {
    use clap::Parser as _;

    let cli = cli::Cli::parse();
    driver::run(cli)
}
