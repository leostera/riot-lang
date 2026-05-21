mod actor;
mod ast;
mod backend;
mod checker;
mod cli;
mod command;
mod diagnostic;
mod driver;
mod infer;
mod ir;
mod lambda;
mod lexer;
mod linker;
mod parser;
mod runtime;
mod signature;
mod signature_type_parser;
mod stdlib;

pub fn run_cli() -> miette::Result<()> {
    use clap::Parser as _;

    let cli = cli::Cli::parse();
    driver::run(cli)
}
