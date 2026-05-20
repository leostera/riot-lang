mod ast;
mod checker;
mod cli;
mod codegen;
mod command;
mod diagnostic;
mod driver;
mod infer;
mod ir;
mod lexer;
mod linker;
mod parser;
mod runtime;
mod signature;

pub fn run_cli() -> miette::Result<()> {
    use clap::Parser as _;

    let cli = cli::Cli::parse();
    driver::run(cli)
}
