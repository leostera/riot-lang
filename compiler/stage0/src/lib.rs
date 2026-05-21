mod actor;
mod ast;
mod backend;
mod checker;
mod cli;
mod command;
mod diagnostic;
mod driver;
mod fingerprint;
mod infer;
mod lambda;
mod lexer;
mod linker;
mod parser;
mod runtime;
mod signature;
#[cfg(test)]
mod signature_type_parser;
mod stdlib;
mod type_lowerer;

pub fn run_cli() -> miette::Result<()> {
    use clap::Parser as _;

    let cli = cli::Cli::parse();
    driver::Stage0Driver::new().run(cli)
}
