//! TinyML command-line entrypoint.

mod cli;

fn main() -> miette::Result<()> {
    cli::run()
}
