use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::path::PathBuf;
use walkdir::WalkDir;

mod analyzer;
mod codegen;
mod types;

use analyzer::CrateAnalyzer;
use codegen::OCamlGenerator;

#[derive(Parser, Debug)]
#[command(name = "riot-bindgen")]
#[command(about = "Generate OCaml bindings from Rust crates", long_about = None)]
struct Args {
    #[arg(help = "Path to Rust crate root (containing Cargo.toml)")]
    crate_path: PathBuf,
    
    #[arg(short, long, help = "Output directory for generated OCaml files")]
    output: Option<PathBuf>,
    
    #[arg(short, long, help = "Module name prefix for generated bindings")]
    module_name: Option<String>,
    
    #[arg(long, help = "Generate .mli interface files")]
    generate_mli: bool,
    
    #[arg(long, help = "Verbose output")]
    verbose: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    
    if args.verbose {
        println!("riot-bindgen: Analyzing crate at {:?}", args.crate_path);
    }
    
    let src_path = args.crate_path.join("src");
    if !src_path.exists() {
        anyhow::bail!("No src/ directory found at {:?}", src_path);
    }
    
    let mut rust_files = Vec::new();
    for entry in WalkDir::new(&src_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map(|ext| ext == "rs").unwrap_or(false))
    {
        rust_files.push(entry.path().to_path_buf());
    }
    
    if args.verbose {
        println!("Found {} Rust files", rust_files.len());
    }
    
    let mut analyzer = CrateAnalyzer::new();
    
    for file in &rust_files {
        if args.verbose {
            println!("  Analyzing {:?}", file.file_name().unwrap());
        }
        
        let content = fs::read_to_string(file)
            .with_context(|| format!("Failed to read {:?}", file))?;
        
        analyzer.analyze_file(&content)?;
    }
    
    let bindings = analyzer.bindings();
    
    if args.verbose {
        println!("\nFound:");
        println!("  {} public functions", bindings.functions.len());
        println!("  {} public types", bindings.types.len());
        println!("  {} public modules", bindings.modules.len());
    }
    
    let module_name = args
        .module_name
        .unwrap_or_else(|| {
            let crate_name = args
                .crate_path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string();
            sanitize_module_name(&crate_name)
        });
    
    let generator = OCamlGenerator::new(module_name);
    let ocaml_code = generator.generate(bindings)?;
    
    let output_dir = args.output.unwrap_or_else(|| {
        args.crate_path.join("ocaml_bindings")
    });
    
    fs::create_dir_all(&output_dir)
        .context("Failed to create output directory")?;
    
    let output_file = output_dir.join(format!("{}.ml", generator.module_name()));
    fs::write(&output_file, ocaml_code)
        .context("Failed to write OCaml file")?;
    
    if args.generate_mli {
        let mli_code = generator.generate_interface(bindings)?;
        let mli_file = output_dir.join(format!("{}.mli", generator.module_name()));
        fs::write(&mli_file, mli_code)
            .context("Failed to write .mli file")?;
        
        println!("Generated: {:?}", mli_file);
    }
    
    println!("Generated: {:?}", output_file);
    println!("\nTo use in OCaml:");
    println!("  open {}", generator.module_name().to_uppercase());
    
    Ok(())
}

fn sanitize_module_name(name: &str) -> String {
    name.chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}
