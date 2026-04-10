use std::env;
use std::fs;
use std::fs::File;
use std::hint::black_box;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::time::Instant;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Child {
    owner: String,
    score: f64,
    flags: Vec<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Item {
    id: i64,
    name: String,
    active: bool,
    tags: Vec<String>,
    metrics: Vec<i64>,
    child: Child,
    note: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Dataset {
    version: i64,
    source: String,
    items: Vec<Item>,
}

#[derive(Debug, Clone)]
struct Config {
    path: PathBuf,
    iterations: usize,
    reader_capacity: usize,
}

#[derive(Debug, Clone)]
struct Stats {
    name: &'static str,
    iterations: usize,
    min_ms: f64,
    max_ms: f64,
    mean_ms: f64,
    median_ms: f64,
    throughput_mb_s: f64,
}

fn default_fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../packages/serde-json/bench/fixtures/large_payload.json")
}

fn parse_args() -> Result<Config, String> {
    let mut path = default_fixture_path();
    let mut iterations = 20usize;
    let mut reader_capacity = 128 * 1024usize;

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--path" => {
                let value = args
                    .next()
                    .ok_or_else(|| String::from("missing value for --path"))?;
                path = PathBuf::from(value);
            }
            "--iterations" => {
                let value = args
                    .next()
                    .ok_or_else(|| String::from("missing value for --iterations"))?;
                iterations = value
                    .parse::<usize>()
                    .map_err(|_| String::from("invalid integer for --iterations"))?;
            }
            "--reader-capacity" => {
                let value = args
                    .next()
                    .ok_or_else(|| String::from("missing value for --reader-capacity"))?;
                reader_capacity = value
                    .parse::<usize>()
                    .map_err(|_| String::from("invalid integer for --reader-capacity"))?;
            }
            "--help" | "-h" => {
                print_help();
                std::process::exit(0);
            }
            other => {
                return Err(format!("unknown argument: {other}"));
            }
        }
    }

    if iterations == 0 {
        return Err(String::from("--iterations must be > 0"));
    }

    if reader_capacity == 0 {
        return Err(String::from("--reader-capacity must be > 0"));
    }

    Ok(Config {
        path,
        iterations,
        reader_capacity,
    })
}

fn print_help() {
    println!("serde-json-bench");
    println!();
    println!("Options:");
    println!("  --path <file>              JSON fixture path");
    println!("  --iterations <n>          Number of timing iterations (default: 20)");
    println!("  --reader-capacity <bytes> BufReader capacity for from_reader (default: 131072)");
}

fn human_size(bytes: usize) -> String {
    if bytes >= 1_000_000 {
        format!("{}MB", bytes / 1_000_000)
    } else if bytes >= 1_000 {
        format!("{}KB", bytes / 1_000)
    } else {
        format!("{}B", bytes)
    }
}

fn compute_stats(name: &'static str, mut samples_ms: Vec<f64>, bytes: usize) -> Stats {
    samples_ms.sort_by(|left, right| left.partial_cmp(right).unwrap());
    let iterations = samples_ms.len();
    let min_ms = samples_ms[0];
    let max_ms = samples_ms[iterations - 1];
    let mean_ms = samples_ms.iter().sum::<f64>() / iterations as f64;
    let median_ms = if iterations % 2 == 0 {
        let upper = iterations / 2;
        (samples_ms[upper - 1] + samples_ms[upper]) / 2.0
    } else {
        samples_ms[iterations / 2]
    };
    let throughput_mb_s = if mean_ms == 0.0 {
        0.0
    } else {
        (bytes as f64 / 1_000_000.0) / (mean_ms / 1_000.0)
    };
    Stats {
        name,
        iterations,
        min_ms,
        max_ms,
        mean_ms,
        median_ms,
        throughput_mb_s,
    }
}

fn bench_decode_from_str(text: &str, iterations: usize) -> Result<(Stats, Dataset), serde_json::Error> {
    let mut samples_ms = Vec::with_capacity(iterations);
    let mut last = None;
    for _ in 0..iterations {
        let started = Instant::now();
        let dataset: Dataset = serde_json::from_str(text)?;
        let elapsed_ms = started.elapsed().as_secs_f64() * 1_000.0;
        black_box(&dataset);
        samples_ms.push(elapsed_ms);
        last = Some(dataset);
    }
    let dataset = last.expect("iterations > 0 ensured by arg parsing");
    let stats = compute_stats("decode from_str", samples_ms, text.len());
    Ok((stats, dataset))
}

fn bench_decode_from_reader(
    path: &Path,
    iterations: usize,
    reader_capacity: usize,
    bytes: usize,
) -> Result<Stats, Box<dyn std::error::Error>> {
    let mut samples_ms = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        let file = File::open(path)?;
        let reader = BufReader::with_capacity(reader_capacity, file);
        let started = Instant::now();
        let dataset: Dataset = serde_json::from_reader(reader)?;
        let elapsed_ms = started.elapsed().as_secs_f64() * 1_000.0;
        black_box(dataset);
        samples_ms.push(elapsed_ms);
    }
    Ok(compute_stats("decode from_reader", samples_ms, bytes))
}

fn bench_encode_to_string(dataset: &Dataset, iterations: usize) -> Result<Stats, serde_json::Error> {
    let mut samples_ms = Vec::with_capacity(iterations);
    let mut encoded_len = 0usize;
    for _ in 0..iterations {
        let started = Instant::now();
        let encoded = serde_json::to_string(dataset)?;
        let elapsed_ms = started.elapsed().as_secs_f64() * 1_000.0;
        encoded_len = encoded.len();
        black_box(encoded);
        samples_ms.push(elapsed_ms);
    }
    Ok(compute_stats("encode to_string", samples_ms, encoded_len))
}

fn print_stats(stats: &Stats) {
    println!(
        "{:<20} mean {:>8.2}ms  median {:>8.2}ms  min {:>8.2}ms  max {:>8.2}ms  {:>8.2} MB/s  ({:>2} iters)",
        stats.name,
        stats.mean_ms,
        stats.median_ms,
        stats.min_ms,
        stats.max_ms,
        stats.throughput_mb_s,
        stats.iterations
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = match parse_args() {
        Ok(config) => config,
        Err(message) => {
            eprintln!("{message}");
            eprintln!();
            print_help();
            std::process::exit(2);
        }
    };

    let path = config.path.canonicalize().unwrap_or(config.path.clone());
    let text = fs::read_to_string(&path)?;
    let bytes = text.len();

    println!("fixture: {}", path.display());
    println!("size:    {}", human_size(bytes));
    println!("iters:   {}", config.iterations);
    println!("reader:  {} bytes", config.reader_capacity);
    println!();

    let (decode_from_str, dataset) = bench_decode_from_str(&text, config.iterations)?;
    let decode_from_reader =
        bench_decode_from_reader(&path, config.iterations, config.reader_capacity, bytes)?;
    let encode_to_string = bench_encode_to_string(&dataset, config.iterations)?;

    print_stats(&decode_from_str);
    print_stats(&decode_from_reader);
    print_stats(&encode_to_string);

    Ok(())
}
