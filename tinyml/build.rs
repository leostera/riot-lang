use std::{env, fs, path::Path};

fn main() {
    println!("cargo:rerun-if-changed=tests/fixtures");

    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR");
    let fixtures_dir = Path::new(&manifest_dir).join("tests/fixtures");
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR");
    let out_file = Path::new(&out_dir).join("corpus_tests_generated.rs");

    let mut fixtures = fs::read_dir(&fixtures_dir)
        .expect("read tests/fixtures")
        .map(|entry| entry.expect("fixture entry").path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "ml"))
        .collect::<Vec<_>>();

    fixtures.sort();

    let mut generated = String::from("corpus_tests! {\n");
    for path in fixtures {
        let file_name = path
            .file_stem()
            .expect("fixture file stem")
            .to_string_lossy();
        let ident = file_name
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
            .collect::<String>();
        let ident = format!("c{ident}");
        let rel_path = format!(
            "tests/fixtures/{}",
            path.file_name().unwrap().to_string_lossy()
        );
        generated.push_str(&format!("    {ident} => {rel_path:?},\n"));
    }
    generated.push_str("}\n");

    fs::write(out_file, generated).expect("write generated corpus tests");
}
