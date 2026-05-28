use assert_cmd::Command;

fn tinyml(args: &[&str]) -> String {
    let output = Command::cargo_bin("tinyml")
        .expect("tinyml binary")
        .args(args)
        .output()
        .expect("command output");

    assert!(
        output.status.success(),
        "command failed\nstatus: {}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
    String::from_utf8(output.stdout).expect("utf8 stdout")
}

#[test]
fn lex_basic() {
    let stdout = tinyml(&["lex", "tests/fixtures/basic.ml"]);
    insta::assert_snapshot!(stdout);
}

#[test]
fn parse_basic() {
    let stdout = tinyml(&["parse", "tests/fixtures/basic.ml"]);
    insta::assert_snapshot!(stdout);
}

#[test]
fn lex_match() {
    let stdout = tinyml(&["lex", "tests/fixtures/match.ml"]);
    insta::assert_snapshot!(stdout);
}

#[test]
fn parse_match() {
    let stdout = tinyml(&["parse", "tests/fixtures/match.ml"]);
    insta::assert_snapshot!(stdout);
}

#[test]
fn parse_use_qualified() {
    let stdout = tinyml(&["parse", "tests/fixtures/use_qualified.ml"]);
    insta::assert_snapshot!(stdout);
}

#[test]
fn parse_records() {
    let stdout = tinyml(&["parse", "tests/fixtures/records.ml"]);
    insta::assert_snapshot!(stdout);
}

#[test]
fn lex_unit() {
    let stdout = tinyml(&["lex", "tests/fixtures/unit.ml"]);
    insta::assert_snapshot!(stdout);
}

#[test]
fn parse_unit() {
    let stdout = tinyml(&["parse", "tests/fixtures/unit.ml"]);
    insta::assert_snapshot!(stdout);
}

#[test]
fn parse_variant_record() {
    let stdout = tinyml(&["parse", "tests/fixtures/variant_record.ml"]);
    insta::assert_snapshot!(stdout);
}
