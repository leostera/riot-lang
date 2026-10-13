use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};

fn tinyml() -> &'static str {
    env!("CARGO_BIN_EXE_tinyml")
}

fn temp_dir(name: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "tinyml-cli-{name}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&dir).unwrap();
    dir
}

fn write_valid_source(dir: &Path) -> std::path::PathBuf {
    let dst = dir.join("hello.ml");
    fs::write(&dst, "let x = 1\n").unwrap();
    dst
}

fn riotml_sources(directory: &Path) -> Vec<PathBuf> {
    let mut sources = Vec::new();
    for entry in fs::read_dir(directory).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            sources.extend(riotml_sources(&path));
        } else if path.extension().is_some_and(|extension| extension == "ml") {
            sources.push(path);
        }
    }
    sources.sort();
    sources
}

#[test]
fn check_prints_ok() {
    let dir = temp_dir("check");
    let file = write_valid_source(&dir);

    let output = Command::new(tinyml())
        .arg("check")
        .arg(&file)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "ok\n");
}

#[test]
fn riotc_native_ir_verifier_fixtures_execute() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let include_dir = repo.join("riotc");
    let fixture = include_dir.join("native_ir/verify_test.ml");
    let dir = temp_dir("riotc-native-ir-verify");
    let output_module = dir.join("verify.mjs");

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_dir)
        .arg(&fixture)
        .arg("-o")
        .arg(&output_module)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ main }} from {:?}; const status = main(); if (status !== 0 && status !== 0n) process.exit(1)",
            output_module.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "fixture stderr: {}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn riotc_fixed_width_integer_literal_ranges_execute() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let include_dir = repo.join("riotc");
    let fixture = include_dir.join("check/integer_literals_test.ml");
    let dir = temp_dir("riotc-integer-literal-ranges");
    let output_module = dir.join("integer-literals.mjs");

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_dir)
        .arg("-o")
        .arg(&output_module)
        .arg(&fixture)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "Stage 0 compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ main }} from {:?}; const status = main(); if (status !== 0 && status !== 0n) process.exit(1)",
            output_module.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "integer literal range fixture stderr: {}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn aarch64_machine_ir_cli_selects_supported_functions_and_typed_division() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let include_dir = repo.join("riotc");
    let compiler_source = include_dir.join("main.ml");
    let dir = temp_dir("aarch64-machine-ir-cli");
    let compiler_js = dir.join("riotc.mjs");
    let stage0 = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_dir)
        .arg("-o")
        .arg(&compiler_js)
        .arg(&compiler_source)
        .output()
        .unwrap();
    assert!(
        stage0.status.success(),
        "Stage 0 compiler stderr: {}",
        String::from_utf8_lossy(&stage0.stderr)
    );

    let runner = dir.join("run-compiler.mjs");
    fs::write(
        &runner,
        "import { main } from \"./riotc.mjs\"; process.exitCode = Number(main(undefined));\n",
    )
    .unwrap();
    let supported_source = dir.join("supported.ml");
    fs::write(
        &supported_source,
        "let add = fn x -> x + 1\n\
         let callee = fn x -> x + 2\n\
         let caller = fn x -> callee x\n\
         let is_ten = fn x -> match x { | 10 -> true | _ -> false }\n\
         let is_true = fn x -> match x { | true -> 1 | false -> 0 }\n\
         let main = fn () -> 42\n",
    )
    .unwrap();
    let selected = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-mir")
        .arg(&supported_source)
        .arg("add")
        .output()
        .unwrap();
    assert!(
        selected.status.success(),
        "selection stderr: {}",
        String::from_utf8_lossy(&selected.stderr)
    );
    let printed = String::from_utf8_lossy(&selected.stdout);
    assert!(
        printed.contains("add_w %v0, %v1"),
        "unexpected MachineIR:\n{printed}"
    );

    let integer_match = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-mir")
        .arg(&supported_source)
        .arg("is_ten")
        .output()
        .unwrap();
    assert!(
        integer_match.status.success(),
        "integer literal match selection stderr: {}",
        String::from_utf8_lossy(&integer_match.stderr)
    );
    let integer_machine_ir = String::from_utf8_lossy(&integer_match.stdout);
    assert!(
        integer_machine_ir.contains("cmp_set.eq_w %v0, #10"),
        "unexpected integer-match MachineIR:\n{integer_machine_ir}"
    );

    let boolean_match = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-mir")
        .arg(&supported_source)
        .arg("is_true")
        .output()
        .unwrap();
    assert!(
        boolean_match.status.success(),
        "boolean literal match selection stderr: {}",
        String::from_utf8_lossy(&boolean_match.stderr)
    );
    let boolean_machine_ir = String::from_utf8_lossy(&boolean_match.stdout);
    assert!(
        boolean_machine_ir.contains("cmp_set.eq_bool %v0, true"),
        "unexpected boolean-match MachineIR:\n{boolean_machine_ir}"
    );

    let imported_helper = dir.join("Helper.ml");
    fs::write(&imported_helper, "let value = 41\n").unwrap();
    let imported_global_source = dir.join("imported-global.ml");
    fs::write(
        &imported_global_source,
        "use Helper.{value}\nlet main = fn () -> value\n",
    )
    .unwrap();
    let imported_global = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-mir")
        .arg(&imported_global_source)
        .arg("main")
        .output()
        .unwrap();
    assert!(!imported_global.status.success());
    let imported_global_diagnostic = String::from_utf8_lossy(&imported_global.stderr);
    assert!(imported_global_diagnostic.contains("AArch64 selection failed"));
    assert!(
        !imported_global_diagnostic.contains("NativeIR verification failed"),
        "dependency global declarations were absent from root NativeIR verification: {imported_global_diagnostic}"
    );

    let unsupported_source = dir.join("unsupported.ml");
    fs::write(
        &unsupported_source,
        "let divide = fn x -> x / 2\nlet main = fn () -> 42\n",
    )
    .unwrap();
    let selected_division = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-mir")
        .arg(&unsupported_source)
        .arg("divide")
        .output()
        .unwrap();
    assert!(
        selected_division.status.success(),
        "typed i32 division was not selected: {}",
        String::from_utf8_lossy(&selected_division.stderr)
    );
    let division_machine_ir = String::from_utf8_lossy(&selected_division.stdout);
    assert!(division_machine_ir.contains("call runtime rt_native_i32_div"));
    assert!(division_machine_ir.contains("trap:division_by_zero"));

    let unsupported_value_source = dir.join("unsupported-value.ml");
    fs::write(
        &unsupported_value_source,
        "let concatenate = fn x -> x + \"!\"\nlet main = fn () -> 42\n",
    )
    .unwrap();
    let unsupported_value = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-mir")
        .arg(&unsupported_value_source)
        .arg("concatenate")
        .output()
        .unwrap();
    assert!(
        !unsupported_value.status.success(),
        "string representation unexpectedly selected"
    );
    assert!(
        unsupported_value.stdout.is_empty(),
        "unsupported selection unexpectedly emitted MachineIR"
    );
    let unsupported_value_diagnostic = String::from_utf8_lossy(&unsupported_value.stderr);
    assert!(
        unsupported_value_diagnostic.contains("string constants are outside the scalar selector"),
        "unexpected unsupported-value diagnostic: {unsupported_value_diagnostic}"
    );

    let pointer_source = dir.join("pointer-identity.ml");
    fs::write(
        &pointer_source,
        "let identity = fn (text: string) -> text\nlet caller = fn (text: string) -> identity text\nlet main = fn () -> 42\n",
    )
    .unwrap();
    let pointer_linear_ir = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-linear-ir")
        .arg(&pointer_source)
        .arg("identity")
        .output()
        .unwrap();
    assert!(
        pointer_linear_ir.status.success(),
        "opaque pointer parameter/return lowering failed: {}",
        String::from_utf8_lossy(&pointer_linear_ir.stderr)
    );
    let pointer_ir_text = String::from_utf8_lossy(&pointer_linear_ir.stdout);
    assert!(
        pointer_ir_text.contains("ptr"),
        "missing pointer type:\n{pointer_ir_text}"
    );
    assert!(
        pointer_ir_text.contains("x1"),
        "missing X-register argument:\n{pointer_ir_text}"
    );
    assert!(
        pointer_ir_text.contains("x0"),
        "missing X-register return:\n{pointer_ir_text}"
    );
    let pointer_caller = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-linear-ir")
        .arg(&pointer_source)
        .arg("caller")
        .output()
        .unwrap();
    assert!(
        pointer_caller.status.success(),
        "typed pointer direct-call lowering failed: {}",
        String::from_utf8_lossy(&pointer_caller.stderr)
    );
    let pointer_caller_text = String::from_utf8_lossy(&pointer_caller.stdout);
    assert!(pointer_caller_text.contains("bl fn "));
    assert!(pointer_caller_text.contains("x1"));

    let generic_source = dir.join("generic.ml");
    fs::write(
        &generic_source,
        "let generic = fn value -> value\nlet main = fn () -> 42\n",
    )
    .unwrap();
    let generic_selection = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-mir")
        .arg(&generic_source)
        .arg("generic")
        .output()
        .unwrap();
    assert!(!generic_selection.status.success());
    assert!(generic_selection.stdout.is_empty());
    let generic_diagnostic = String::from_utf8_lossy(&generic_selection.stderr);
    assert!(generic_diagnostic.contains("unresolved generic type has no native specialization"));

    let unit_entry = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-mir")
        .arg(&supported_source)
        .arg("main")
        .output()
        .unwrap();
    assert!(
        unit_entry.status.success(),
        "erased unit entry parameter failed selection: {}",
        String::from_utf8_lossy(&unit_entry.stderr)
    );
    assert!(
        String::from_utf8_lossy(&unit_entry.stdout).contains("unit"),
        "MachineIR omitted the typed erased unit entry parameter"
    );

    let linear_main = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-linear-ir")
        .arg(&supported_source)
        .arg("main")
        .output()
        .unwrap();
    assert!(
        linear_main.status.success(),
        "unit-entry LinearIR lowering failed: {}",
        String::from_utf8_lossy(&linear_main.stderr)
    );
    let linear_main_text = String::from_utf8_lossy(&linear_main.stdout);
    assert!(linear_main_text.contains("%v0:unit = erased"));
    assert!(linear_main_text.contains("mov_imm w9, #42"));

    let linear_caller = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-linear-ir")
        .arg(&supported_source)
        .arg("caller")
        .output()
        .unwrap();
    assert!(
        linear_caller.status.success(),
        "same-module direct-call lowering failed: {}",
        String::from_utf8_lossy(&linear_caller.stderr)
    );
    let linear_caller_text = String::from_utf8_lossy(&linear_caller.stdout);
    assert!(linear_caller_text.contains("bl fn "));
    assert!(linear_caller_text.contains("callee@"));

    let linear_division = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-linear-ir")
        .arg(&unsupported_source)
        .arg("divide")
        .output()
        .unwrap();
    assert!(
        linear_division.status.success(),
        "typed i32 division did not reach LinearIR: {}",
        String::from_utf8_lossy(&linear_division.stderr)
    );
    let linear_division_text = String::from_utf8_lossy(&linear_division.stdout);
    assert!(linear_division_text.contains("bl rt_native_i32_div"));
    assert!(linear_division_text.contains("ldr w0,"));
    assert!(linear_division_text.contains("ldr w1,"));
}

#[test]
fn aarch64_linear_ir_abi_and_control_flow_fixtures_execute() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let include_dir = repo.join("riotc");
    let fixture = include_dir.join("backend/native/aarch64/linear_ir_test.ml");
    let dir = temp_dir("aarch64-linear-ir-abi");
    let module_path = dir.join("linear_ir_test.mjs");

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_dir)
        .arg("-o")
        .arg(&module_path)
        .arg(&fixture)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ main }} from {:?}; process.exitCode = Number(main());",
            module_path.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "LinearIR fixture failed: {}",
        String::from_utf8_lossy(&node.stderr)
    );
    let printed = String::from_utf8_lossy(&node.stdout);
    assert!(
        printed.contains("%v0:i32 = [sp,#0]"),
        "missing spill layout:\n{printed}"
    );
    assert!(
        printed.contains("%v0:unit = erased"),
        "unit was not erased:\n{printed}"
    );
    assert!(
        printed.contains("mov x0, xzr"),
        "missing hidden environment ABI:\n{printed}"
    );
    assert!(
        printed.contains(".Ledge0_true:"),
        "missing edge-copy block:\n{printed}"
    );
    assert!(
        printed.contains("b .Lbb1"),
        "missing linear branch target:\n{printed}"
    );
    assert!(printed.contains("function fn linear-ir-test::unit_caller@0 -> i32"));
    let unit_caller = printed
        .split("function fn linear-ir-test::unit_caller@0")
        .nth(1)
        .unwrap()
        .split("function fn ")
        .next()
        .unwrap();
    assert!(unit_caller.contains("mov x0, xzr"));
    assert!(
        !unit_caller.contains("ldr w1,"),
        "unit argument was not erased:\n{unit_caller}"
    );
    assert!(printed.contains("unit_return@0 -> unit entry .Lbb0 frame 0"));
    assert!(printed.contains("pointer_identity@0 -> ptr"));
    assert!(
        printed.contains("str x1,"),
        "pointer entry was not spilled from x1:\n{printed}"
    );
    assert!(
        printed.contains("ldr x0,"),
        "pointer result was not returned in x0:\n{printed}"
    );
    assert!(printed.contains("bl fn linear-ir-test::callee@0"));
    assert!(
        printed.contains("ldr x9,"),
        "pointer edge copy omitted x9:\n{printed}"
    );
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[test]
fn aarch64_codegen_assembles_links_and_removes_failed_outputs() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let include_dir = repo.join("riotc");
    let compiler_source = include_dir.join("main.ml");
    let dir = temp_dir("aarch64-codegen");
    let compiler_js = dir.join("riotc.mjs");
    let stage0 = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_dir)
        .arg("-o")
        .arg(&compiler_js)
        .arg(&compiler_source)
        .output()
        .unwrap();
    assert!(
        stage0.status.success(),
        "Stage 0 compiler stderr: {}",
        String::from_utf8_lossy(&stage0.stderr)
    );

    let runner = dir.join("run-compiler.mjs");
    fs::write(
        &runner,
        "import { main } from \"./riotc.mjs\"; process.exitCode = Number(main(undefined));\n",
    )
    .unwrap();
    let source = dir.join("main.ml");
    fs::write(&source, "let main = fn () -> 42\n").unwrap();
    let executable = dir.join("main.native");
    let compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-function")
        .arg(&source)
        .arg("main")
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "owned Codegen/link failed: {}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let assembly = fs::read_to_string(executable.with_file_name("main.native.aarch64.s")).unwrap();
    assert!(assembly.contains(".section __TEXT,__text,regular,pure_instructions"));
    assert!(assembly.contains("_main:"));
    assert!(assembly.contains("bl _rt_set_args"));
    assert!(assembly.contains("stp x29, x30"));
    assert!(assembly.contains("ldr w9, =42"));
    assert!(assembly.contains("bl _rt_pattern_fail"));
    assert!(assembly.contains("brk #260"));
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));

    let c_executable = dir.join("main.c-backend");
    let c_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("compile")
        .arg(&source)
        .arg("-o")
        .arg(&c_executable)
        .output()
        .unwrap();
    assert!(
        c_compile.status.success(),
        "C-backend differential fixture did not compile: {}",
        String::from_utf8_lossy(&c_compile.stderr)
    );
    let c_run = Command::new(&c_executable).output().unwrap();
    assert_eq!(run.status.code(), c_run.status.code());
    assert_eq!(run.stdout, c_run.stdout);
    assert_eq!(run.stderr, c_run.stderr);

    let pointer_entry_source = dir.join("pointer-entry.ml");
    fs::write(
        &pointer_entry_source,
        "let entry = fn (value: string) -> 42\n",
    )
    .unwrap();
    let pointer_executable = dir.join("pointer-entry.native");
    let pointer_assembly = dir.join("pointer-entry.native.aarch64.s");
    fs::write(&pointer_executable, "stale executable").unwrap();
    fs::write(&pointer_assembly, "stale assembly").unwrap();
    let pointer_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-function")
        .arg(&pointer_entry_source)
        .arg("entry")
        .arg("-o")
        .arg(&pointer_executable)
        .output()
        .unwrap();
    assert!(!pointer_compile.status.success());
    assert!(
        String::from_utf8_lossy(&pointer_compile.stderr)
            .contains("function-level entry cannot accept a source argument")
    );
    assert!(
        !pointer_executable.exists(),
        "rejected pointer entry left an executable"
    );
    assert!(
        !pointer_assembly.exists(),
        "rejected pointer entry left assembly"
    );

    let direct_call_source = dir.join("direct-call.ml");
    fs::write(
        &direct_call_source,
        "let callee = fn (value: i32) -> value + 1\nlet main = fn () -> callee 41\n",
    )
    .unwrap();
    let direct_call_executable = dir.join("direct-call.native");
    let direct_call_assembly = dir.join("direct-call.native.aarch64.s");
    fs::write(&direct_call_executable, "stale executable").unwrap();
    fs::write(&direct_call_assembly, "stale assembly").unwrap();
    let direct_call_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-function")
        .arg(&direct_call_source)
        .arg("main")
        .arg("-o")
        .arg(&direct_call_executable)
        .output()
        .unwrap();
    assert!(!direct_call_compile.status.success());
    assert!(
        String::from_utf8_lossy(&direct_call_compile.stderr)
            .contains("direct-call target is absent from the Codegen function set")
    );
    assert!(!direct_call_executable.exists());
    assert!(!direct_call_assembly.exists());

    let direct_graph_executable = dir.join("direct-graph.native");
    let direct_graph_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&direct_call_source)
        .arg("main")
        .arg("-o")
        .arg(&direct_graph_executable)
        .output()
        .unwrap();
    assert!(
        direct_graph_compile.status.success(),
        "direct-call graph Codegen failed: {}",
        String::from_utf8_lossy(&direct_graph_compile.stderr)
    );
    let direct_graph_run = Command::new(&direct_graph_executable).output().unwrap();
    assert_eq!(direct_graph_run.status.code(), Some(42));
    let direct_graph_c_executable = dir.join("direct-graph.c-backend");
    let direct_graph_c_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("compile")
        .arg(&direct_call_source)
        .arg("-o")
        .arg(&direct_graph_c_executable)
        .output()
        .unwrap();
    assert!(
        direct_graph_c_compile.status.success(),
        "C-backend direct-call differential fixture failed: {}",
        String::from_utf8_lossy(&direct_graph_c_compile.stderr)
    );
    let direct_graph_c_run = Command::new(&direct_graph_c_executable).output().unwrap();
    assert_eq!(
        direct_graph_run.status.code(),
        direct_graph_c_run.status.code()
    );
    assert_eq!(direct_graph_run.stdout, direct_graph_c_run.stdout);
    assert_eq!(direct_graph_run.stderr, direct_graph_c_run.stderr);

    let recursive_source = dir.join("recursive-direct-graph.ml");
    fs::write(
        &recursive_source,
        "let countdown = fn (value: i32) -> match value {\n  | 0 -> 42\n  | _ -> countdown (value - 1)\n}\nlet main = fn () -> countdown 10\n",
    )
    .unwrap();
    let recursive_executable = dir.join("recursive-direct-graph.native");
    let recursive_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&recursive_source)
        .arg("main")
        .arg("-o")
        .arg(&recursive_executable)
        .output()
        .unwrap();
    assert!(
        recursive_compile.status.success(),
        "recursive direct-call graph Codegen failed: {}",
        String::from_utf8_lossy(&recursive_compile.stderr)
    );
    let recursive_run = Command::new(&recursive_executable).output().unwrap();
    assert_eq!(recursive_run.status.code(), Some(42));
    let recursive_c_executable = dir.join("recursive-direct-graph.c-backend");
    let recursive_c_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("compile")
        .arg(&recursive_source)
        .arg("-o")
        .arg(&recursive_c_executable)
        .output()
        .unwrap();
    assert!(
        recursive_c_compile.status.success(),
        "C-backend recursive fixture failed: {}",
        String::from_utf8_lossy(&recursive_c_compile.stderr)
    );
    let recursive_c_run = Command::new(&recursive_c_executable).output().unwrap();
    assert_eq!(recursive_run.status.code(), recursive_c_run.status.code());
    assert_eq!(recursive_run.stdout, recursive_c_run.stdout);
    assert_eq!(recursive_run.stderr, recursive_c_run.stderr);

    let i64_call_source = dir.join("i64-direct-graph.ml");
    fs::write(
        &i64_call_source,
        "let echo = fn (value: i64) -> value\nlet choose = fn flag -> match flag { | true -> echo (9223372036854775807: i64) | false -> echo (-9223372036854775808: i64) }\nlet take_i64 = fn (ignored: i64) -> 42\nlet main = fn () -> take_i64 (choose true)\n",
    )
    .unwrap();
    let i64_call_executable = dir.join("i64-direct-graph.native");
    let i64_call_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&i64_call_source)
        .arg("main")
        .arg("-o")
        .arg(&i64_call_executable)
        .output()
        .unwrap();
    assert!(
        i64_call_compile.status.success(),
        "i64 direct-call graph Codegen failed: {}",
        String::from_utf8_lossy(&i64_call_compile.stderr)
    );
    let i64_call_assembly =
        fs::read_to_string(i64_call_executable.with_file_name("i64-direct-graph.native.aarch64.s"))
            .unwrap();
    assert!(i64_call_assembly.contains("ldr x9, =9223372036854775807"));
    assert!(i64_call_assembly.contains("ldr x9, =-9223372036854775808"));
    assert!(i64_call_assembly.contains("    str x1, [sp,#0]\n    ldr x0, [sp,#0]"));
    assert!(i64_call_assembly.contains("ldr x1"));
    assert!(i64_call_assembly.contains("str x0"));
    assert!(i64_call_assembly.contains("str x9"));
    let i64_call_run = Command::new(&i64_call_executable).output().unwrap();
    assert_eq!(i64_call_run.status.code(), Some(42));

    let i64_call_c_executable = dir.join("i64-direct-graph.c-backend");
    let i64_call_c_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("compile")
        .arg(&i64_call_source)
        .arg("-o")
        .arg(&i64_call_c_executable)
        .output()
        .unwrap();
    assert!(
        i64_call_c_compile.status.success(),
        "C-backend i64 differential fixture failed: {}",
        String::from_utf8_lossy(&i64_call_c_compile.stderr)
    );
    let i64_call_c_run = Command::new(&i64_call_c_executable).output().unwrap();
    assert_eq!(i64_call_run.status.code(), i64_call_c_run.status.code());
    assert_eq!(i64_call_run.stdout, i64_call_c_run.stdout);
    assert_eq!(i64_call_run.stderr, i64_call_c_run.stderr);

    let i64_negation_source = dir.join("i64-negation.ml");
    fs::write(
        &i64_negation_source,
        "let negate = fn (value: i64) -> -value\nlet take_i64 = fn (ignored: i64) -> 42\nlet main = fn () -> take_i64 (negate (9223372036854775807: i64))\n",
    )
    .unwrap();
    let i64_negation_executable = dir.join("i64-negation.native");
    let i64_negation_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&i64_negation_source)
        .arg("main")
        .arg("-o")
        .arg(&i64_negation_executable)
        .output()
        .unwrap();
    assert!(
        i64_negation_compile.status.success(),
        "i64 negation graph Codegen failed: {}",
        String::from_utf8_lossy(&i64_negation_compile.stderr)
    );
    let i64_negation_assembly =
        fs::read_to_string(i64_negation_executable.with_file_name("i64-negation.native.aarch64.s"))
            .unwrap();
    assert!(i64_negation_assembly.contains("neg x11, x9"));
    let i64_negation_run = Command::new(&i64_negation_executable).output().unwrap();
    assert_eq!(i64_negation_run.status.code(), Some(42));
    let i64_negation_c_executable = dir.join("i64-negation.c-backend");
    let i64_negation_c_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("compile")
        .arg(&i64_negation_source)
        .arg("-o")
        .arg(&i64_negation_c_executable)
        .output()
        .unwrap();
    assert!(!i64_negation_c_compile.status.success());
    assert!(
        String::from_utf8_lossy(&i64_negation_c_compile.stderr)
            .contains("C backend does not support negation on `i64`")
    );
    assert!(!i64_negation_c_executable.exists());

    let i64_arithmetic_source = dir.join("i64-arithmetic.ml");
    fs::write(
        &i64_arithmetic_source,
        "let calculate = fn (value: i64) -> value + (1: i64) - (2: i64) * (3: i64)\nlet take_i64 = fn (ignored: i64) -> 42\nlet main = fn () -> take_i64 (calculate (10: i64))\n",
    )
    .unwrap();
    let i64_arithmetic_executable = dir.join("i64-arithmetic.native");
    let i64_arithmetic_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&i64_arithmetic_source)
        .arg("main")
        .arg("-o")
        .arg(&i64_arithmetic_executable)
        .output()
        .unwrap();
    assert!(
        i64_arithmetic_compile.status.success(),
        "i64 arithmetic graph Codegen failed: {}",
        String::from_utf8_lossy(&i64_arithmetic_compile.stderr)
    );
    let i64_arithmetic_assembly = fs::read_to_string(
        i64_arithmetic_executable.with_file_name("i64-arithmetic.native.aarch64.s"),
    )
    .unwrap();
    assert!(i64_arithmetic_assembly.contains("add x11, x9, x10"));
    assert!(i64_arithmetic_assembly.contains("sub x11, x9, x10"));
    assert!(i64_arithmetic_assembly.contains("mul x11, x9, x10"));
    let i64_arithmetic_run = Command::new(&i64_arithmetic_executable).output().unwrap();
    assert_eq!(i64_arithmetic_run.status.code(), Some(42));
    let i64_arithmetic_c_executable = dir.join("i64-arithmetic.c-backend");
    let i64_arithmetic_c_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("compile")
        .arg(&i64_arithmetic_source)
        .arg("-o")
        .arg(&i64_arithmetic_c_executable)
        .output()
        .unwrap();
    assert!(!i64_arithmetic_c_compile.status.success());
    assert!(
        String::from_utf8_lossy(&i64_arithmetic_c_compile.stderr)
            .contains("C backend does not support integer arithmetic on `i64`")
    );
    assert!(!i64_arithmetic_c_executable.exists());

    let i64_comparison_source = dir.join("i64-comparison.ml");
    fs::write(
        &i64_comparison_source,
        "let is_greater = fn (value: i64) -> value > (9: i64)\nlet main = fn () -> match (is_greater (10: i64)) { | true -> 42 | false -> 0 }\n",
    )
    .unwrap();
    let i64_comparison_executable = dir.join("i64-comparison.native");
    let i64_comparison_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&i64_comparison_source)
        .arg("main")
        .arg("-o")
        .arg(&i64_comparison_executable)
        .output()
        .unwrap();
    assert!(
        i64_comparison_compile.status.success(),
        "i64 comparison graph Codegen failed: {}",
        String::from_utf8_lossy(&i64_comparison_compile.stderr)
    );
    let i64_comparison_assembly = fs::read_to_string(
        i64_comparison_executable.with_file_name("i64-comparison.native.aarch64.s"),
    )
    .unwrap();
    assert!(i64_comparison_assembly.contains("cmp x9, x10"));
    assert!(i64_comparison_assembly.contains("cset w11, gt"));
    let i64_comparison_run = Command::new(&i64_comparison_executable).output().unwrap();
    assert_eq!(i64_comparison_run.status.code(), Some(42));
    let i64_comparison_c_executable = dir.join("i64-comparison.c-backend");
    let i64_comparison_c_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("compile")
        .arg(&i64_comparison_source)
        .arg("-o")
        .arg(&i64_comparison_c_executable)
        .output()
        .unwrap();
    assert!(!i64_comparison_c_compile.status.success());
    assert!(
        String::from_utf8_lossy(&i64_comparison_c_compile.stderr)
            .contains("C backend does not support integer comparison on `i64`")
    );
    assert!(!i64_comparison_c_executable.exists());

    let i64_division_source = dir.join("i64-division.ml");
    fs::write(
        &i64_division_source,
        "let calculate = fn (value: i64) -> (value / (2: i64)) + (value % (5: i64))\nlet main = fn () -> { let ignored = calculate (85: i64); 42 }\n",
    )
    .unwrap();
    let i64_division_executable = dir.join("i64-division.native");
    let i64_division_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&i64_division_source)
        .arg("main")
        .arg("-o")
        .arg(&i64_division_executable)
        .output()
        .unwrap();
    assert!(
        i64_division_compile.status.success(),
        "i64 division/remainder graph Codegen failed: {}",
        String::from_utf8_lossy(&i64_division_compile.stderr)
    );
    let i64_division_assembly =
        fs::read_to_string(i64_division_executable.with_file_name("i64-division.native.aarch64.s"))
            .unwrap();
    assert!(i64_division_assembly.contains("bl _rt_native_i64_div"));
    assert!(i64_division_assembly.contains("bl _rt_native_i64_rem"));
    let i64_division_run = Command::new(&i64_division_executable).output().unwrap();
    assert_eq!(i64_division_run.status.code(), Some(42));

    let i64_division_zero_source = dir.join("i64-division-zero.ml");
    fs::write(
        &i64_division_zero_source,
        "let divide = fn () -> (1: i64) / (0: i64)\nlet main = fn () -> { let ignored = divide (); 42 }\n",
    )
    .unwrap();
    let i64_division_zero_executable = dir.join("i64-division-zero.native");
    let i64_division_zero_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&i64_division_zero_source)
        .arg("main")
        .arg("-o")
        .arg(&i64_division_zero_executable)
        .output()
        .unwrap();
    assert!(
        i64_division_zero_compile.status.success(),
        "i64 division-by-zero graph Codegen failed: {}",
        String::from_utf8_lossy(&i64_division_zero_compile.stderr)
    );
    let i64_division_zero_run = Command::new(&i64_division_zero_executable)
        .output()
        .unwrap();
    assert!(!i64_division_zero_run.status.success());
    assert!(
        String::from_utf8_lossy(&i64_division_zero_run.stderr)
            .contains("riotml runtime: division by zero")
    );

    let i64_division_overflow_source = dir.join("i64-division-overflow.ml");
    fs::write(
        &i64_division_overflow_source,
        "let minimum = fn () -> (-(9223372036854775807: i64)) - (1: i64)\nlet main = fn () -> { let min = minimum (); let quotient_ok = (min / (-1: i64)) == min; let remainder_ok = (min % (-1: i64)) == (0: i64); match quotient_ok { | true -> match remainder_ok { | true -> 42 | false -> 0 } | false -> 0 } }\n",
    )
    .unwrap();
    let i64_division_overflow_executable = dir.join("i64-division-overflow.native");
    let i64_division_overflow_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&i64_division_overflow_source)
        .arg("main")
        .arg("-o")
        .arg(&i64_division_overflow_executable)
        .output()
        .unwrap();
    assert!(
        i64_division_overflow_compile.status.success(),
        "i64 division-overflow graph Codegen failed: {}",
        String::from_utf8_lossy(&i64_division_overflow_compile.stderr)
    );
    let i64_division_overflow_run = Command::new(&i64_division_overflow_executable)
        .output()
        .unwrap();
    assert_eq!(i64_division_overflow_run.status.code(), Some(42));

    let i64_division_c_executable = dir.join("i64-division.c-backend");
    let i64_division_c_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("compile")
        .arg(&i64_division_source)
        .arg("-o")
        .arg(&i64_division_c_executable)
        .output()
        .unwrap();
    assert!(!i64_division_c_compile.status.success());
    assert!(
        String::from_utf8_lossy(&i64_division_c_compile.stderr)
            .contains("C backend does not support addition on `i64`")
    );
    assert!(!i64_division_c_executable.exists());
    assert!(!i64_division_c_executable.with_extension("c").exists());

    let out_of_range_i64_source = dir.join("out-of-range-i64.ml");
    fs::write(
        &out_of_range_i64_source,
        "let main = fn () -> { let ignored = (9223372036854775808: i64); 42 }\n",
    )
    .unwrap();
    let out_of_range_i64_executable = dir.join("out-of-range-i64.native");
    let out_of_range_i64_assembly = dir.join("out-of-range-i64.native.aarch64.s");
    fs::write(&out_of_range_i64_executable, "stale executable").unwrap();
    fs::write(&out_of_range_i64_assembly, "stale assembly").unwrap();
    let out_of_range_i64_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&out_of_range_i64_source)
        .arg("main")
        .arg("-o")
        .arg(&out_of_range_i64_executable)
        .output()
        .unwrap();
    assert!(!out_of_range_i64_compile.status.success());
    assert!(
        String::from_utf8_lossy(&out_of_range_i64_compile.stderr)
            .contains("integer literal is outside i64 range")
    );
    assert!(!out_of_range_i64_executable.exists());
    assert!(!out_of_range_i64_assembly.exists());

    let out_of_range_i32_source = dir.join("out-of-range-i32.ml");
    fs::write(
        &out_of_range_i32_source,
        "let main = fn () -> { let ignored = 2147483648; 42 }\n",
    )
    .unwrap();
    let out_of_range_i32_executable = dir.join("out-of-range-i32.native");
    fs::write(&out_of_range_i32_executable, "stale executable").unwrap();
    let out_of_range_i32_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&out_of_range_i32_source)
        .arg("main")
        .arg("-o")
        .arg(&out_of_range_i32_executable)
        .output()
        .unwrap();
    assert!(!out_of_range_i32_compile.status.success());
    assert!(
        String::from_utf8_lossy(&out_of_range_i32_compile.stderr)
            .contains("integer literal is outside i32 range")
    );
    assert!(!out_of_range_i32_executable.exists());

    let unsupported_i8_source = dir.join("unsupported-i8.ml");
    fs::write(
        &unsupported_i8_source,
        "let take_i8 = fn (value: i8) -> 42\nlet main = fn () -> take_i8 (1: i8)\n",
    )
    .unwrap();
    let unsupported_i8_executable = dir.join("unsupported-i8.native");
    let unsupported_i8_assembly = dir.join("unsupported-i8.native.aarch64.s");
    fs::write(&unsupported_i8_executable, "stale executable").unwrap();
    fs::write(&unsupported_i8_assembly, "stale assembly").unwrap();
    let unsupported_i8_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&unsupported_i8_source)
        .arg("main")
        .arg("-o")
        .arg(&unsupported_i8_executable)
        .output()
        .unwrap();
    assert!(!unsupported_i8_compile.status.success());
    assert!(
        String::from_utf8_lossy(&unsupported_i8_compile.stderr)
            .contains("i8 has no AArch64 representation in this backend slice")
    );
    assert!(!unsupported_i8_executable.exists());
    assert!(!unsupported_i8_assembly.exists());

    let global_initializer_source = dir.join("global-initializer.ml");
    fs::write(
        &global_initializer_source,
        "let answer = 42\nlet main = fn () -> answer\n",
    )
    .unwrap();
    let global_initializer_executable = dir.join("global-initializer.native");
    let global_initializer_assembly = dir.join("global-initializer.native.aarch64.s");
    fs::write(&global_initializer_executable, "stale executable").unwrap();
    fs::write(&global_initializer_assembly, "stale assembly").unwrap();
    let global_initializer_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&global_initializer_source)
        .arg("main")
        .arg("-o")
        .arg(&global_initializer_executable)
        .output()
        .unwrap();
    assert!(!global_initializer_compile.status.success());
    assert!(
        String::from_utf8_lossy(&global_initializer_compile.stderr).contains("module initializer")
    );
    assert!(!global_initializer_executable.exists());
    assert!(!global_initializer_assembly.exists());

    let closure_source = dir.join("closure.ml");
    fs::write(
        &closure_source,
        "let main = fn () -> { let identity = fn (value: i32) -> value; identity 42 }\n",
    )
    .unwrap();
    let closure_executable = dir.join("closure.native");
    let closure_assembly = dir.join("closure.native.aarch64.s");
    fs::write(&closure_executable, "stale executable").unwrap();
    fs::write(&closure_assembly, "stale assembly").unwrap();
    let closure_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-direct-graph")
        .arg(&closure_source)
        .arg("main")
        .arg("-o")
        .arg(&closure_executable)
        .output()
        .unwrap();
    assert!(!closure_compile.status.success());
    assert!(
        String::from_utf8_lossy(&closure_compile.stderr)
            .contains("closure creation requires a concrete native closure layout")
    );
    assert!(!closure_executable.exists());
    assert!(!closure_assembly.exists());

    let host_args_source = dir.join("host-args.ml");
    fs::write(
        &host_args_source,
        "use Host.{args}\nlet main = fn () -> match args () { | _ -> 42 }\n",
    )
    .unwrap();
    let host_args_executable = dir.join("host-args.native");
    let host_args_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-function")
        .arg(&host_args_source)
        .arg("main")
        .arg("-o")
        .arg(&host_args_executable)
        .output()
        .unwrap();
    assert!(
        host_args_compile.status.success(),
        "native Host.args bridge failed: {}",
        String::from_utf8_lossy(&host_args_compile.stderr)
    );
    let host_args_assembly =
        fs::read_to_string(host_args_executable.with_file_name("host-args.native.aarch64.s"))
            .unwrap();
    assert!(host_args_assembly.contains("bl _rt_native_host_args"));
    let host_args_run = Command::new(&host_args_executable).output().unwrap();
    assert_eq!(host_args_run.status.code(), Some(42));

    let host_args_c_executable = dir.join("host-args.c-backend");
    let host_args_c_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("compile")
        .arg(&host_args_source)
        .arg("-o")
        .arg(&host_args_c_executable)
        .output()
        .unwrap();
    assert!(
        host_args_c_compile.status.success(),
        "C-backend Host.args differential fixture did not compile: {}",
        String::from_utf8_lossy(&host_args_c_compile.stderr)
    );
    let host_args_c_run = Command::new(&host_args_c_executable).output().unwrap();
    assert_eq!(host_args_run.status.code(), host_args_c_run.status.code());
    assert_eq!(host_args_run.stdout, host_args_c_run.stdout);
    assert_eq!(host_args_run.stderr, host_args_c_run.stderr);

    let host_args_probe = include_dir.join("backend/native/aarch64/host_args_abi_probe.c");
    let host_args_probe_executable = dir.join("host-args-abi-probe");
    let host_args_probe_compile = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&include_dir)
        .arg(&host_args_probe)
        .arg(include_dir.join("runtime.c"))
        .arg("-o")
        .arg(&host_args_probe_executable)
        .output()
        .unwrap();
    assert!(
        host_args_probe_compile.status.success(),
        "Host.args ABI probe failed to compile: {}",
        String::from_utf8_lossy(&host_args_probe_compile.stderr)
    );
    let host_args_probe_run = Command::new(&host_args_probe_executable)
        .arg("alpha")
        .arg("beta")
        .output()
        .unwrap();
    assert_eq!(host_args_probe_run.status.code(), Some(0));

    let unsupported_host_source = dir.join("unsupported-host.ml");
    fs::write(
        &unsupported_host_source,
        "use Host.{current_directory}\nlet main = fn () -> match current_directory () { | _ -> 42 }\n",
    )
    .unwrap();
    let unsupported_host_executable = dir.join("unsupported-host.native");
    let unsupported_host_assembly = dir.join("unsupported-host.native.aarch64.s");
    let unsupported_host_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-function")
        .arg(&unsupported_host_source)
        .arg("main")
        .arg("-o")
        .arg(&unsupported_host_executable)
        .output()
        .unwrap();
    assert!(!unsupported_host_compile.status.success());
    assert!(
        String::from_utf8_lossy(&unsupported_host_compile.stderr)
            .contains("unsupported Host helper `current_directory`")
    );
    assert!(!unsupported_host_executable.exists());
    assert!(!unsupported_host_assembly.exists());

    let nested_pattern_source = dir.join("unsupported-pattern.ml");
    fs::write(
        &nested_pattern_source,
        "use Host.{args}\nuse Prelude.{Cons, Nil}\nlet main = fn () -> match args () { | Cons(\"run\", Cons(_, Nil)) -> 42 | _ -> 0 }\n",
    )
    .unwrap();
    let nested_pattern_executable = dir.join("unsupported-pattern.native");
    let nested_pattern_assembly = dir.join("unsupported-pattern.native.aarch64.s");
    fs::write(&nested_pattern_executable, "stale executable").unwrap();
    fs::write(&nested_pattern_assembly, "stale assembly").unwrap();
    let nested_pattern_compile = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-function")
        .arg(&nested_pattern_source)
        .arg("main")
        .arg("-o")
        .arg(&nested_pattern_executable)
        .output()
        .unwrap();
    assert!(!nested_pattern_compile.status.success());
    assert!(
        String::from_utf8_lossy(&nested_pattern_compile.stderr)
            .contains("nested constructor patterns require explicit runtime matching")
    );
    assert!(!nested_pattern_executable.exists());
    assert!(!nested_pattern_assembly.exists());

    for (fixture_name, source_text, expected_status) in [
        ("arithmetic", "let main = fn () -> 20 + 22\n", 42),
        ("subtraction", "let main = fn () -> 84 - 42\n", 42),
        ("multiplication", "let main = fn () -> 6 * 7\n", 42),
        ("negation", "let main = fn () -> -42\n", 214),
        (
            "boolean-branch",
            "let main = fn () -> match true { | true -> 42 | false -> 41 }\n",
            42,
        ),
        (
            "comparison-equal",
            "let main = fn () -> match (20 == 20) { | true -> 42 | false -> 41 }\n",
            42,
        ),
        (
            "comparison-not-equal",
            "let main = fn () -> match (20 != 21) { | true -> 42 | false -> 41 }\n",
            42,
        ),
        (
            "comparison-less",
            "let main = fn () -> match (20 < 21) { | true -> 42 | false -> 41 }\n",
            42,
        ),
        (
            "comparison-less-equal",
            "let main = fn () -> match (20 <= 20) { | true -> 42 | false -> 41 }\n",
            42,
        ),
        (
            "comparison-greater",
            "let main = fn () -> match (21 > 20) { | true -> 42 | false -> 41 }\n",
            42,
        ),
        (
            "comparison-greater-equal",
            "let main = fn () -> match (20 >= 20) { | true -> 42 | false -> 41 }\n",
            42,
        ),
        ("wrapped-add", "let main = fn () -> 2147483647 + 1\n", 0),
        ("division", "let main = fn () -> 7 / 2\n", 3),
        ("remainder", "let main = fn () -> 7 % 2\n", 1),
        (
            "division-overflow",
            "let main = fn () -> (0 - 2147483647 - 1) / (0 - 1)\n",
            0,
        ),
        (
            "remainder-overflow",
            "let main = fn () -> (0 - 2147483647 - 1) % (0 - 1)\n",
            0,
        ),
    ] {
        let fixture_source = dir.join(format!("{fixture_name}.ml"));
        fs::write(&fixture_source, source_text).unwrap();
        let native_executable = dir.join(format!("{fixture_name}.native"));
        let native_compile = Command::new("node")
            .current_dir(repo)
            .arg(&runner)
            .arg("aarch64-compile-function")
            .arg(&fixture_source)
            .arg("main")
            .arg("-o")
            .arg(&native_executable)
            .output()
            .unwrap();
        assert!(
            native_compile.status.success(),
            "native {fixture_name} fixture failed: {}",
            String::from_utf8_lossy(&native_compile.stderr)
        );
        let c_executable = dir.join(format!("{fixture_name}.c-backend"));
        let c_compile = Command::new("node")
            .current_dir(repo)
            .arg(&runner)
            .arg("compile")
            .arg(&fixture_source)
            .arg("-o")
            .arg(&c_executable)
            .output()
            .unwrap();
        assert!(
            c_compile.status.success(),
            "C-backend {fixture_name} fixture failed: {}",
            String::from_utf8_lossy(&c_compile.stderr)
        );
        let native_run = Command::new(&native_executable)
            .current_dir(&dir)
            .output()
            .unwrap();
        let c_run = Command::new(&c_executable)
            .current_dir(&dir)
            .output()
            .unwrap();
        assert_eq!(native_run.status.code(), Some(expected_status));
        assert_eq!(native_run.status.code(), c_run.status.code());
        assert_eq!(native_run.stdout, c_run.stdout);
        assert_eq!(native_run.stderr, c_run.stderr);
    }

    use std::os::unix::process::ExitStatusExt;
    for (fixture_name, source_text, diagnostic) in [
        (
            "division-by-zero",
            "let main = fn () -> 1 / 0\n",
            "riotml runtime: division by zero",
        ),
        (
            "remainder-by-zero",
            "let main = fn () -> 1 % 0\n",
            "riotml runtime: remainder by zero",
        ),
        (
            "pattern-failure",
            "let main = fn () -> match 0 { | 1 -> 42 }\n",
            "riotml runtime: RiotML pattern match failed",
        ),
    ] {
        let fixture_source = dir.join(format!("{fixture_name}.ml"));
        fs::write(&fixture_source, source_text).unwrap();
        let native_executable = dir.join(format!("{fixture_name}.native"));
        let native_compile = Command::new("node")
            .current_dir(repo)
            .arg(&runner)
            .arg("aarch64-compile-function")
            .arg(&fixture_source)
            .arg("main")
            .arg("-o")
            .arg(&native_executable)
            .output()
            .unwrap();
        assert!(
            native_compile.status.success(),
            "native {fixture_name} fixture failed to link: {}",
            String::from_utf8_lossy(&native_compile.stderr)
        );
        let c_executable = dir.join(format!("{fixture_name}.c-backend"));
        let c_compile = Command::new("node")
            .current_dir(repo)
            .arg(&runner)
            .arg("compile")
            .arg(&fixture_source)
            .arg("-o")
            .arg(&c_executable)
            .output()
            .unwrap();
        assert!(
            c_compile.status.success(),
            "C-backend {fixture_name} fixture failed to link: {}",
            String::from_utf8_lossy(&c_compile.stderr)
        );
        let native_run = Command::new(&native_executable)
            .current_dir(&dir)
            .output()
            .unwrap();
        let c_run = Command::new(&c_executable)
            .current_dir(&dir)
            .output()
            .unwrap();
        assert_eq!(native_run.status.signal(), c_run.status.signal());
        assert_eq!(native_run.stdout, c_run.stdout);
        assert_eq!(native_run.stderr, c_run.stderr);
        assert!(
            String::from_utf8_lossy(&native_run.stderr).contains(diagnostic),
            "unexpected {fixture_name} diagnostic: {}",
            String::from_utf8_lossy(&native_run.stderr)
        );
    }

    let ill_typed_source = dir.join("ill-typed.ml");
    fs::write(&ill_typed_source, "let main = fn () -> true + 1\n").unwrap();
    let failed_output = dir.join("ill-typed.native");
    let failed_assembly = dir.join("ill-typed.native.aarch64.s");
    fs::write(&failed_output, b"stale output must not survive").unwrap();
    fs::write(&failed_assembly, b"stale assembly must not survive").unwrap();
    let rejected = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-function")
        .arg(&ill_typed_source)
        .arg("main")
        .arg("-o")
        .arg(&failed_output)
        .output()
        .unwrap();
    assert!(
        !rejected.status.success(),
        "ill-typed source unexpectedly compiled"
    );
    assert!(
        !failed_output.exists(),
        "failed native compilation left a plausible executable"
    );
    assert!(
        !failed_assembly.exists(),
        "failed native compilation left stale assembly"
    );
    assert!(
        String::from_utf8_lossy(&rejected.stderr).contains("type error"),
        "unexpected typechecking diagnostic: {}",
        String::from_utf8_lossy(&rejected.stderr)
    );

    let unsupported_source = dir.join("unsupported.ml");
    fs::write(&unsupported_source, "let main = fn () -> \"not-native\"\n").unwrap();
    let unsupported_output = dir.join("unsupported.native");
    let unsupported_assembly = dir.join("unsupported.native.aarch64.s");
    fs::write(&unsupported_output, b"stale output must not survive").unwrap();
    fs::write(&unsupported_assembly, b"stale assembly must not survive").unwrap();
    let unsupported = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-function")
        .arg(&unsupported_source)
        .arg("main")
        .arg("-o")
        .arg(&unsupported_output)
        .output()
        .unwrap();
    assert!(
        !unsupported.status.success(),
        "unsupported string result unexpectedly compiled"
    );
    assert!(!unsupported_output.exists());
    assert!(!unsupported_assembly.exists());
    assert!(
        String::from_utf8_lossy(&unsupported.stderr)
            .contains("string constants are outside the scalar selector")
    );

    let fake_bin = dir.join("fake-bin");
    fs::create_dir_all(&fake_bin).unwrap();
    let fake_clang = fake_bin.join("clang");
    fs::write(
        &fake_clang,
        "#!/bin/sh\nwhile [ \"$#\" -gt 0 ]; do\n  if [ \"$1\" = \"-o\" ]; then\n    shift\n    printf 'partial link output' > \"$1\"\n    exit 1\n  fi\n  shift\ndone\nexit 1\n",
    )
    .unwrap();
    use std::os::unix::fs::PermissionsExt;
    fs::set_permissions(&fake_clang, fs::Permissions::from_mode(0o755)).unwrap();
    let linker_failure_output = dir.join("linker-failure.native");
    let original_path = std::env::var_os("PATH").unwrap_or_default();
    let test_path = format!("{}:{}", fake_bin.display(), original_path.to_string_lossy());
    let linker_failure = Command::new("node")
        .env("PATH", test_path)
        .current_dir(repo)
        .arg(&runner)
        .arg("aarch64-compile-function")
        .arg(&source)
        .arg("main")
        .arg("-o")
        .arg(&linker_failure_output)
        .output()
        .unwrap();
    assert!(!linker_failure.status.success());
    assert!(
        !linker_failure_output.exists(),
        "failed linker left a partial executable"
    );
    assert!(
        String::from_utf8_lossy(&linker_failure.stderr).contains("Clang assembler/linker failed")
    );
}

#[test]
fn aarch64_machine_ir_scalar_selection_fixtures_execute() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let include_dir = repo.join("riotc");
    let fixture = include_dir.join("backend/native/aarch64/machine_ir_test.ml");
    let dir = temp_dir("aarch64-machine-ir-selection");
    let module_path = dir.join("machine_ir_test.mjs");

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_dir)
        .arg("-o")
        .arg(&module_path)
        .arg(&fixture)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let runner = dir.join("run.mjs");
    fs::write(
        &runner,
        "import { main } from \"./machine_ir_test.mjs\"; process.exitCode = Number(main());\n",
    )
    .unwrap();
    let node = Command::new("node").arg(&runner).output().unwrap();
    assert!(
        node.status.success(),
        "fixture stderr: {}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn riotc_native_ir_command_verifies_and_prints_nominal_aggregates() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let include_dir = repo.join("riotc");
    let compiler_source = include_dir.join("main.ml");
    let dir = temp_dir("riotc-native-ir-command");
    let compiler_js = dir.join("riotc.mjs");
    let stage0 = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_dir)
        .arg("-o")
        .arg(&compiler_js)
        .arg(&compiler_source)
        .output()
        .unwrap();
    assert!(
        stage0.status.success(),
        "Stage 0 compiler stderr: {}",
        String::from_utf8_lossy(&stage0.stderr)
    );

    let source = dir.join("main.ml");
    fs::write(
        &source,
        "type User = User { name: string, age: i32 }\ntype Choice = Red | Blue(i32)\nlet person = User { name: \"Riot\", age: 42 }\nlet choice = Blue(42)\nlet main = fn () -> 42\n",
    )
    .unwrap();
    let runner = dir.join("run-compiler.mjs");
    fs::write(
        &runner,
        "import { main } from \"./riotc.mjs\"; process.exitCode = Number(main(undefined));\n",
    )
    .unwrap();
    let output = Command::new("node")
        .current_dir(repo)
        .arg(&runner)
        .arg("native-ir")
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "NativeIR command stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let printed = String::from_utf8_lossy(&output.stdout);
    assert!(
        printed.contains(" = record "),
        "missing record aggregate in IR:\n{printed}"
    );
    assert!(
        printed.contains(" = variant "),
        "missing variant aggregate in IR:\n{printed}"
    );
    assert!(
        printed.contains("name=%") && printed.contains("age=%"),
        "record field identities were not preserved:\n{printed}"
    );
}

#[test]
fn emit_writes_selected_artifacts() {
    let dir = temp_dir("emit");
    let file = write_valid_source(&dir);

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--stage")
        .arg("ast")
        .arg("--stage")
        .arg("tir")
        .arg("--stage")
        .arg("lambda")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    for extension in ["ast", "tir", "lambda", "mjs"] {
        let artifact = dir.join(format!("hello.{extension}"));
        assert!(artifact.exists(), "missing artifact {}", artifact.display());
    }
}

#[test]
fn emit_javascript_produces_executable_module() {
    let dir = temp_dir("emit-js");
    let file = dir.join("main.ml");
    fs::write(&file, "let add = fn x -> x + 1\n").unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let module = dir.join("main.mjs");
    assert!(module.exists());
    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ add }} from {:?}; if (add(41n) !== 42n) process.exit(1)",
            module.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_match_module_executes() {
    let dir = temp_dir("emit-js-match");
    let file = dir.join("match.ml");
    fs::write(
        &file,
        "type Option<'a> = | None | Some('a)\nlet get = fn x -> match x { | None -> 0 | Some(value) -> value }\nlet unwrap = fn (Some(value)) -> value\n",
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let module = dir.join("match.mjs");
    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ get, unwrap }} from {:?}; if (get({{type: 'Some', value: 41n}}) !== 41n || get({{type: 'None'}}) !== 0n || unwrap({{type: 'Some', value: 9n}}) !== 9n) process.exit(1)",
            module.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_recursive_functions_execute() {
    let dir = temp_dir("emit-js-recursive");
    let file = dir.join("fact.ml");
    fs::write(
        &file,
        "let fact = fn n -> match n { | 0 -> 1 | n -> n * fact (n - 1) }\n",
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ fact }} from {:?}; if (fact(6n) !== 720n) process.exit(1)",
            dir.join("fact.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_closures_capture_values() {
    let dir = temp_dir("emit-js-closure");
    let file = dir.join("closure.ml");
    fs::write(&file, "let make_adder = fn x -> fn y -> x + y\n").unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ make_adder }} from {:?}; if (make_adder(2n)(40n) !== 42n) process.exit(1)",
            dir.join("closure.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_multi_parameter_functions_use_curried_calls() {
    let dir = temp_dir("emit-js-curried-functions");
    let file = dir.join("curried.ml");
    fs::write(
        &file,
        r#"let add left right = left + right
let result = add 20 22
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .current_dir(&dir)
        .arg("compile")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ add, result }} from {:?}; if (add(20n)(22n) !== 42n || result !== 42n) process.exit(1)",
            dir.join("curried.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_comparisons_execute() {
    let dir = temp_dir("emit-js-comparisons");
    let file = dir.join("comparisons.ml");
    fs::write(
        &file,
        "let less = fn x -> x < 3\nlet same = fn x -> x == 3\n",
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ less, same }} from {:?}; if (!less(2n) || less(3n) || !same(3n) || same(2n)) process.exit(1)",
            dir.join("comparisons.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_tuple_and_record_patterns_execute() {
    let dir = temp_dir("emit-js-patterns");
    let file = dir.join("patterns.ml");
    fs::write(
        &file,
        "type User = User { name: string, age: i32 }\nlet first = fn (a, b) -> a\nlet get_name = fn (User { name, age: _ }) -> name\n",
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ first, get_name }} from {:?}; if (first([7n, 8n]) !== 7n || get_name({{name: 'Ada', age: 42n}}) !== 'Ada') process.exit(1)",
            dir.join("patterns.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_multiple_tuple_lets_use_distinct_bindings() {
    let dir = temp_dir("emit-js-multiple-tuple-lets");
    let file = dir.join("tuple_lets.ml");
    fs::write(
        &file,
        "let result = { let (first, _) = (1, 2); let (second, _) = (3, 4); first + second }",
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .current_dir(&dir)
        .arg("compile")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result !== 4n) process.exit(1)",
            dir.join("tuple_lets.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_fixed_width_integer_operations_are_exact() {
    let dir = temp_dir("emit-js-integers");
    let file = dir.join("integers.ml");
    fs::write(
        &file,
        r#"
        let add_i8 = fn (value: i8) -> value + 1
        let add_i16 = fn (value: i16) -> value + 1
        let add_i32 = fn (value: i32) -> value + 1
        let add_i64 = fn (value: i64) -> value + 1
        let add_u8 = fn (value: u8) -> value + 1
        let add_u16 = fn (value: u16) -> value + 1
        let add_u32 = fn (value: u32) -> value + 1
        let add_u64 = fn (value: u64) -> value + 1
        let subtract_i8 = fn (value: i8) -> value - 1
        let multiply_u8 = fn (value: u8) -> value * 2
        let unsigned_divide = fn (left: u8) -> fn (right: u8) -> left / right
        let unsigned_remainder = fn (left: u8) -> fn (right: u8) -> left % right
        let signed_divide = fn (left: i32) -> fn (right: i32) -> left / right
        let signed_remainder = fn (left: i32) -> fn (right: i32) -> left % right
        let exact_u64 = (18446744073709551615: u64)
        let radix_i8 = (-0x80: i8)
        "#,
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("compile")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import * as m from {:?}; let trapped = false; try {{ m.signed_divide(1n)(0n) }} catch (error) {{ trapped = error instanceof RangeError }}; if (m.add_i8(127n) !== -128n || m.add_i16(32767n) !== -32768n || m.add_i32(2147483647n) !== -2147483648n || m.add_i64(9223372036854775807n) !== -9223372036854775808n || m.add_u8(255n) !== 0n || m.add_u16(65535n) !== 0n || m.add_u32(4294967295n) !== 0n || m.add_u64(18446744073709551615n) !== 0n || m.subtract_i8(-128n) !== 127n || m.multiply_u8(200n) !== 144n || m.unsigned_divide(255n)(10n) !== 25n || m.unsigned_remainder(255n)(10n) !== 5n || m.signed_divide(-7n)(2n) !== -3n || m.signed_remainder(-7n)(2n) !== -1n || m.exact_u64 !== 18446744073709551615n || m.radix_i8 !== -128n || !trapped) process.exit(1)",
            dir.join("integers.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_string_literals_decode_escapes() {
    let dir = temp_dir("emit-js-string-escapes");
    let file = dir.join("strings.ml");
    let escape = char::from(92);
    let quote = char::from(34);
    let source = format!(
        "let escaped = {quote}line{escape}nquote: {escape}{quote}yes{escape}{quote} slash: {escape}{escape}{quote}\nlet unknown = {quote}unknown{escape}q{quote}\n"
    );
    fs::write(&file, source).unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("compile")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let expected = format!(
        "line{}quote: {quote}yes{quote} slash: {escape}",
        char::from(10)
    );
    let codepoints = expected
        .chars()
        .map(|character| (character as u32).to_string())
        .collect::<Vec<_>>()
        .join(",");
    let unknown = format!("unknown{escape}q");
    let unknown_codepoints = unknown
        .chars()
        .map(|character| (character as u32).to_string())
        .collect::<Vec<_>>()
        .join(",");
    let node_source = format!(
        "import {{ escaped, unknown }} from {:?}; const expected = String.fromCharCode({codepoints}); const expectedUnknown = String.fromCharCode({unknown_codepoints}); if (escaped !== expected || unknown !== expectedUnknown) process.exit(1)",
        dir.join("strings.mjs").to_str().unwrap()
    );
    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(node_source)
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn bundled_string_intrinsics_cover_unicode_indexing_and_reject_bad_types() {
    let dir = temp_dir("string-intrinsics");
    let source = dir.join("strings.ml");
    fs::write(
        &source,
        r#"
        use String.{concat, length, get, slice, equal}
        use Prelude.{Option, None, Some}
        let text = "A😀B"
        let result = (
          length text,
          get text 1,
          get text 4,
          slice text 1 2,
          equal text "A😀B",
          equal text "AB",
          concat text "!"
        )
        "#,
    )
    .unwrap();
    let output = dir.join("strings.mjs");
    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-o")
        .arg(&output)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result[0] !== 3n || result[1].type !== 'Some' || result[1].value !== 128512n || result[2].type !== 'None' || result[3] !== '😀' || result[4] !== true || result[5] !== false || result[6] !== 'A😀B!') process.exit(1)",
            output.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
    assert!(dir.join("String.mjs").exists());

    let invalid = dir.join("invalid_string_call.ml");
    fs::write(
        &invalid,
        r#"use String.{get}
let invalid = get "text" true
"#,
    )
    .unwrap();
    let invalid_output = dir.join("invalid.mjs");
    let failed = Command::new(tinyml())
        .arg("compile")
        .arg("-o")
        .arg(&invalid_output)
        .arg(&invalid)
        .output()
        .unwrap();
    assert!(!failed.status.success());
    assert!(!invalid_output.exists());
}

#[test]
fn emitted_string_concatenation_executes() {
    let dir = temp_dir("emit-js-strings");
    let file = dir.join("strings.ml");
    fs::write(&file, "let greeting = \"hello, \" + \"world\"\n").unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ greeting }} from {:?}; if (greeting !== 'hello, world') process.exit(1)",
            dir.join("strings.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn bundled_riotml_compiler_source_executes_nested_match_lexer() {
    let repository_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let compiler_source = repository_root.join("riotc/main.ml");
    let include_root = repository_root.join("riotc");
    let dir = temp_dir("riotml-compiler-lexer");
    let output = dir.join("compiler.mjs");
    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_root)
        .arg("-o")
        .arg(&output)
        .arg(&compiler_source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ lex }} from {:?}; import {{ readFileSync }} from 'node:fs'; let source = readFileSync({:?}, 'utf8'); let result = lex(source); if (result.type !== 'Ok') process.exit(1); let tokens = result.value; let count = 0; while (tokens.type === 'Cons') {{ count += 1; tokens = tokens.value[1]; }} if (count < 100 || tokens.type !== 'Nil' || lex('let x = \"unterminated').type !== 'Err') process.exit(1)",
            dir.join("Syntax_Lexer.mjs").to_str().unwrap(),
            compiler_source.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn riotml_source_module_parser_accepts_compiler_sources_and_rejects_bad_declarations() {
    let repository_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let include_root = repository_root.join("riotc");
    let dir = temp_dir("riotml-source-parser");
    let client_source = dir.join("parser_client.ml");
    fs::write(
        &client_source,
        "use Syntax.Parser.SourceModule.{parse_source_module}\nuse Modules.{load_source_units}\nlet main = fn () -> 0\n",
    )
    .unwrap();
    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_root)
        .arg("-o")
        .arg(dir.join("parser_client.mjs"))
        .arg(&client_source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let source_paths = riotml_sources(&include_root);
    let valid_dir = dir.join("valid");
    fs::create_dir_all(&valid_dir).unwrap();
    let valid_root = valid_dir.join("main.ml");
    fs::write(
        &valid_root,
        "use Helper.{value}\nlet main = fn () -> value\n",
    )
    .unwrap();
    fs::write(valid_dir.join("helper.ml"), "let value = 42\n").unwrap();
    let missing_member_root = valid_dir.join("missing_member.ml");
    fs::write(
        &missing_member_root,
        "use Helper.{missing_value}\nlet main = fn () -> 0\n",
    )
    .unwrap();

    let missing_root = dir.join("missing.ml");
    fs::write(
        &missing_root,
        "use Missing.{value}\nlet main = fn () -> value\n",
    )
    .unwrap();

    let cycle_dir = dir.join("cycle");
    fs::create_dir_all(&cycle_dir).unwrap();
    let cycle_root = cycle_dir.join("main.ml");
    fs::write(&cycle_root, "use Helper.{value}\nlet result = value\n").unwrap();
    fs::write(
        cycle_dir.join("helper.ml"),
        "use Main.{result}\nlet value = result\n",
    )
    .unwrap();

    let node_script = r#"
        import { readFileSync } from 'node:fs';
        const { parse_source_module } = await import(process.argv[1]);
        const { load_source_units } = await import(process.argv[2]);
        const [compilerRoot, compilerInclude, validRoot, missingRoot, cycleRoot, missingMemberRoot, ...sources] = process.argv.slice(3);

        for (const path of sources) {
            const result = parse_source_module(readFileSync(path, 'utf8'));
            if (result.type !== 'Ok') {
                console.error(path + ': ' + result.value);
                process.exit(1);
            }
        }

        const valid = parse_source_module(`use Demo.Core.{make_value, Value}
          type Option<'a> = | None | Some('a)
          type User = User { name: string, id: i32 }
          let increment x = x + 1
          let main = fn () -> 42
        `);
        if (valid.type !== 'Ok' || valid.value.imports.type !== 'Cons') process.exit(2);
        const invalid = parse_source_module('type Broken = { field i32 }');
        if (invalid.type !== 'Err') process.exit(3);

        const nil = { type: 'Nil' };
        const cons = (head, tail) => ({ type: 'Cons', value: [head, tail] });
        const compilerModules = load_source_units(compilerRoot)(cons(compilerInclude, nil));
        if (compilerModules.type !== 'Ok') {
            console.error(compilerModules.value);
            process.exit(4);
        }
        const moduleCount = (modules) => {
            let count = 0;
            while (modules.type === 'Cons') {
                count += 1;
                modules = modules.value[1];
            }
            return count;
        };
        if (moduleCount(compilerModules.value) < 10) process.exit(5);

        const validGraph = load_source_units(validRoot)(nil);
        if (validGraph.type !== 'Ok' || moduleCount(validGraph.value) !== 2) process.exit(6);
        const helperUnit = validGraph.value.value[0];
        const rootUnit = validGraph.value.value[1].value[0];
        if (!helperUnit[0].endsWith('/helper.ml') || rootUnit[0] !== validRoot) {
            console.error({ helper: helperUnit[0], root: rootUnit[0], expectedRoot: validRoot });
            process.exit(7);
        }
        if (load_source_units(missingRoot)(nil).type !== 'Err') process.exit(8);
        const missingMember = load_source_units(missingMemberRoot)(nil);
        if (missingMember.type !== 'Err' || !missingMember.value.includes('does not export')) process.exit(9);
        const cycle = load_source_units(cycleRoot)(nil);
        if (cycle.type !== 'Err' || !cycle.value.includes('cycle')) process.exit(10);
    "#;
    let mut node_command = Command::new("node");
    node_command
        .current_dir(&repository_root)
        .arg("--input-type=module")
        .arg("-e")
        .arg(node_script)
        .arg(dir.join("Syntax_Parser_SourceModule.mjs"))
        .arg(dir.join("Modules.mjs"))
        .arg(include_root.join("main.ml"))
        .arg(&include_root)
        .arg(&valid_root)
        .arg(&missing_root)
        .arg(&cycle_root)
        .arg(&missing_member_root);
    for path in source_paths {
        node_command.arg(path);
    }
    let node = node_command.output().unwrap();
    assert!(
        node.status.success(),
        "node exited {:?}: {}",
        node.status.code(),
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn riotml_compiler_emits_native_smoke_program_and_rejects_bad_types() {
    let repository_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let compiler_source = repository_root.join("riotc/main.ml");
    let include_root = repository_root.join("riotc");
    let dir = temp_dir("riotml-native-smoke");
    let unrelated_working_directory = dir.join("unrelated-working-directory");
    fs::create_dir_all(&unrelated_working_directory).unwrap();
    let compiler_js = dir.join("compiler.mjs");
    let stage0 = Command::new(tinyml())
        .arg("compile")
        .arg("-I")
        .arg(&include_root)
        .arg("-o")
        .arg(&compiler_js)
        .arg(&compiler_source)
        .output()
        .unwrap();
    assert!(
        stage0.status.success(),
        "{}",
        String::from_utf8_lossy(&stage0.stderr)
    );

    let runner = dir.join("run-compiler.mjs");
    fs::write(
        &runner,
        format!(
            "import {{ main }} from {:?}; process.exitCode = Number(main(undefined));\n",
            compiler_js.to_str().unwrap()
        ),
    )
    .unwrap();
    let native_compiler_source = dir.join("riotml-compiler.native");
    let self_compile = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&compiler_source)
        .arg("-o")
        .arg(&native_compiler_source)
        .output()
        .unwrap();
    assert!(
        self_compile.status.success(),
        "JS-hosted self-compilation failed: {}",
        String::from_utf8_lossy(&self_compile.stderr)
    );

    let native_smoke_source = dir.join("native_compiler_smoke.ml");
    fs::write(
        &native_smoke_source,
        r#"type Maybe = None | Some(i32)
let negate = fn (value: i32) -> -value
let main = fn () -> {
  let item = Some(42);
  let wrapped_add = ((2147483647: i32) + 1) == (-2147483648: i32);
  let wrapped_sub = ((-2147483648: i32) - 1) == (2147483647: i32);
  let wrapped_mul = ((65536: i32) * 65536) == 0;
  let wrapped_neg = negate (-2147483648: i32) == (-2147483648: i32);
  let wrapped_div = ((-2147483648: i32) / (-1)) == (-2147483648: i32);
  let wrapped_rem = ((-2147483648: i32) % (-1)) == 0;
  let text = "A\0B";
  let string_matches = match text { | "A\0B" -> true | _ -> false };
  match (wrapped_add, wrapped_sub, wrapped_mul, wrapped_neg, wrapped_div, wrapped_rem, string_matches, item) {
    | (true, true, true, true, true, true, true, Some(value)) -> value
    | _ -> 0
  }
}
"#,
    )
    .unwrap();
    let native_smoke_output = dir.join("native_compiler_smoke.native");
    let native_compile = Command::new(&native_compiler_source)
        .current_dir(&unrelated_working_directory)
        .arg("compile")
        .arg(&native_smoke_source)
        .arg("-o")
        .arg(&native_smoke_output)
        .output()
        .unwrap();
    assert!(
        native_compile.status.success(),
        "native compiler failed: {}",
        String::from_utf8_lossy(&native_compile.stderr)
    );
    assert_eq!(
        Command::new(&native_smoke_output).status().unwrap().code(),
        Some(42)
    );

    let native_invalid_source = dir.join("native_compiler_invalid.ml");
    fs::write(&native_invalid_source, "let main = fn () -> 1 + true\n").unwrap();
    let native_invalid_output = dir.join("native_compiler_invalid.native");
    let native_rejected = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&native_invalid_source)
        .arg("-o")
        .arg(&native_invalid_output)
        .output()
        .unwrap();
    assert!(!native_rejected.status.success());
    assert!(
        String::from_utf8_lossy(&native_rejected.stderr)
            .contains("integer arithmetic requires matching integer types")
    );
    assert!(!native_invalid_output.exists());

    let native_unknown_source = dir.join("native_compiler_unknown_name.ml");
    fs::write(&native_unknown_source, "let main = fn () -> missing_name\n").unwrap();
    let native_unknown_output = dir.join("native_compiler_unknown_name.native");
    let native_unknown = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&native_unknown_source)
        .arg("-o")
        .arg(&native_unknown_output)
        .output()
        .unwrap();
    assert!(!native_unknown.status.success());
    assert!(String::from_utf8_lossy(&native_unknown.stderr).contains("unbound value name"));
    assert!(!native_unknown_output.exists());

    let native_wrong_argument_source = dir.join("native_compiler_wrong_argument.ml");
    fs::write(
        &native_wrong_argument_source,
        "let main = fn () -> let increment = fn value -> let next = value + 1; next; increment true\n",
    )
    .unwrap();
    let native_wrong_argument_output = dir.join("native_compiler_wrong_argument.native");
    let native_wrong_argument = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&native_wrong_argument_source)
        .arg("-o")
        .arg(&native_wrong_argument_output)
        .output()
        .unwrap();
    assert!(!native_wrong_argument.status.success());
    assert!(
        String::from_utf8_lossy(&native_wrong_argument.stderr)
            .contains("function argument has an incompatible type")
    );
    assert!(!native_wrong_argument_output.exists());

    let native_wrong_top_level_argument_source =
        dir.join("native_compiler_wrong_top_level_argument.ml");
    fs::write(
        &native_wrong_top_level_argument_source,
        "let increment = fn value -> value + 1\nlet main = fn () -> increment true\n",
    )
    .unwrap();
    let native_wrong_top_level_argument_output =
        dir.join("native_compiler_wrong_top_level_argument.native");
    let native_wrong_top_level_argument = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&native_wrong_top_level_argument_source)
        .arg("-o")
        .arg(&native_wrong_top_level_argument_output)
        .output()
        .unwrap();
    assert!(!native_wrong_top_level_argument.status.success());
    assert!(
        String::from_utf8_lossy(&native_wrong_top_level_argument.stderr)
            .contains("function argument has an incompatible type")
    );
    assert!(!native_wrong_top_level_argument_output.exists());

    fs::write(
        dir.join("helper.ml"),
        "let increment = fn value -> value + 1\nlet identity = fn value -> value\ntype Maybe<'a> = | Empty | Present('a)\nlet wrap = fn value -> Present(value)\n",
    )
    .unwrap();
    let native_wrong_imported_argument_source =
        dir.join("native_compiler_wrong_imported_argument.ml");
    fs::write(
        &native_wrong_imported_argument_source,
        "use Helper.{increment}\nlet main = fn () -> increment true\n",
    )
    .unwrap();
    let native_wrong_imported_argument_output =
        dir.join("native_compiler_wrong_imported_argument.native");
    let native_wrong_imported_argument = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&native_wrong_imported_argument_source)
        .arg("-o")
        .arg(&native_wrong_imported_argument_output)
        .output()
        .unwrap();
    assert!(!native_wrong_imported_argument.status.success());
    assert!(
        String::from_utf8_lossy(&native_wrong_imported_argument.stderr)
            .contains("function argument has an incompatible type"),
        "unexpected imported-call diagnostic: {}",
        String::from_utf8_lossy(&native_wrong_imported_argument.stderr)
    );
    assert!(!native_wrong_imported_argument_output.exists());

    let local_polymorphic_source = dir.join("native_local_polymorphism.ml");
    fs::write(
        &local_polymorphic_source,
        "let main = fn () -> let identity = fn value -> value; let number = identity 42; let truth = identity true; match truth { | true -> number | false -> 0 }\n",
    )
    .unwrap();
    let local_polymorphic_output = dir.join("native_local_polymorphism.native");
    let local_polymorphic = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&local_polymorphic_source)
        .arg("-o")
        .arg(&local_polymorphic_output)
        .output()
        .unwrap();
    assert!(
        local_polymorphic.status.success(),
        "local polymorphism: {}",
        String::from_utf8_lossy(&local_polymorphic.stderr)
    );
    assert_eq!(
        Command::new(&local_polymorphic_output)
            .status()
            .unwrap()
            .code(),
        Some(42)
    );

    let imported_polymorphic_source = dir.join("native_imported_polymorphism.ml");
    fs::write(
        &imported_polymorphic_source,
        "use Helper.{identity, Maybe, wrap}\nuse Helper.{Empty, Present}\nlet main = fn () -> let number = identity 42; let truth = identity true; let boxed = wrap number; match truth { | true -> match boxed { | Present(value) -> value | Empty -> 0 } | false -> 0 }\n",
    )
    .unwrap();
    let imported_polymorphic_output = dir.join("native_imported_polymorphism.native");
    let imported_polymorphic = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&imported_polymorphic_source)
        .arg("-o")
        .arg(&imported_polymorphic_output)
        .output()
        .unwrap();
    assert!(
        imported_polymorphic.status.success(),
        "imported polymorphism and nominal type: {}",
        String::from_utf8_lossy(&imported_polymorphic.stderr)
    );
    assert_eq!(
        Command::new(&imported_polymorphic_output)
            .status()
            .unwrap()
            .code(),
        Some(42)
    );

    fs::write(dir.join("other.ml"), "let identity = fn value -> 0\n").unwrap();
    let ambiguous_import_source = dir.join("native_ambiguous_import.ml");
    fs::write(
        &ambiguous_import_source,
        "use Helper.{identity}\nuse Other.{identity}\nlet main = fn () -> identity 42\n",
    )
    .unwrap();
    let ambiguous_import_output = dir.join("native_ambiguous_import.native");
    let ambiguous_import = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&ambiguous_import_source)
        .arg("-o")
        .arg(&ambiguous_import_output)
        .output()
        .unwrap();
    assert!(!ambiguous_import.status.success());
    assert!(
        String::from_utf8_lossy(&ambiguous_import.stderr)
            .contains("ambiguous imported name `identity`"),
        "unexpected ambiguous-import diagnostic: {}",
        String::from_utf8_lossy(&ambiguous_import.stderr)
    );
    assert!(!ambiguous_import_output.exists());

    let duplicate_pattern_source = dir.join("native_duplicate_pattern_binding.ml");
    fs::write(
        &duplicate_pattern_source,
        "let main = fn () -> let (same, same) = (1, true); 0\n",
    )
    .unwrap();
    let duplicate_pattern_output = dir.join("native_duplicate_pattern_binding.native");
    let duplicate_pattern = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&duplicate_pattern_source)
        .arg("-o")
        .arg(&duplicate_pattern_output)
        .output()
        .unwrap();
    assert!(!duplicate_pattern.status.success());
    assert!(
        String::from_utf8_lossy(&duplicate_pattern.stderr)
            .contains("duplicate pattern binding `same`"),
        "unexpected duplicate-pattern diagnostic: {}",
        String::from_utf8_lossy(&duplicate_pattern.stderr)
    );
    assert!(!duplicate_pattern_output.exists());

    let duplicate_value_source = dir.join("native_duplicate_value_declaration.ml");
    fs::write(
        &duplicate_value_source,
        "let repeated = 1\nlet repeated = true\nlet main = fn () -> 0\n",
    )
    .unwrap();
    let duplicate_value_output = dir.join("native_duplicate_value_declaration.native");
    let duplicate_value = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&duplicate_value_source)
        .arg("-o")
        .arg(&duplicate_value_output)
        .output()
        .unwrap();
    assert!(!duplicate_value.status.success());
    assert!(
        String::from_utf8_lossy(&duplicate_value.stderr)
            .contains("duplicate value declaration `repeated`"),
        "unexpected duplicate-value diagnostic: {}",
        String::from_utf8_lossy(&duplicate_value.stderr)
    );
    assert!(!duplicate_value_output.exists());

    let conflicting_constructor_source = dir.join("native_conflicting_constructor.ml");
    fs::write(
        &conflicting_constructor_source,
        "use Helper.{Present}\ntype Other<'a> = | Present('a)\nlet main = fn () -> 0\n",
    )
    .unwrap();
    let conflicting_constructor_output = dir.join("native_conflicting_constructor.native");
    let conflicting_constructor = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&conflicting_constructor_source)
        .arg("-o")
        .arg(&conflicting_constructor_output)
        .output()
        .unwrap();
    assert!(!conflicting_constructor.status.success());
    assert!(
        String::from_utf8_lossy(&conflicting_constructor.stderr)
            .contains("imported name conflicts with local declaration `Present`"),
        "unexpected constructor-conflict diagnostic: {}",
        String::from_utf8_lossy(&conflicting_constructor.stderr)
    );
    assert!(!conflicting_constructor_output.exists());

    let mismatched_nominal_source = dir.join("native_mismatched_nominal.ml");
    fs::write(
        &mismatched_nominal_source,
        "use Helper.{Empty, Present}\ntype Other<'a> = | Other('a)\nlet consume = fn value -> match value { | Present(item) -> item | Empty -> 0 }\nlet main = fn () -> consume (Other(42))\n",
    )
    .unwrap();
    let mismatched_nominal_output = dir.join("native_mismatched_nominal.native");
    let mismatched_nominal = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&mismatched_nominal_source)
        .arg("-o")
        .arg(&mismatched_nominal_output)
        .output()
        .unwrap();
    assert!(!mismatched_nominal.status.success());
    assert!(
        String::from_utf8_lossy(&mismatched_nominal.stderr).contains("type mismatch"),
        "unexpected nominal-type diagnostic: {}",
        String::from_utf8_lossy(&mismatched_nominal.stderr)
    );
    assert!(!mismatched_nominal_output.exists());

    let native_unknown_type_source = dir.join("native_compiler_unknown_type.ml");
    fs::write(
        &native_unknown_type_source,
        "type Broken = MissingType\nlet main = fn () -> 0\n",
    )
    .unwrap();
    let native_unknown_type_output = dir.join("native_compiler_unknown_type.native");
    let native_unknown_type = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&native_unknown_type_source)
        .arg("-o")
        .arg(&native_unknown_type_output)
        .output()
        .unwrap();
    assert!(!native_unknown_type.status.success());
    assert!(String::from_utf8_lossy(&native_unknown_type.stderr).contains("unbound type name"));
    assert!(!native_unknown_type_output.exists());

    let native_unbound_type_variable_source = dir.join("native_compiler_unbound_type_variable.ml");
    fs::write(
        &native_unbound_type_variable_source,
        "type Broken = 'missing\nlet main = fn () -> 0\n",
    )
    .unwrap();
    let native_unbound_type_variable_output =
        dir.join("native_compiler_unbound_type_variable.native");
    let native_unbound_type_variable = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&native_unbound_type_variable_source)
        .arg("-o")
        .arg(&native_unbound_type_variable_output)
        .output()
        .unwrap();
    assert!(!native_unbound_type_variable.status.success());
    assert!(
        String::from_utf8_lossy(&native_unbound_type_variable.stderr)
            .contains("unbound type variable")
    );
    assert!(!native_unbound_type_variable_output.exists());

    let native_mismatched_pattern_source = dir.join("native_compiler_mismatched_pattern.ml");
    fs::write(
        &native_mismatched_pattern_source,
        "let main = fn () -> match 1 { | true -> 42 | false -> 0 }\n",
    )
    .unwrap();
    let native_mismatched_pattern_output = dir.join("native_compiler_mismatched_pattern.native");
    let native_mismatched_pattern = Command::new(&native_compiler_source)
        .current_dir(&repository_root)
        .arg("compile")
        .arg(&native_mismatched_pattern_source)
        .arg("-o")
        .arg(&native_mismatched_pattern_output)
        .output()
        .unwrap();
    assert!(!native_mismatched_pattern.status.success());
    assert!(
        String::from_utf8_lossy(&native_mismatched_pattern.stderr)
            .contains("match pattern does not match the scrutinee type")
    );
    assert!(!native_mismatched_pattern_output.exists());

    let valid_source = dir.join("smoke.ml");
    fs::write(&valid_source, "let main = fn () -> 42\n").unwrap();
    let native_compiler = dir.join("riotml-compiler.native");
    let hosted = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&valid_source)
        .arg("-o")
        .arg(&native_compiler)
        .output()
        .unwrap();
    assert!(
        hosted.status.success(),
        "{}",
        String::from_utf8_lossy(&hosted.stderr)
    );
    let program = Command::new(&native_compiler).status().unwrap();
    assert_eq!(program.code(), Some(42));

    let arithmetic_source = dir.join("arithmetic.ml");
    fs::write(&arithmetic_source, "let main = fn () -> 6 + 6 * 6\n").unwrap();
    let arithmetic_output = dir.join("arithmetic.native");
    let arithmetic_compile = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&arithmetic_source)
        .arg("-o")
        .arg(&arithmetic_output)
        .output()
        .unwrap();
    assert!(
        arithmetic_compile.status.success(),
        "{}",
        String::from_utf8_lossy(&arithmetic_compile.stderr)
    );
    assert_eq!(
        Command::new(&arithmetic_output).status().unwrap().code(),
        Some(42)
    );

    let division_source = dir.join("division.ml");
    fs::write(&division_source, "let main = fn () -> -7 / 2\n").unwrap();
    let division_output = dir.join("division.native");
    let division_compile = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&division_source)
        .arg("-o")
        .arg(&division_output)
        .output()
        .unwrap();
    assert!(
        division_compile.status.success(),
        "{}",
        String::from_utf8_lossy(&division_compile.stderr)
    );
    assert_eq!(
        Command::new(&division_output).status().unwrap().code(),
        Some(253)
    );

    let match_source = dir.join("boolean_match.ml");
    fs::write(
        &match_source,
        "let main = fn () -> match (3 < 4) { | true -> 42 | false -> 1 }\n",
    )
    .unwrap();
    let match_output = dir.join("boolean_match.native");
    let match_compile = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&match_source)
        .arg("-o")
        .arg(&match_output)
        .output()
        .unwrap();
    assert!(
        match_compile.status.success(),
        "{}",
        String::from_utf8_lossy(&match_compile.stderr)
    );
    assert_eq!(
        Command::new(&match_output).status().unwrap().code(),
        Some(42)
    );

    let dynamic_cases = [
        (
            "imported_helper",
            "use Helper.{increment}\nlet main = fn () -> increment 41\n",
        ),
        (
            "top_level_helper",
            "let increment = fn value -> value + 1\nlet main = fn () -> increment 41\n",
        ),
        (
            "closure_and_let",
            "let main = fn () -> let increment = fn value -> let next = value + 1; next; let result = increment 41; result\n",
        ),
        (
            "tuple_match",
            "let main = fn () -> match (40, 2) { | (left, right) -> left + right }\n",
        ),
        (
            "constructor_match",
            "type Option<'a> = | None | Some('a)\nlet main = fn () -> match Some(42) { | Some(value) -> value | None -> 0 }\n",
        ),
        (
            "record_match",
            "type Pair = Pair { left: i32, right: i32 }\nlet main = fn () -> match Pair { left: 40, right: 2 } { | Pair { left, right: _ } -> left + 2 }\n",
        ),
    ];
    for (name, source) in dynamic_cases {
        let source_path = dir.join(format!("{name}.ml"));
        let output_path = dir.join(format!("{name}.native"));
        fs::write(&source_path, source).unwrap();
        let compile = Command::new("node")
            .current_dir(&repository_root)
            .arg(&runner)
            .arg("compile")
            .arg(&source_path)
            .arg("-o")
            .arg(&output_path)
            .output()
            .unwrap();
        assert!(
            compile.status.success(),
            "{name}: {}",
            String::from_utf8_lossy(&compile.stderr)
        );
        assert_eq!(
            Command::new(&output_path).status().unwrap().code(),
            Some(42),
            "{name}"
        );
    }

    let non_exhaustive_source = dir.join("non_exhaustive_match.ml");
    fs::write(
        &non_exhaustive_source,
        "let main = fn () -> match true { | true -> 42 }\n",
    )
    .unwrap();
    let non_exhaustive_output = dir.join("non_exhaustive.native");
    let non_exhaustive = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&non_exhaustive_source)
        .arg("-o")
        .arg(&non_exhaustive_output)
        .output()
        .unwrap();
    assert!(!non_exhaustive.status.success());
    assert!(String::from_utf8_lossy(&non_exhaustive.stderr).contains("missing a `false` arm"));
    assert!(!non_exhaustive_output.exists());

    let invalid_source = dir.join("ill_typed.ml");
    fs::write(&invalid_source, "let main = fn () -> 1 + true\n").unwrap();
    let invalid_output = dir.join("must_not_exist.native");
    let rejected = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&invalid_source)
        .arg("-o")
        .arg(&invalid_output)
        .output()
        .unwrap();
    assert!(!rejected.status.success());
    assert!(String::from_utf8_lossy(&rejected.stderr).contains("typechecking"));
    assert!(!invalid_output.exists());

    let dynamic_invalid_source = dir.join("dynamic_ill_typed.ml");
    fs::write(
        &dynamic_invalid_source,
        "let main = fn () -> let broken = fn value -> value + true; 0\n",
    )
    .unwrap();
    let dynamic_invalid_output = dir.join("dynamic_must_not_exist.native");
    let dynamic_rejected = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&dynamic_invalid_source)
        .arg("-o")
        .arg(&dynamic_invalid_output)
        .output()
        .unwrap();
    assert!(!dynamic_rejected.status.success());
    assert!(String::from_utf8_lossy(&dynamic_rejected.stderr).contains("typechecking"));
    assert!(!dynamic_invalid_output.exists());

    let out_of_range_source = dir.join("out_of_range.ml");
    fs::write(&out_of_range_source, "let main = fn () -> 2147483648\n").unwrap();
    let out_of_range_output = dir.join("out_of_range.native");
    let out_of_range = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&out_of_range_source)
        .arg("-o")
        .arg(&out_of_range_output)
        .output()
        .unwrap();
    assert!(!out_of_range.status.success());
    assert!(String::from_utf8_lossy(&out_of_range.stderr).contains("outside i32 range"));
    assert!(!out_of_range_output.exists());

    let malformed_source = dir.join("unterminated_string.ml");
    fs::write(&malformed_source, "let main = fn () -> \"unterminated\n").unwrap();
    let malformed_output = dir.join("malformed.native");
    let malformed = Command::new("node")
        .current_dir(&repository_root)
        .arg(&runner)
        .arg("compile")
        .arg(&malformed_source)
        .arg("-o")
        .arg(&malformed_output)
        .output()
        .unwrap();
    assert!(!malformed.status.success());
    let diagnostic = String::from_utf8_lossy(&malformed.stderr);
    assert!(diagnostic.contains("unterminated string literal"));
    assert!(diagnostic.contains("codepoints"));
    assert!(!malformed_output.exists());
}

#[test]
fn compile_js_writes_module_artifact() {
    let dir = temp_dir("compile-js");
    let file = dir.join("prelude.ml");
    fs::write(&file, "let answer = 42\n").unwrap();

    let output = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("js")
        .arg("-o")
        .arg(dir.join("prelude.mjs"))
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(dir.join("prelude.mjs").exists());
    assert!(!dir.join("prelude.tmi").exists());
}

#[test]
fn compile_js_resolves_dependency_source_modules() {
    let dir = temp_dir("compile-js-import");
    let prelude = dir.join("prelude.ml");
    let main = dir.join("Main.ml");
    fs::write(&prelude, "let answer = 41\nlet identity = fn x -> x\n").unwrap();
    fs::write(
        &main,
        "use Prelude.{answer, identity}\nlet result = identity (answer + 1)\n",
    )
    .unwrap();

    for (source, output) in [(&prelude, "Prelude.mjs"), (&main, "Main.mjs")] {
        let mut command = Command::new(tinyml());
        command
            .arg("compile")
            .arg("--target")
            .arg("js")
            .arg("-o")
            .arg(dir.join(output));
        if source == &main {
            command.arg("-I").arg(&dir);
        }
        let result = command.arg(source).output().unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
    }

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result !== 42n) process.exit(1)",
            dir.join("Main.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn compiled_reserved_identifiers_import_with_collision_free_js_names() {
    let dir = temp_dir("reserved-js-imports");
    let prelude = dir.join("prelude.ml");
    let main = dir.join("Main.ml");
    fs::write(&prelude, "let default = 40\nlet _default = 2\n").unwrap();
    fs::write(
        &main,
        "use Prelude.{default, _default}\nlet result = default + _default\n",
    )
    .unwrap();

    for (source, output) in [(&prelude, "Prelude.mjs"), (&main, "Main.mjs")] {
        let mut command = Command::new(tinyml());
        command.arg("compile").arg("-o").arg(dir.join(output));
        if source == &main {
            command.arg("-I").arg(&dir);
        }
        let result = command.arg(source).output().unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
    }

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result !== 42n) process.exit(1)",
            dir.join("Main.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn compile_resolves_source_modules_and_preserves_imported_types() {
    let dir = temp_dir("source-modules");
    let invocation_dir = temp_dir("source-modules-cwd");
    fs::write(
        dir.join("syntax.ml"),
        r#"
        type Tree<'a> =
          | Leaf('a)
          | Branch(Tree<'a>, Tree<'a>)

        let identity = fn value -> value
        "#,
    )
    .unwrap();
    fs::write(
        dir.join("types.ml"),
        "type User = User { name: string, age: i32 }\n",
    )
    .unwrap();
    let main = dir.join("main.ml");
    fs::write(
        &main,
        r#"
        use Syntax.{Tree, Leaf, Branch, identity}
        use Types.{User}

        let polymorphic = (identity 41, identity true)
        let leaf_count = fn (tree: Tree<i32>) -> match tree {
          | Leaf(_) -> 1
          | Branch(left, right) -> leaf_count left + leaf_count right
        }
        let get_name = fn (User { name, age: _ }) -> name
        "#,
    )
    .unwrap();

    let check = Command::new(tinyml())
        .current_dir(&invocation_dir)
        .arg("check")
        .arg(&main)
        .output()
        .unwrap();
    assert!(
        check.status.success(),
        "{}",
        String::from_utf8_lossy(&check.stderr)
    );

    let run_source = dir.join("run.ml");
    fs::write(
        &run_source,
        "use Syntax.{identity}\nlet polymorphic = (identity 41, identity true)\n",
    )
    .unwrap();
    let output = dir.join("run.mjs");
    let result = Command::new(tinyml())
        .current_dir(&invocation_dir)
        .arg("compile")
        .arg("-o")
        .arg(&output)
        .arg(&run_source)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert!(dir.join("Syntax.mjs").exists());

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ polymorphic }} from {:?}; if (polymorphic[0] !== 41n || polymorphic[1] !== true) process.exit(1)",
            output.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn compiled_modules_run_with_adt_tuple_and_record_patterns() {
    let dir = temp_dir("module-runtime-patterns");
    let invocation_dir = temp_dir("module-runtime-patterns-cwd");
    fs::write(
        dir.join("syntax.ml"),
        r#"
        type Tree<'value> = | Leaf('value) | Fork(Tree<'value>, Tree<'value>)
        type Message =
          | Message { marker: i32 }
          | User { name: string, value: i32 }
          | Done
        let identity = fn value -> value
        "#,
    )
    .unwrap();
    let source = dir.join("main.ml");
    fs::write(
        &source,
        r#"
        use Syntax.{Tree, Leaf, Fork, identity}
        use Syntax.{Message, User, Done}
        type Person = Person { name: string, age: i32 }

        let depth = fn tree -> match tree {
          | Leaf(_) -> 1
          | Fork(left, right) -> depth left + depth right
        }
        let tuple_value = fn pair -> match pair {
          | (0, value) -> value
          | (_, value) -> value + 1
        }
        let classify_message = fn message -> match message {
          | Message { marker } -> marker
          | User { name: "Ada", value } -> value
          | Done -> 0
          | _ -> -1
        }
        let get_age = fn user -> match user {
          | Person { name: "Ada", age } -> age
          | _ -> 0
        }
        let result = (
          identity (depth (Fork(Leaf(1), Fork(Leaf(2), Leaf(3))))),
          tuple_value((0, 41)),
          get_age(Person { name: "Ada", age: 41 }),
          tuple_value((1, 41)),
          classify_message(User { name: "Ada", value: 43 }),
          classify_message(Message { marker: 44 })
        )
        "#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .current_dir(&invocation_dir)
        .arg("compile")
        .arg("-o")
        .arg(dir.join("main.mjs"))
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result[0] !== 3n || result[1] !== 41n || result[2] !== 41n || result[3] !== 42n || result[4] !== 43n || result[5] !== 44n) process.exit(1)",
            dir.join("main.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
    let generated = fs::read_to_string(dir.join("main.mjs")).unwrap();
    assert!(generated.contains("import { identity } from \"./Syntax.mjs\";"));
    assert!(!generated.contains("import { Tree"));
}

#[test]
fn bundled_source_library_resolves_outside_the_repository() {
    let dir = temp_dir("bundled-library-source");
    let unrelated_cwd = temp_dir("bundled-library-cwd");
    let source = dir.join("library_client.ml");
    fs::write(
        &source,
        r#"
        use Prelude.{List, Nil, Cons, Option, Some, Result, Ok}
        use List.{length}
        use Option.{map}
        use Result.{unwrap_or}
        use String.{concat}

        let size = length (Cons(1, Cons(2, Nil)))
        let mapped = map (fn value -> value + 1) (Some(41))
        let unwrapped = unwrap_or 0 (Ok(42))
        let message = concat "hello" " world"
        "#,
    )
    .unwrap();

    let result = Command::new(tinyml())
        .current_dir(unrelated_cwd)
        .arg("check")
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&result.stdout), "ok\n");
}

#[test]
fn only_selected_imports_are_available_and_missing_members_fail() {
    let dir = temp_dir("selected-imports");
    fs::write(
        dir.join("values.ml"),
        "let visible = 1\nlet hidden = true\n",
    )
    .unwrap();
    let client = dir.join("client.ml");
    fs::write(
        &client,
        "use Values.{visible}\nlet result = (visible, hidden)\n",
    )
    .unwrap();

    let result = Command::new(tinyml())
        .arg("check")
        .arg(&client)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(String::from_utf8_lossy(&result.stderr).contains("unknown value `hidden`"));

    fs::write(&client, "use Values.{missing}\nlet result = 1\n").unwrap();
    let result = Command::new(tinyml())
        .arg("check")
        .arg(&client)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(String::from_utf8_lossy(&result.stderr).contains("does not export `missing`"));

    fs::write(dir.join("left.ml"), "let shared = 1\n").unwrap();
    fs::write(dir.join("right.ml"), "let shared = 2\n").unwrap();
    fs::write(
        &client,
        "use Left.{shared}\nuse Right.{shared}\nlet result = shared\n",
    )
    .unwrap();
    let result = Command::new(tinyml())
        .arg("check")
        .arg(&client)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(String::from_utf8_lossy(&result.stderr).contains("conflicts with another declaration"));
}

#[test]
fn imported_type_metadata_tracks_unselected_variant_constructors() {
    let dir = temp_dir("imported-type-metadata");
    fs::write(
        dir.join("shape.ml"),
        "type Shape = | Circle(i32) | Square(i32)\n",
    )
    .unwrap();
    let client = dir.join("client.ml");
    fs::write(
        &client,
        "use Shape.{Shape, Circle}\nlet classify = fn (shape: Shape) -> match shape { | Circle(_) -> 1 }\n",
    )
    .unwrap();

    let result = Command::new(tinyml())
        .arg("check")
        .arg(&client)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(
        String::from_utf8_lossy(&result.stderr).contains("match is missing constructor `Square`")
    );
}

#[test]
fn compile_rejects_type_errors_before_writing_artifacts() {
    let dir = temp_dir("compile-type-error");
    let file = dir.join("invalid.ml");
    let output_path = dir.join("invalid.mjs");
    fs::write(&file, "let invalid = (true: i32)\n").unwrap();

    let output = Command::new(tinyml())
        .arg("compile")
        .arg("-o")
        .arg(&output_path)
        .arg(&file)
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("typecheck failed"));
    assert!(
        !output_path.exists(),
        "type errors must be rejected before JS emission"
    );
}

#[test]
fn host_intrinsics_run_in_node_and_roundtrip_arguments_files_and_processes() {
    let dir = temp_dir("host-intrinsics");
    let invocation_dir = temp_dir("host-intrinsics-cwd");
    let input = dir.join("input.txt");
    let output = dir.join("output.txt");
    fs::write(&input, "hello from Host\\n").unwrap();
    let source = dir.join("host_client.ml");
    fs::write(
        &source,
        r#"
        use Host.{args, read_file, write_file, write_stdout, write_stderr, run_process}
        use Prelude.{List, Nil, Cons, Result, Ok, Err}

        let first = fn values -> match values { | Nil -> "" | Cons(value, _) -> value }
        let second = fn values -> match values { | Cons(_, Cons(value, _)) -> value | _ -> "" }
        let third = fn values -> match values { | Cons(_, Cons(_, Cons(value, _))) -> value | _ -> "" }
        let arguments = args ()
        let input_path = first arguments
        let output_path = second arguments
        let counter_path = third arguments
        let contents = match (read_file input_path) {
          | Ok(text) -> text
          | Err(_) -> "read failed"
        }
        let write_result = write_file output_path contents
        let child_result = run_process "node" (Cons("-e", Cons("process.exit(7)", Nil)))
        let child_status = match child_result { | Ok(status) -> status | Err(_) -> -1 }
        let counted = match (run_process "node" (Cons("-e", Cons("const fs = require('node:fs'); const path = process.argv[1]; if (fs.existsSync(path)) process.exit(2); fs.writeFileSync(path, '1'); process.exit(1)", Cons(counter_path, Nil))))) {
          | Ok(1) -> 1
          | Ok(2) -> 2
          | Ok(_) -> 3
          | Err(_) -> 4
        }
        let wrote_ok = match write_result { | Ok(_) -> true | Err(_) -> false }
        let child_report = match (child_status == 7) { | true -> "child-ok;" | false -> "child-bad;" }
        let count_report = match (counted == 1) { | true -> "once;" | false -> "multiple;" }
        let write_report = match wrote_ok { | true -> "write-ok" | false -> "write-bad" }
        let output = write_stdout (contents + child_report + count_report + write_report)
        let diagnostic = write_stderr "host diagnostic\\n"
        "#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .current_dir(&invocation_dir)
        .arg("compile")
        .arg("-o")
        .arg(dir.join("host_client.mjs"))
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let execution = Command::new("node")
        .arg(dir.join("host_client.mjs"))
        .arg(&input)
        .arg(&output)
        .arg(dir.join("counter.txt"))
        .output()
        .unwrap();
    assert!(
        execution.status.success(),
        "{}",
        String::from_utf8_lossy(&execution.stderr)
    );
    assert_eq!(fs::read_to_string(output).unwrap(), "hello from Host\\n");
    assert_eq!(
        String::from_utf8_lossy(&execution.stdout),
        "hello from Host\\nchild-ok;once;write-ok"
    );
    assert_eq!(
        String::from_utf8_lossy(&execution.stderr),
        "host diagnostic\\n"
    );
    assert!(dir.join("Host.mjs").exists());
    assert_eq!(fs::read_to_string(dir.join("counter.txt")).unwrap(), "1");
}

#[test]
fn host_io_and_process_failures_return_errors() {
    let dir = temp_dir("host-errors");
    let source = dir.join("host_errors.ml");
    let invalid_utf8_path = dir.join("invalid-utf8.txt");
    fs::write(&invalid_utf8_path, [0xff, 0xfe]).unwrap();
    fs::write(
        &source,
        format!(
            r#"
        use Host.{{read_file, write_file, run_process}}
        use Prelude.{{List, Nil, Result, Ok, Err}}

        let missing = read_file "missing-tinyml-host-file"
        let invalid_utf8 = read_file {:?}
        let invalid_write = write_file "" "contents"
        let missing_process = run_process "tinyml-command-does-not-exist" Nil
        "#,
            invalid_utf8_path.to_str().unwrap()
        ),
    )
    .unwrap();
    let output = dir.join("host_errors.mjs");
    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-o")
        .arg(&output)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ missing, invalid_utf8, invalid_write, missing_process }} from {:?}; if (missing.type !== 'Err' || invalid_utf8.type !== 'Err' || invalid_write.type !== 'Err' || missing_process.type !== 'Err') process.exit(1)",
            output.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn compile_c_emits_status_42_executable_without_javascript() {
    let dir = temp_dir("compile-c-status-42");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        "let main = fn () -> { let base = 20; base + 22 }\n",
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .env("PATH", &dir)
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let generated = fs::read_to_string(&generated_c).unwrap();
    assert!(generated.contains("int main(int argc, char **argv)"));
    assert!(generated.contains("rt_environment_get(module_environment_0, \"main\")"));
    assert!(!generated.contains("export {"));
    assert!(!generated.contains("node"));

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_supports_imported_polymorphic_closures_and_matches() {
    let dir = temp_dir("compile-c-imported-closure");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        r#"use Std.List.{map}
use Prelude.{List, Cons, Nil}
let main = fn () -> match (map (fn x -> x + 22) (Cons(20, Nil))) { | Cons(result, Nil) -> result | _ -> 0 }
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_preserves_i32_wraparound() {
    let dir = temp_dir("compile-c-i32-wrap");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        r#"let main = fn () -> match (
  (((2147483647: i32) + 1) == (-2147483648: i32)),
  (((-2147483648: i32) - 1) == (2147483647: i32)),
  (((65536: i32) * 65536) == 0),
  (((-2147483648: i32) / (-1)) == (-2147483648: i32)),
  (((-2147483648: i32) % (-1)) == 0)
) {
  | (true, true, true, true, true) -> 42
  | _ -> 0
}
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_matches_boolean_literals() {
    let dir = temp_dir("compile-c-boolean-match");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        "let main = fn () -> match true { | false -> 0 | true -> 42 }\n",
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_supports_tuple_patterns() {
    let dir = temp_dir("compile-c-tuple-pattern");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        "let main = fn () -> match (40, 2) { | (left, right) -> left + right }\n",
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    assert!(
        !String::from_utf8_lossy(&clang.stderr).contains("return-stack-address"),
        "Clang reported a dangling aggregate argument: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_supports_record_field_patterns() {
    let dir = temp_dir("compile-c-record-pattern");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        "type Pair = Pair { left: i32, right: i32 }\nlet main = fn () -> match (Pair { left: 40, right: 2 }) { | Pair { left, right: _ } -> left + 2 | _ -> 0 }\n",
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_module_graph_output_is_deterministic() {
    let dir = temp_dir("compile-c-deterministic");
    let source = dir.join("main.ml");
    let first = dir.join("first.c");
    let second = dir.join("second.c");
    fs::write(
        &source,
        r#"use Std.List.{map}
use Prelude.{Cons, Nil}
let main = fn () -> match (map (fn x -> x + 22) (Cons(20, Nil))) { | Cons(result, Nil) -> result | _ -> 0 }
"#,
    )
    .unwrap();

    for output in [&first, &second] {
        let compile = Command::new(tinyml())
            .arg("compile")
            .arg("--target")
            .arg("c")
            .arg(&source)
            .arg("-o")
            .arg(output)
            .output()
            .unwrap();
        assert!(
            compile.status.success(),
            "compiler stderr: {}",
            String::from_utf8_lossy(&compile.stderr)
        );
    }
    assert_eq!(fs::read(first).unwrap(), fs::read(second).unwrap());
}

#[test]
fn compile_c_first_bootstraps_riotc_without_javascript() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let riotc_dir = repo.join("riotc");
    let source = riotc_dir.join("main.ml");
    let dir = temp_dir("compile-c-first-bootstrap");
    let first_c = dir.join("riotml-compiler-first.c");
    let second_c = dir.join("riotml-compiler-second.c");
    let bootstrap_compiler = dir.join("riotml-compiler");
    let no_node_path = "/usr/bin:/bin";

    for output in [&first_c, &second_c] {
        let compile = Command::new(tinyml())
            .current_dir(repo)
            .env("PATH", no_node_path)
            .arg("compile")
            .arg("--target")
            .arg("c")
            .arg("-I")
            .arg("riotc")
            .arg(&source)
            .arg("-o")
            .arg(output)
            .output()
            .unwrap();
        assert!(
            compile.status.success(),
            "Stage 0 C emission failed: {}",
            String::from_utf8_lossy(&compile.stderr)
        );
    }
    assert_eq!(
        fs::read(&first_c).unwrap(),
        fs::read(&second_c).unwrap(),
        "Riotc Stage 0 C output was not deterministic"
    );

    let clang = Command::new("clang")
        .current_dir(repo)
        .env("PATH", no_node_path)
        .arg("-O2")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&riotc_dir)
        .arg(&first_c)
        .arg(riotc_dir.join("runtime.c"))
        .arg("-o")
        .arg(&bootstrap_compiler)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "Clang failed to build the C-hosted compiler: {}",
        String::from_utf8_lossy(&clang.stderr)
    );

    let smoke_source = dir.join("smoke.ml");
    let smoke_executable = dir.join("smoke");
    fs::write(
        &smoke_source,
        r#"use String.{concat, length, get, slice, equal}
use Prelude.{Some}
let main = fn () -> {
  let text = "A\0B";
  let joined = concat "A\0" "B";
  match (length text, get text 1, slice text 1 2, equal text joined) {
    | (3, Some(0), "\0", true) -> 42
    | _ -> 0
  }
}
"#,
    )
    .unwrap();
    let runtime_directory = riotc_dir.to_str().unwrap();
    let compile_smoke = Command::new(&bootstrap_compiler)
        .current_dir(repo)
        .env("PATH", no_node_path)
        .env("RIOTML_RUNTIME_DIR", runtime_directory)
        .arg("compile")
        .arg(&smoke_source)
        .arg("-o")
        .arg(&smoke_executable)
        .output()
        .unwrap();
    assert!(
        compile_smoke.status.success(),
        "C-hosted compiler failed to compile the smoke program: {}",
        String::from_utf8_lossy(&compile_smoke.stderr)
    );
    let smoke = Command::new(&smoke_executable)
        .env("PATH", no_node_path)
        .output()
        .unwrap();
    assert_eq!(smoke.status.code(), Some(42));

    let invalid_source = dir.join("invalid.ml");
    let invalid_executable = dir.join("invalid");
    let invalid_c = dir.join("invalid.c");
    fs::write(&invalid_source, "let main = fn () -> 1 + true\n").unwrap();
    let invalid = Command::new(&bootstrap_compiler)
        .current_dir(repo)
        .env("PATH", no_node_path)
        .env("RIOTML_RUNTIME_DIR", runtime_directory)
        .arg("compile")
        .arg(&invalid_source)
        .arg("-o")
        .arg(&invalid_executable)
        .output()
        .unwrap();
    assert!(!invalid.status.success());
    assert!(
        String::from_utf8_lossy(&invalid.stderr).contains("typechecking"),
        "unexpected invalid-source diagnostic: {}",
        String::from_utf8_lossy(&invalid.stderr)
    );
    assert!(!invalid_executable.exists());
    assert!(!invalid_c.exists());
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[test]
fn c_hosted_bootstrap_compiles_native_direct_call_graph_without_javascript() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let riotc_dir = repo.join("riotc");
    let compiler_source = riotc_dir.join("main.ml");
    let dir = temp_dir("c-hosted-native-direct-graph");
    let generated_c = dir.join("riotml-compiler.c");
    let bootstrap_compiler = dir.join("riotml-compiler");
    let no_node_path = "/usr/bin:/bin";

    let stage0 = Command::new(tinyml())
        .current_dir(repo)
        .env("PATH", no_node_path)
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg("-I")
        .arg("riotc")
        .arg(&compiler_source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        stage0.status.success(),
        "Stage 0 C emission failed: {}",
        String::from_utf8_lossy(&stage0.stderr)
    );
    let clang = Command::new("clang")
        .current_dir(repo)
        .env("PATH", no_node_path)
        .arg("-O2")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&riotc_dir)
        .arg(&generated_c)
        .arg(riotc_dir.join("runtime.c"))
        .arg("-o")
        .arg(&bootstrap_compiler)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "Clang failed to build the C-hosted compiler: {}",
        String::from_utf8_lossy(&clang.stderr)
    );

    let direct_source = dir.join("direct-graph.ml");
    fs::write(
        &direct_source,
        "let callee = fn (value: i32) -> value + 1\nlet main = fn () -> callee 41\n",
    )
    .unwrap();
    let direct_executable = dir.join("direct-graph.native");
    let runtime_directory = riotc_dir.to_str().unwrap();
    let direct_compile = Command::new(&bootstrap_compiler)
        .current_dir(repo)
        .env("PATH", no_node_path)
        .env("RIOTML_RUNTIME_DIR", runtime_directory)
        .arg("aarch64-compile-direct-graph")
        .arg(&direct_source)
        .arg("main")
        .arg("-o")
        .arg(&direct_executable)
        .output()
        .unwrap();
    assert!(
        direct_compile.status.success(),
        "C-hosted compiler failed to produce owned native code: {}",
        String::from_utf8_lossy(&direct_compile.stderr)
    );
    let native_run = Command::new(&direct_executable)
        .env("PATH", no_node_path)
        .output()
        .unwrap();
    assert_eq!(native_run.status.code(), Some(42));

    let recursive_source = dir.join("recursive-direct-graph.ml");
    fs::write(
        &recursive_source,
        "let countdown = fn (value: i32) -> match value {\n  | 0 -> 42\n  | _ -> countdown (value - 1)\n}\nlet main = fn () -> countdown 10\n",
    )
    .unwrap();
    let recursive_executable = dir.join("recursive-direct-graph.native");
    let recursive_compile = Command::new(&bootstrap_compiler)
        .current_dir(repo)
        .env("PATH", no_node_path)
        .env("RIOTML_RUNTIME_DIR", runtime_directory)
        .arg("aarch64-compile-direct-graph")
        .arg(&recursive_source)
        .arg("main")
        .arg("-o")
        .arg(&recursive_executable)
        .output()
        .unwrap();
    assert!(
        recursive_compile.status.success(),
        "C-hosted compiler failed to produce recursive native code: {}",
        String::from_utf8_lossy(&recursive_compile.stderr)
    );
    let recursive_run = Command::new(&recursive_executable)
        .env("PATH", no_node_path)
        .output()
        .unwrap();
    assert_eq!(recursive_run.status.code(), Some(42));

    let invalid_source = dir.join("invalid-native.ml");
    let invalid_executable = dir.join("invalid-native");
    let invalid_assembly = dir.join("invalid-native.aarch64.s");
    fs::write(&invalid_source, "let main = fn () -> true + 1\n").unwrap();
    fs::write(&invalid_executable, "stale executable").unwrap();
    fs::write(&invalid_assembly, "stale assembly").unwrap();
    let invalid_compile = Command::new(&bootstrap_compiler)
        .current_dir(repo)
        .env("PATH", no_node_path)
        .env("RIOTML_RUNTIME_DIR", runtime_directory)
        .arg("aarch64-compile-direct-graph")
        .arg(&invalid_source)
        .arg("main")
        .arg("-o")
        .arg(&invalid_executable)
        .output()
        .unwrap();
    assert!(!invalid_compile.status.success());
    assert!(String::from_utf8_lossy(&invalid_compile.stderr).contains("typechecking"));
    assert!(!invalid_executable.exists());
    assert!(!invalid_assembly.exists());
}

#[test]
fn compile_c_uses_the_host_runtime_without_javascript() {
    let dir = temp_dir("compile-c-host-args");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        r#"use Host.{args}
use Prelude.{Cons, Nil}
let main = fn () -> match (args ()) { | Cons(_, _) -> 42 | Nil -> 0 }
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).arg("present").output().unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_preserves_host_file_and_process_behavior() {
    let dir = temp_dir("compile-c-host-file-process");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    let output_file = dir.join("round trip.txt");
    fs::write(
        &source,
        r#"use Host.{args, write_file, read_file, run_process}
use Prelude.{Cons, Nil, Ok}
let check_process = fn path -> match (run_process "/bin/test" (Cons("-f", Cons(path, Nil)))) {
  | Ok(0) -> match (run_process "/usr/bin/true" Nil) {
      | Ok(0) -> match (run_process "/usr/bin/false" Nil) { | Ok(1) -> 42 | _ -> 0 }
      | _ -> 0
    }
  | _ -> 0
}
let main = fn () -> match (args ()) {
  | Cons(path, Nil) -> match (write_file path "✓\0!") {
      | Ok(_) -> match (read_file path) { | Ok("✓\0!") -> check_process path | _ -> 0 }
      | _ -> 0
    }
  | _ -> 0
}
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable)
        .arg(&output_file)
        .output()
        .unwrap();
    assert_eq!(run.status.code(), Some(42));
    assert_eq!(fs::read_to_string(output_file).unwrap(), "✓\0!");
}

#[test]
fn compile_c_reads_runtime_directory_from_the_environment() {
    let dir = temp_dir("compile-c-runtime-directory");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        r#"use Host.{runtime_directory}
let main = fn () -> match (runtime_directory ()) { | "stage0-runtime-root" -> 42 | _ -> 0 }
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable)
        .env("RIOTML_RUNTIME_DIR", "stage0-runtime-root")
        .output()
        .unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_preserves_host_output_and_exit_status() {
    let dir = temp_dir("compile-c-host-output");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        r#"use Host.{write_stdout, write_stderr, current_directory}
let main = fn () -> { write_stdout (current_directory ()); write_stdout "stdout\\n"; write_stderr "stderr"; 42 }
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable)
        .current_dir(&dir)
        .output()
        .unwrap();
    assert_eq!(run.status.code(), Some(42));
    let current_directory = fs::canonicalize(&dir).unwrap();
    assert_eq!(
        run.stdout,
        format!("{}stdout\\n", current_directory.display()).as_bytes()
    );
    assert_eq!(run.stderr, b"stderr");
}

#[test]
fn compile_c_preserves_unicode_string_intrinsics() {
    let dir = temp_dir("compile-c-unicode-strings");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        r#"use String.{concat, length, get, slice, equal}
use Prelude.{Some}
let main = fn () -> {
  let text = "A😀B";
  match (length text, get text 1, slice text 1 2, equal text "A😀B", concat text "!") {
    | (3, Some(128512), "😀", true, "A😀B!") -> 42
    | _ -> 0
  }
}
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_rejects_invalid_utf8_from_host_files() {
    let dir = temp_dir("compile-c-host-invalid-utf8");
    let source = dir.join("main.ml");
    let invalid_file = dir.join("invalid.txt");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(&invalid_file, [0xff, 0xfe]).unwrap();
    fs::write(
        &source,
        r#"use Host.{args, read_file}
use Prelude.{Cons, Nil, Err}
let main = fn () -> match (args ()) {
  | Cons(path, Nil) -> match (read_file path) {
      | Err(message) -> match message { | "file is not valid UTF-8" -> 42 | _ -> 0 }
      | _ -> 0
    }
  | _ -> 0
}
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let invalid_utf8 = Command::new(&executable)
        .arg(invalid_file)
        .output()
        .unwrap();
    assert_eq!(invalid_utf8.status.code(), Some(42));
}

#[test]
fn compile_c_returns_host_process_launch_errors_as_results() {
    let dir = temp_dir("compile-c-host-process-error");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    let missing_program = dir.join("not-an-executable");
    fs::write(
        &source,
        format!(
            r#"use Host.{{run_process}}
use Prelude.{{Nil, Err}}
let main = fn () -> match (run_process "{}" Nil) {{ | Err(_) -> 42 | _ -> 0 }}
"#,
            missing_program.display()
        ),
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
    assert!(
        run.stderr.is_empty(),
        "spawn errors are values, not diagnostics"
    );
}

#[test]
fn compile_c_returns_host_file_errors_as_results() {
    let dir = temp_dir("compile-c-host-file-error");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    let missing_path = dir.join("missing").join("file.txt");
    fs::write(
        &source,
        r#"use Host.{args, read_file, write_file}
use Prelude.{Cons, Nil, Err}
let main = fn () -> match (args ()) {
  | Cons(path, Nil) -> match (read_file path) {
      | Err(_) -> match (write_file path "contents") { | Err(_) -> 42 | _ -> 0 }
      | _ -> 0
    }
  | _ -> 0
}
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable)
        .arg(missing_path)
        .output()
        .unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[test]
fn compile_c_rejects_type_errors_before_writing_output() {
    let dir = temp_dir("compile-c-type-error");
    let source = dir.join("invalid.ml");
    let output = dir.join("invalid.c");
    fs::write(&source, "let main = fn () -> (true: i32)\n").unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()
        .unwrap();
    assert!(!compile.status.success());
    assert!(!output.exists(), "type error left a C artifact");
    assert!(String::from_utf8_lossy(&compile.stderr).contains("typecheck failed"));
}

#[test]
fn compile_c_rejects_unsupported_input_without_writing_output() {
    let dir = temp_dir("compile-c-reject");
    let source = dir.join("main.ml");
    let output = dir.join("main.c");
    fs::write(&source, "let main = fn () -> true\n").unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()
        .unwrap();
    assert!(!compile.status.success());
    assert!(!output.exists(), "unsupported program left a C artifact");
    assert!(String::from_utf8_lossy(&compile.stderr).contains("C source emission failed"));
}

#[test]
fn compile_c_rejects_unsupported_i64_arithmetic_without_output() {
    let dir = temp_dir("compile-c-i64-arithmetic");
    let source = dir.join("main.ml");
    let output = dir.join("main.c");
    fs::write(
        &source,
        "let add_wide = fn (left: i64) -> fn (right: i64) -> left + right\nlet main = fn () -> 42\n",
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()
        .unwrap();
    assert!(!compile.status.success());
    assert!(
        !output.exists(),
        "unsupported i64 operation left a C artifact"
    );
    assert!(String::from_utf8_lossy(&compile.stderr).contains("primitive"));
}

#[test]
fn compile_c_rejects_unrepresentable_u64_literals_without_output() {
    let dir = temp_dir("compile-c-u64-range");
    let source = dir.join("main.ml");
    let output = dir.join("main.c");
    fs::write(
        &source,
        "let main = fn () -> { let too_large = (18446744073709551615: u64); 42 }\n",
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()
        .unwrap();
    assert!(!compile.status.success());
    assert!(
        !output.exists(),
        "unrepresentable integer left a C artifact"
    );
    assert!(String::from_utf8_lossy(&compile.stderr).contains("signed 64-bit value range"));
}

#[test]
fn compile_c_requires_a_unit_entry_parameter() {
    let dir = temp_dir("compile-c-entry-parameter");
    let source = dir.join("main.ml");
    let output = dir.join("main.c");
    fs::write(&source, "let main = fn (input: i32) -> input\n").unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()
        .unwrap();
    assert!(!compile.status.success());
    assert!(!output.exists(), "invalid entry ABI left a C artifact");
    assert!(
        String::from_utf8_lossy(&compile.stderr).contains("requires a top-level"),
        "unexpected entry diagnostic: {}",
        String::from_utf8_lossy(&compile.stderr)
    );
}

#[test]
fn compile_c_preserves_embedded_nul_strings() {
    let dir = temp_dir("compile-c-nul-string");
    let source = dir.join("main.ml");
    let generated_c = dir.join("main.c");
    let executable = dir.join("main");
    fs::write(
        &source,
        r#"use String.{concat, length, get, slice, equal}
use Prelude.{Cons, Err, Nil, Some}
use Host.{write_stdout, read_file, write_file, run_process}
let main = fn () -> {
  let text = "A\0B";
  write_stdout text;
  let joined = concat "A\0" "B";
  let read_rejects_nul = match (read_file "file\0name") { | Err(_) -> true | _ -> false };
  let write_rejects_nul = match (write_file "file\0name" "contents") { | Err(_) -> true | _ -> false };
  let process_rejects_nul = match (run_process "/bin/true" (Cons("argument\0", Nil))) { | Err(_) -> true | _ -> false };
  match (length text, get text 1, slice text 1 2, equal joined text, text, read_rejects_nul, write_rejects_nul, process_rejects_nul) {
    | (3, Some(0), "\0", true, "A\0B", true, true, true) -> 42
    | _ -> 0
  }
}
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("c")
        .arg(&source)
        .arg("-o")
        .arg(&generated_c)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "compiler stderr: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let runtime_dir = repo.join("riotc");
    let clang = Command::new("clang")
        .arg("-std=gnu11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg(&generated_c)
        .arg(runtime_dir.join("runtime.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        clang.status.success(),
        "clang stderr: {}",
        String::from_utf8_lossy(&clang.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
    assert_eq!(run.stdout, b"A\0B");
}

#[test]
fn compile_rejects_unsupported_target() {
    let dir = temp_dir("compile-target");
    let file = write_valid_source(&dir);

    let output = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("x86_64-unknown-linux-gnu")
        .arg(&file)
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("unsupported compile target"));
}
