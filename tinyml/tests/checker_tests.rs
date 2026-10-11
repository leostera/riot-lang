use tinyml::{checker, parser};

fn check(source: &str) -> checker::ModuleSummary {
    let module = parser::parse(source).expect("source parses");
    checker::check_module(&module)
}

fn assert_valid(source: &str) {
    let summary = check(source);
    assert!(
        summary.diagnostics.is_empty(),
        "expected source to typecheck, got: {:#?}",
        summary.diagnostics
    );
}

fn assert_has_type_mismatch(source: &str) {
    let summary = check(source);
    assert!(
        summary
            .diagnostics
            .iter()
            .any(|diagnostic| matches!(diagnostic, checker::CheckDiagnostic::TypeMismatch { .. })),
        "expected a type mismatch, got: {:#?}",
        summary.diagnostics
    );
}

#[test]
fn polymorphic_identity_is_independently_instantiated() {
    assert_valid(
        r#"
        let identity = fn value -> value
        let pair = (identity 1, identity true)
        "#,
    );
}

#[test]
fn all_fixed_width_integer_types_are_available_in_annotations() {
    let source = ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"]
        .into_iter()
        .map(|ty| format!("let identity_{ty} = fn (value: {ty}) -> value"))
        .collect::<Vec<_>>()
        .join("\n");
    assert_valid(&source);
}

#[test]
fn top_level_recursive_functions_are_checked_monomorphically() {
    assert_valid(
        r#"
        let factorial = fn (n: i32) -> match n {
          | 0 -> 1
          | _ -> n * factorial (n - 1)
        }
        "#,
    );
}

#[test]
fn recursively_defined_variant_can_be_matched() {
    assert_valid(
        r#"
        type Expr =
          | Int(i32)
          | Add(Expr, Expr)
          | Let(string, Expr, Expr)

        let evaluate = fn (expr: Expr) -> match expr {
          | Int(value) -> value
          | Add(left, right) -> evaluate left + evaluate right
          | Let(_, value, body) -> evaluate body
        }
        "#,
    );
}

#[test]
fn annotations_constrain_expression_and_parameter_types() {
    assert_valid(
        r#"
        let increment = fn (value: i32) -> (value + 1: i32)
        let answer = (increment 41: i32)
        "#,
    );
}

#[test]
fn annotated_strings_keep_concatenation_support() {
    assert_valid(r#"let join = fn (left: string) -> left + "!""#);
}

#[test]
fn expression_annotation_mismatch_is_rejected() {
    assert_has_type_mismatch("let invalid = (true: i32)");
}

#[test]
fn parameter_pattern_annotation_mismatch_is_rejected() {
    assert_has_type_mismatch(r#"let invalid = fn ((value: i32): bool) -> value"#);
}

#[test]
fn integer_literals_are_contextual_and_range_checked() {
    assert_valid(
        r#"
        let small = (255: u8)
        let large = (18446744073709551615: u64)
        let add_one = fn (value: u8) -> value + 1
        let applied = add_one 255
        "#,
    );

    let summary = check("let too_large = (256: u8)");
    assert!(summary.diagnostics.iter().any(|diagnostic| matches!(
        diagnostic,
        checker::CheckDiagnostic::IntegerLiteralOutOfRange { type_name, .. }
            if type_name == "u8"
    )));
}

#[test]
fn signed_integer_literals_cover_minimum_values_and_unary_negation() {
    assert_valid(
        r#"
        let minimum_i8 = (-128: i8)
        let minimum_i64 = (-9223372036854775808: i64)
        let negate = fn (value: i32) -> -value
        "#,
    );

    let summary = check("let negative_unsigned = (-1: u8)");
    assert!(summary.diagnostics.iter().any(|diagnostic| matches!(
        diagnostic,
        checker::CheckDiagnostic::IntegerLiteralOutOfRange { type_name, .. }
            if type_name == "u8"
    )));
}

#[test]
fn tuple_and_match_contexts_type_integer_literals() {
    assert_valid(
        r#"
        type Wrapped = Wrapped(u8)
        let values = ((1, 2): (u8, u32))
        let is_one = fn (value: u8) -> match value {
          | 1 -> true
          | _ -> false
        }
        let wrapped = Wrapped(1)
        let wrapped_is_one = fn (value: Wrapped) -> match value {
          | Wrapped(1) -> true
          | Wrapped(_) -> false
        }
        "#,
    );
}

#[test]
fn unsupported_float_literals_and_types_are_rejected() {
    let literal_summary = check("let pi = 3.14");
    assert!(
        literal_summary
            .diagnostics
            .iter()
            .any(|diagnostic| matches!(
                diagnostic,
                checker::CheckDiagnostic::UnsupportedFloatLiteral { .. }
            ))
    );

    let type_summary = check(r#"let identity = fn (value: f64) -> value"#);
    assert!(type_summary.diagnostics.iter().any(|diagnostic| matches!(
        diagnostic,
        checker::CheckDiagnostic::UnsupportedType { name, .. } if name == "f64"
    )));
}

#[test]
fn integer_operators_require_compatible_operands() {
    assert_has_type_mismatch("let invalid = fn (value: i32) -> value + true");
}

#[test]
fn polymorphic_variants_and_constructor_patterns_typecheck() {
    assert_valid(
        r#"
        type Option<'a> =
          | None
          | Some('a)

        let get_or_zero = fn (option: Option<i32>) -> match option {
          | None -> 0
          | Some(value) -> value
        }
        "#,
    );
}

#[test]
fn constructor_expression_and_pattern_arities_are_checked() {
    let summary = check(
        r#"
        type Option<'a> =
          | None
          | Some('a)

        let too_many_arguments = Some(1, 2)
        let missing_pattern_argument = fn (Some) -> 0
        "#,
    );

    let arity_errors = summary
        .diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                diagnostic,
                checker::CheckDiagnostic::ConstructorArityMismatch { .. }
            )
        })
        .count();
    assert_eq!(arity_errors, 2, "diagnostics: {:#?}", summary.diagnostics);
}

#[test]
fn incomplete_variant_match_is_rejected() {
    let summary = check(
        r#"
        type Option<'a> =
          | None
          | Some('a)

        let get = fn (option: Option<i32>) -> match option {
          | Some(value) -> value
        }
        "#,
    );
    assert!(
        summary
            .diagnostics
            .iter()
            .any(|diagnostic| matches!(diagnostic, checker::CheckDiagnostic::MissingConstructor { name, .. } if name == "None")),
        "expected missing-constructor diagnostic, got: {:#?}",
        summary.diagnostics
    );
}

#[test]
fn record_construction_and_field_patterns_typecheck() {
    assert_valid(
        r#"
        type User = User { name: string, age: i32 }

        let user = User { name: "Ada", age: 42 }
        let get_age = fn (User { age, name: _ }) -> age
        "#,
    );
}

#[test]
fn incomplete_record_construction_is_rejected() {
    let summary = check(
        r#"
        type User = User { name: string, age: i32 }
        let incomplete = User { name: "Ada" }
        "#,
    );
    assert!(summary.diagnostics.iter().any(|diagnostic| matches!(
        diagnostic,
        checker::CheckDiagnostic::MissingField { name, .. } if name == "age"
    )));
}

#[test]
fn tuple_patterns_preserve_each_element_type() {
    assert_valid(
        r#"
        let first = fn (left, right) -> left
        let selected = first (true, 42)
        "#,
    );
}
