// Emit checked RiotML expressions as C over the small RtValue runtime.
use Prelude.{List, Nil, Cons, Option, Some, None}
use Syntax.Ast.{
  Expr, IntegerLiteral, TextLiteral, ValueName, ConstructorName,
  Integer, Boolean, StringLiteral, Unit, VariableExpr, TupleExpr,
  RecordExpr, RecordExpression, RecordExpressionField, Binary,
  BinaryExpression, BinaryOperator, Addition, Subtraction, Multiplication,
  Division, Remainder, Equal, NotEqual, LessThan, LessThanOrEqual,
  GreaterThan, GreaterThanOrEqual, Negate, Function, FunctionExpression,
  Apply, ApplicationExpression, Let, LetExpression, Match, MatchExpression,
  MatchArm, ConstructorExpr, ConstructorExpression, Ascription,
  AscriptionExpression, Block, Pattern, Variable, PatternUnit, Wildcard,
  PatternInteger, PatternBoolean, PatternString, TuplePattern, SourceModule,
  ModulePath, ModuleName, Import, ImportedName, MemberName, Declaration,
  ValueDeclaration, ValueDeclarationData, TypeDeclaration,
  ConstructorPattern, ConstructorPatternData, RecordPattern,
  RecordPatternData, RecordPatternField, AscribedPattern,
  AscribedPatternData, expression_kind, pattern_kind
}
use String.{equal, get, length, slice}
use Modules.{module_path_relative}

type GeneratedCode = GeneratedCode {
  expression: string,
  functions: string
}

type GeneratedValues = GeneratedValues {
  initializer: string,
  count: i32,
  functions: string
}

type GeneratedRecordFields = GeneratedRecordFields {
  names_initializer: string,
  values_initializer: string,
  count: i32,
  functions: string
}

type GeneratedInitialization = GeneratedInitialization {
  statements: string,
  functions: string
}

let generated_expression = fn (GeneratedCode { expression, functions: _ }) -> expression
let generated_functions = fn (GeneratedCode { expression: _, functions }) -> functions
let generated_values_initializer = fn (GeneratedValues { initializer, count: _, functions: _ }) -> initializer
let generated_values_count = fn (GeneratedValues { initializer: _, count, functions: _ }) -> count
let generated_values_functions = fn (GeneratedValues { initializer: _, count: _, functions }) -> functions
let generated_record_names = fn (GeneratedRecordFields { names_initializer, values_initializer: _, count: _, functions: _ }) -> names_initializer
let generated_record_values = fn (GeneratedRecordFields { names_initializer: _, values_initializer, count: _, functions: _ }) -> values_initializer
let generated_record_count = fn (GeneratedRecordFields { names_initializer: _, values_initializer: _, count, functions: _ }) -> count
let generated_record_functions = fn (GeneratedRecordFields { names_initializer: _, values_initializer: _, count: _, functions }) -> functions

let c_escape_character = fn codepoint ->
  match codepoint {
    | 0 -> "\\000"
    | 8 -> "\\b"
    | 9 -> "\\t"
    | 10 -> "\\n"
    | 12 -> "\\f"
    | 13 -> "\\r"
    | 34 -> "\\\""
    | 92 -> "\\\\"
    | _ -> ""
  }

let c_string_contents = fn text -> fn offset ->
  match (offset == length text) {
    | true -> ""
    | false -> match (get text offset) {
      | None -> ""
      | Some(codepoint) -> match (equal (c_escape_character codepoint) "") {
        | true -> slice text offset (offset + 1) + c_string_contents text (offset + 1)
        | false -> c_escape_character codepoint + c_string_contents text (offset + 1)
      }
    }
  }

let c_string_literal = fn text -> "\"" + c_string_contents text 0 + "\""

let c_source_binary = fn operator ->
  match operator {
    | Addition -> "rt_add"
    | Subtraction -> "rt_sub"
    | Multiplication -> "rt_mul"
    | Division -> "rt_div"
    | Remainder -> "rt_rem"
    | Equal -> "rt_equal"
    | NotEqual -> "rt_equal"
    | LessThan -> "rt_less"
    | LessThanOrEqual -> "rt_greater"
    | GreaterThan -> "rt_greater"
    | GreaterThanOrEqual -> "rt_less"
  }

let decimal_digit = fn digit ->
  match digit {
    | 0 -> "0" | 1 -> "1" | 2 -> "2" | 3 -> "3" | 4 -> "4"
    | 5 -> "5" | 6 -> "6" | 7 -> "7" | 8 -> "8" | _ -> "9"
  }

let decimal_text = fn value ->
  match (value < 10) {
    | true -> decimal_digit value
    | false -> decimal_text (value / 10) + decimal_digit (value % 10)
  }

let count_values = fn values ->
  match values {
    | Nil -> 0
    | Cons(_, remaining) -> 1 + count_values remaining
  }

let emit_source_values = fn path -> fn environment -> fn values -> fn index ->
  match values {
    | Nil -> GeneratedValues {
      initializer: "NULL",
      count: 0,
      functions: ""
    }
    | Cons(value, remaining) -> {
      let emitted_value = emit_source_expression
        (path + "_" + decimal_text index) environment value;
      let emitted_remaining = emit_source_values path environment remaining (index + 1);
      GeneratedValues {
        initializer: append_initializer_item (generated_expression emitted_value)
          (generated_values_initializer emitted_remaining) (generated_values_count emitted_remaining),
        count: generated_values_count emitted_remaining + 1,
        functions: generated_functions emitted_value + generated_values_functions emitted_remaining
      }
    }
  }

let append_initializer_item = fn first -> fn remaining -> fn remaining_count ->
  match (remaining_count == 0) {
    | true -> "(RtValue[]){" + first + "}"
    | false -> {
      let without_wrapper = slice remaining 12 (length remaining - 1);
      "(RtValue[]){" + first + "," + without_wrapper + "}"
    }
  }

let emit_record_fields = fn path -> fn environment -> fn fields -> fn index ->
  match fields {
    | Nil -> GeneratedRecordFields {
      names_initializer: "NULL",
      values_initializer: "NULL",
      count: 0,
      functions: ""
    }
    | Cons(RecordExpressionField { name: ValueName(name), value }, remaining) -> {
      let emitted_value = emit_source_expression
        (path + "_field" + decimal_text index) environment value;
      let emitted_remaining = emit_record_fields path environment remaining (index + 1);
      GeneratedRecordFields {
        names_initializer: append_string_initializer_item
          (c_string_literal name) (generated_record_names emitted_remaining)
          (generated_record_count emitted_remaining),
        values_initializer: append_initializer_item
          (generated_expression emitted_value) (generated_record_values emitted_remaining)
          (generated_record_count emitted_remaining),
        count: generated_record_count emitted_remaining + 1,
        functions: generated_functions emitted_value + generated_record_functions emitted_remaining
      }
    }
  }

let append_string_initializer_item = fn first -> fn remaining -> fn remaining_count ->
  match (remaining_count == 0) {
    | true -> "(const char*[]){" + first + "}"
    | false -> {
      let without_wrapper = slice remaining 16 (length remaining - 1);
      "(const char*[]){" + first + "," + without_wrapper + "}"
    }
  }

let emit_pattern_condition = fn pattern -> fn value ->
  match (pattern_kind pattern) {
    | Variable(_) -> "1"
    | Wildcard -> "1"
    | PatternUnit -> "rt_is_unit(" + value + ")"
    | PatternInteger(IntegerLiteral(literal)) ->
      "(rt_is_int(" + value + ") && rt_as_int(" + value + ") == " + literal + ")"
    | PatternBoolean(true) -> "(rt_is_bool(" + value + ") && rt_as_bool(" + value + "))"
    | PatternBoolean(false) -> "(rt_is_bool(" + value + ") && !rt_as_bool(" + value + "))"
    | PatternString(TextLiteral(text)) ->
      "(rt_is_string(" + value + ") && rt_as_bool(rt_string_equal_to("
      + value + "," + c_string_literal text + ")))"
    | TuplePattern(items) ->
      "(rt_has_tuple_count(" + value + "," + decimal_text (count_values items) + ") && "
      + emit_pattern_list_condition items value 0 + ")"
    | ConstructorPattern(ConstructorPatternData { constructor_name: ConstructorName(name), arguments }) ->
      "(rt_has_variant_name(" + value + "," + c_string_literal name + ") && rt_has_variant_count("
      + value + "," + decimal_text (count_values arguments) + ") && "
      + emit_pattern_list_condition arguments value 0 + ")"
    | RecordPattern(RecordPatternData { constructor_name: ConstructorName(name), fields }) ->
      "(rt_has_variant_name(" + value + "," + c_string_literal name + ") && "
      + emit_record_pattern_condition fields value + ")"
    | AscribedPattern(AscribedPatternData { pattern: inner, annotation: _ }) ->
      emit_pattern_condition inner value
  }

let emit_pattern_list_condition = fn patterns -> fn value -> fn index ->
  match patterns {
    | Nil -> "1"
    | Cons(pattern, remaining) ->
      "(" + emit_pattern_condition pattern ("rt_field(" + value + "," + decimal_text index + ")")
      + " && " + emit_pattern_list_condition remaining value (index + 1) + ")"
  }

let emit_record_pattern_condition = fn fields -> fn value ->
  match fields {
    | Nil -> "1"
    | Cons(RecordPatternField { name: ValueName(name), pattern }, remaining) ->
      "(rt_has_named_field(" + value + "," + c_string_literal name + ") && "
      + emit_pattern_condition pattern ("rt_field_named(" + value + "," + c_string_literal name + ")")
      + " && " + emit_record_pattern_condition remaining value + ")"
  }

let emit_pattern_bindings = fn pattern -> fn value -> fn environment ->
  match (pattern_kind pattern) {
    | Variable(ValueName(name)) -> "rt_environment_bind(" + environment + ","
      + c_string_literal name + "," + value + ");\n"
    | Wildcard -> ""
    | PatternUnit -> ""
    | PatternInteger(_) -> ""
    | PatternBoolean(_) -> ""
    | PatternString(_) -> ""
    | TuplePattern(items) -> emit_pattern_list_bindings items value environment 0
    | ConstructorPattern(ConstructorPatternData { constructor_name: _, arguments }) ->
      emit_pattern_list_bindings arguments value environment 0
    | RecordPattern(RecordPatternData { constructor_name: _, fields }) ->
      emit_record_pattern_bindings fields value environment
    | AscribedPattern(AscribedPatternData { pattern: inner, annotation: _ }) ->
      emit_pattern_bindings inner value environment
  }

let emit_pattern_list_bindings = fn patterns -> fn value -> fn environment -> fn index ->
  match patterns {
    | Nil -> ""
    | Cons(pattern, remaining) ->
      emit_pattern_bindings pattern
        ("rt_field(" + value + "," + decimal_text index + ")") environment
      + emit_pattern_list_bindings remaining value environment (index + 1)
  }

let emit_record_pattern_bindings = fn fields -> fn value -> fn environment ->
  match fields {
    | Nil -> ""
    | Cons(RecordPatternField { name: ValueName(name), pattern }, remaining) ->
      emit_pattern_bindings pattern
        ("rt_field_named(" + value + "," + c_string_literal name + ")") environment
      + emit_record_pattern_bindings remaining value environment
  }

let emit_match_arms = fn path -> fn environment -> fn scrutinee -> fn arms -> fn index ->
  match arms {
    | Nil -> GeneratedCode { expression: "", functions: "" }
    | Cons(MatchArm { pattern, body }, remaining) -> {
      let arm_name = "arm_environment_" + path + "_" + decimal_text index;
      let arm_path = path + "_arm" + decimal_text index;
      let emitted_body = emit_source_expression arm_path arm_name body;
      let emitted_remaining = emit_match_arms path environment scrutinee remaining (index + 1);
      let branch_prefix = match (index == 0) {
        | true -> "if ("
        | false -> "else if ("
      };
      GeneratedCode {
        expression: branch_prefix + emit_pattern_condition pattern scrutinee + ") {\n"
          + "RtValue " + arm_name + " = rt_environment_child(" + environment + ");\n"
          + emit_pattern_bindings pattern scrutinee arm_name
          + "match_result = " + generated_expression emitted_body + ";\nmatch_found = 1;\n"
          + "}\n" + generated_expression emitted_remaining,
        functions: generated_functions emitted_body + generated_functions emitted_remaining
      }
    }
  }

let emit_source_expression = fn path -> fn environment -> fn expression ->
  match (expression_kind expression) {
    | Integer(IntegerLiteral(value)) -> GeneratedCode {
      expression: "rt_int(" + value + ")",
      functions: ""
    }
    | Boolean(true) -> GeneratedCode { expression: "rt_bool(1)", functions: "" }
    | Boolean(false) -> GeneratedCode { expression: "rt_bool(0)", functions: "" }
    | StringLiteral(TextLiteral(value)) -> GeneratedCode {
      expression: "rt_string(" + c_string_literal value + ")",
      functions: ""
    }
    | Unit -> GeneratedCode { expression: "rt_unit()", functions: "" }
    | VariableExpr(ValueName(name)) -> GeneratedCode {
      expression: "rt_environment_get(" + environment + "," + c_string_literal name + ")",
      functions: ""
    }
    | TupleExpr(items) -> {
      let values = emit_source_values path environment items 0;
      GeneratedCode {
        expression: "rt_tuple(" + decimal_text (generated_values_count values) + ","
          + generated_values_initializer values + ")",
        functions: generated_values_functions values
      }
    }
    | RecordExpr(RecordExpression { constructor_name: ConstructorName(name), fields }) -> {
      let emitted_fields = emit_record_fields path environment fields 0;
      GeneratedCode {
        expression: "rt_record_named(" + c_string_literal name + ","
          + decimal_text (generated_record_count emitted_fields) + ","
          + generated_record_names emitted_fields + ","
          + generated_record_values emitted_fields + ")",
        functions: generated_record_functions emitted_fields
      }
    }
    | Binary(BinaryExpression { operator, left, right }) -> {
      let emitted_left = emit_source_expression (path + "_left") environment left;
      let emitted_right = emit_source_expression (path + "_right") environment right;
      let left_code = generated_expression emitted_left;
      let right_code = generated_expression emitted_right;
      let expression_code = match operator {
        | NotEqual -> "rt_bool(!rt_as_bool(rt_equal(" + left_code + "," + right_code + ")))"
        | LessThanOrEqual -> "rt_bool(!rt_as_bool(rt_greater(" + left_code + "," + right_code + ")))"
        | GreaterThanOrEqual -> "rt_bool(!rt_as_bool(rt_less(" + left_code + "," + right_code + ")))"
        | _ -> c_source_binary operator + "(" + left_code + "," + right_code + ")"
      };
      GeneratedCode {
        expression: expression_code,
        functions: generated_functions emitted_left + generated_functions emitted_right
      }
    }
    | Negate(operand) -> {
      let emitted_operand = emit_source_expression (path + "_negated") environment operand;
      GeneratedCode {
        expression: "rt_i32_neg(" + generated_expression emitted_operand + ")",
        functions: generated_functions emitted_operand
      }
    }
    | Function(FunctionExpression { parameter, body }) -> {
      let function_name = "riotml_lambda_" + path;
      let local_environment = "function_environment_" + path;
      let body_code = emit_source_expression (path + "_body") local_environment body;
      let function_definition =
        "static RtValue " + function_name + "(RtValue function_environment, RtValue argument) {\n"
        + "RtValue " + local_environment + " = rt_environment_child(function_environment);\n"
        + "if (!(" + emit_pattern_condition parameter "argument" + ")) rt_pattern_fail();\n"
        + emit_pattern_bindings parameter "argument" local_environment
        + "return " + generated_expression body_code + ";\n}\n";
      GeneratedCode {
        expression: "rt_function(" + function_name + "," + environment + ")",
        functions: "static RtValue " + function_name + "(RtValue, RtValue);\n"
          + generated_functions body_code + function_definition
      }
    }
    | Apply(ApplicationExpression { callee, argument }) -> {
      let emitted_callee = emit_source_expression (path + "_callee") environment callee;
      let emitted_argument = emit_source_expression (path + "_argument") environment argument;
      GeneratedCode {
        expression: "rt_apply(" + generated_expression emitted_callee + ","
          + generated_expression emitted_argument + ")",
        functions: generated_functions emitted_callee + generated_functions emitted_argument
      }
    }
    | Let(LetExpression { pattern, annotation: _, value, body }) -> {
      let binding_value = "let_value_" + path;
      let local_environment = "let_environment_" + path;
      let emitted_value = emit_source_expression (path + "_value") environment value;
      let emitted_body = emit_source_expression (path + "_body") local_environment body;
      let expression_code = "({ RtValue " + binding_value + " = "
        + generated_expression emitted_value + ";\n"
        + "RtValue " + local_environment + " = rt_environment_child(" + environment + ");\n"
        + "if (!(" + emit_pattern_condition pattern binding_value + ")) rt_pattern_fail();\n"
        + emit_pattern_bindings pattern binding_value local_environment
        + generated_expression emitted_body + "; })";
      GeneratedCode {
        expression: expression_code,
        functions: generated_functions emitted_value + generated_functions emitted_body
      }
    }
    | Match(MatchExpression { scrutinee, arms }) -> {
      let emitted_scrutinee = emit_source_expression (path + "_scrutinee") environment scrutinee;
      let arms_code = emit_match_arms path environment "match_value" arms 0;
      GeneratedCode {
        expression: "({ RtValue match_value = " + generated_expression emitted_scrutinee + ";\n"
          + "RtValue match_result = rt_unit(); int match_found = 0;\n"
          + generated_expression arms_code + "if (!match_found) rt_pattern_fail();\nmatch_result; })",
        functions: generated_functions emitted_scrutinee + generated_functions arms_code
      }
    }
    | ConstructorExpr(ConstructorExpression { constructor_name: ConstructorName(name), arguments }) -> {
      let values = emit_source_values path environment arguments 0;
      GeneratedCode {
        expression: "rt_variant_named(" + c_string_literal name + ","
          + decimal_text (generated_values_count values) + ","
          + generated_values_initializer values + ")",
        functions: generated_values_functions values
      }
    }
    | Ascription(AscriptionExpression { expression: inner, annotation: _ }) ->
      emit_source_expression (path + "_ascribed") environment inner
    | Block(items) -> {
      let emitted_items = emit_block_items path environment items 0;
      GeneratedCode {
        expression: "({ RtValue block_result = rt_unit();\n"
          + generated_expression emitted_items + "block_result; })",
        functions: generated_functions emitted_items
      }
    }
  }

let emit_block_items = fn path -> fn environment -> fn items -> fn index ->
  match items {
    | Nil -> GeneratedCode { expression: "", functions: "" }
    | Cons(item, remaining) -> {
      let emitted_item = emit_source_expression
        (path + "_item" + decimal_text index) environment item;
      let emitted_remaining = emit_block_items path environment remaining (index + 1);
      GeneratedCode {
        expression: "block_result = " + generated_expression emitted_item + ";\n"
          + generated_expression emitted_remaining,
        functions: generated_functions emitted_item + generated_functions emitted_remaining
      }
    }
  }

let path_ends_with = fn (path: string) -> fn (suffix: string) ->
  match (length path < length suffix) {
    | true -> false
    | false -> equal (slice path (length path - length suffix) (length path)) suffix
  }

let source_path_matches_import = fn (source_path: string) -> fn (module_path: ModulePath) -> {
  let relative_path = module_path_relative module_path;
  match (equal source_path relative_path) {
    | true -> true
    | false -> path_ends_with source_path ("/" + relative_path)
  }
}

let find_module_index = fn target_path -> fn modules -> fn index ->
  match modules {
    | Nil -> -1
    | Cons((source_path, _), remaining) -> match (source_path_matches_import source_path target_path) {
      | true -> index
      | false -> find_module_index target_path remaining (index + 1)
    }
  }

let find_root_module_index = fn root_path -> fn modules -> fn index ->
  match modules {
    | Nil -> -1
    | Cons((source_path, _), remaining) -> match (equal root_path source_path) {
      | true -> index
      | false -> find_root_module_index root_path remaining (index + 1)
    }
  }

let module_at_index = fn modules -> fn index ->
  match modules {
    | Nil -> None
    | Cons(module, remaining) -> match (index == 0) {
      | true -> Some(module)
      | false -> module_at_index remaining (index - 1)
    }
  }

let module_exports_value = fn wanted_name -> fn declarations ->
  match declarations {
    | Nil -> false
    | Cons(ValueDeclaration(ValueDeclarationData {
        name: ValueName(declared_name),
        body: _
      }), remaining) -> match (equal wanted_name declared_name) {
      | true -> true
      | false -> module_exports_value wanted_name remaining
    }
    | Cons(TypeDeclaration(_), remaining) -> module_exports_value wanted_name remaining
  }

let emit_imported_names = fn target_environment -> fn source_environment -> fn declarations -> fn names ->
  match names {
    | Nil -> ""
    | Cons(ImportedName(MemberName(name)), remaining) -> {
      let binding = match (module_exports_value name declarations) {
        | true -> "rt_environment_bind(" + target_environment + ","
          + c_string_literal name + ",rt_environment_get(" + source_environment
          + "," + c_string_literal name + "));\n"
        | false -> ""
      };
      binding + emit_imported_names target_environment source_environment declarations remaining
    }
  }

let emit_module_imports = fn modules -> fn target_environment -> fn imports ->
  match imports {
    | Nil -> ""
    | Cons(Import { module_path, selected_names }, remaining) -> {
      let source_index = find_module_index module_path modules 0;
      let binding = match (module_at_index modules source_index) {
        | None -> ""
        | Some((_, SourceModule { imports: _, declarations })) -> emit_imported_names
            target_environment ("module_environment_" + decimal_text source_index)
            declarations selected_names
      };
      binding + emit_module_imports modules target_environment remaining
    }
  }

let emit_host_bindings = fn environment ->
  "rt_environment_bind(" + environment + ",\"args\",rt_function(rt_host_args,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"current_directory\",rt_function(rt_host_current_directory,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"runtime_directory\",rt_function(rt_host_runtime_directory,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"read_file\",rt_function(rt_read_file,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"write_file\",rt_function(rt_write_file,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"write_stdout\",rt_function(rt_write_stdout,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"write_stderr\",rt_function(rt_write_stderr,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"run_process\",rt_function(rt_run_process,rt_unit()));\n"

let emit_string_bindings = fn environment ->
  "rt_environment_bind(" + environment + ",\"concat\",rt_function(rt_string_concat,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"length\",rt_function(rt_string_length,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"get\",rt_function(rt_string_get,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"slice\",rt_function(rt_string_slice,rt_unit()));\n"
  + "rt_environment_bind(" + environment + ",\"equal\",rt_function(rt_string_equal,rt_unit()));\n"

let emit_module_value_declarations = fn index -> fn environment -> fn declarations -> fn declaration_index ->
  match declarations {
    | Nil -> GeneratedInitialization { statements: "", functions: "" }
    | Cons(ValueDeclaration(ValueDeclarationData { name: ValueName(name), body }), remaining) -> {
      let emitted_value = emit_source_expression
        ("module" + decimal_text index + "_value" + decimal_text declaration_index)
        environment body;
      let emitted_remaining = emit_module_value_declarations index environment remaining
        (declaration_index + 1);
      GeneratedInitialization {
        statements: "rt_environment_bind(" + environment + "," + c_string_literal name
          + "," + generated_expression emitted_value + ");\n"
          + generated_initialization_statements emitted_remaining,
        functions: generated_functions emitted_value
          + generated_initialization_functions emitted_remaining
      }
    }
    | Cons(TypeDeclaration(_), remaining) ->
      emit_module_value_declarations index environment remaining (declaration_index + 1)
  }

let generated_initialization_statements = fn (GeneratedInitialization { statements, functions: _ }) -> statements
let generated_initialization_functions = fn (GeneratedInitialization { statements: _, functions }) -> functions

let source_path_is_host_module = fn source_path ->
  path_ends_with source_path "tinyml/lib/host.ml"

let source_path_is_string_module = fn source_path ->
  path_ends_with source_path "tinyml/lib/std/string.ml"

let emit_module_initialization = fn modules -> fn index ->
  match (module_at_index modules index) {
    | None -> GeneratedInitialization { statements: "", functions: "" }
    | Some((source_path, SourceModule { imports, declarations })) -> {
      let environment = "module_environment_" + decimal_text index;
      let module_imports = emit_module_imports modules environment imports;
      let special_bindings = match (source_path_is_host_module source_path) {
        | true -> emit_host_bindings environment
        | false -> match (source_path_is_string_module source_path) {
          | true -> emit_string_bindings environment
          | false -> ""
        }
      };
      let generated_declarations = match (source_path_is_host_module source_path) {
        | true -> GeneratedInitialization { statements: "", functions: "" }
        | false -> match (source_path_is_string_module source_path) {
          | true -> GeneratedInitialization { statements: "", functions: "" }
          | false -> emit_module_value_declarations index environment declarations 0
        }
      };
      GeneratedInitialization {
        statements: module_imports + special_bindings
          + generated_initialization_statements generated_declarations,
        functions: generated_initialization_functions generated_declarations
      }
    }
  }

let emit_module_environments = fn modules -> fn index ->
  match modules {
    | Nil -> ""
    | Cons(_, remaining) ->
      "static RtValue module_environment_" + decimal_text index + ";\n"
      + emit_module_environments remaining (index + 1)
  }

let emit_module_environment_setup = fn modules -> fn index ->
  match modules {
    | Nil -> ""
    | Cons(_, remaining) ->
      "module_environment_" + decimal_text index + " = rt_environment_child(root_environment);\n"
      + emit_module_environment_setup remaining (index + 1)
  }

let emit_module_initializations = fn all_modules -> fn remaining_modules -> fn index ->
  match remaining_modules {
    | Nil -> GeneratedInitialization { statements: "", functions: "" }
    | Cons(_, remaining) -> {
      let emitted_module = emit_module_initialization all_modules index;
      let emitted_remaining = emit_module_initializations all_modules remaining (index + 1);
      GeneratedInitialization {
        statements: generated_initialization_statements emitted_module
          + generated_initialization_statements emitted_remaining,
        functions: generated_initialization_functions emitted_module
          + generated_initialization_functions emitted_remaining
      }
    }
  }

let emit_module_program = fn root_path -> fn runtime_directory -> fn modules -> {
  let root_index = find_root_module_index root_path modules 0;
  let generated_initializations = emit_module_initializations modules modules 0;
  let root_environment = "module_environment_" + decimal_text root_index;
  "#include \"runtime.h\"\n"
  + emit_module_environments modules 0
  + generated_initialization_functions generated_initializations
  + "int main(int argc, char **argv) {\n"
  + "  rt_set_args(argc, argv);\n"
  + "  rt_set_runtime_directory(" + c_string_literal runtime_directory + ");\n"
  + "  RtValue root_environment = rt_environment_root();\n"
  + emit_module_environment_setup modules 0
  + generated_initialization_statements generated_initializations
  + "  RtValue entry = rt_environment_get(" + root_environment + ",\"main\");\n"
  + "  return (int)rt_as_int(rt_apply(entry, rt_unit()));\n}\n"
}
