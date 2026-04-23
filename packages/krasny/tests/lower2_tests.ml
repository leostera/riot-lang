open Std
open Std.Collections

let sample_ml = Path.v "sample.ml"

let sample_mli = Path.v "sample.mli"

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create source slice: " ^ Kernel.IO.Error.message error)

let parse2_ml = fun source -> Syn.parse2 ~filename:sample_ml (source_slice source)

let parse2_mli = fun source -> Syn.parse2 ~filename:sample_mli (source_slice source)

let format2_ml = fun source -> parse2_ml source |> Krasny.format2

let format2_mli = fun source -> parse2_mli source |> Krasny.format2

let parse2_source = fun ~filename source -> Syn.parse2 ~filename (source_slice source)

let format2_source = fun ~filename source -> parse2_source ~filename source |> Krasny.format2

let top_level = fun items -> String.concat "\n\n" items ^ "\n"

let assert_format2_ml = fun ~expected source ->
  let actual = format2_ml source |> Result.expect ~msg:"implementation should format through lower2" in
  if expected = actual then
    Ok ()
  else
    Error ("lower2 implementation output mismatch\nexpected:\n" ^ expected ^ "\nactual:\n" ^ actual)

let assert_format2_mli = fun ~expected source ->
  let actual = format2_mli source |> Result.expect ~msg:"interface should format through lower2" in
  if expected = actual then
    Ok ()
  else
    Error ("lower2 interface output mismatch\nexpected:\n" ^ expected ^ "\nactual:\n" ^ actual)

let assert_format2_ml_fails = fun source ->
  match format2_ml source with
  | Ok formatted -> Error ("lower2 unexpectedly formatted unsupported source as:\n" ^ formatted)
  | Error _ -> Ok ()

let approved_snapshot_path = fun path -> Path.add_extension path ~ext:"expected"

let fixture_manifest_path = Path.v "packages/krasny/tests/format_expectations.txt"

let fixtures_dir = Path.v "packages/krasny/tests/fixtures"

let manifest_fixture_paths = fun () ->
  let manifest = Fs.read fixture_manifest_path |> Result.expect ~msg:"failed to read krasny fixture manifest" in
  let lines = manifest |> String.split_on_char '\n' |> List.map ~fn:String.trim in
  let rec loop paths = function
    | [] -> List.reverse paths
    | line :: rest ->
        if String.equal line "" || String.starts_with ~prefix:"#" line then
          loop paths rest
        else
          let relpath = Path.from_string line |> Result.expect ~msg:"fixture manifest entry should be valid UTF-8" in
          loop (Path.(fixtures_dir / relpath) :: paths) rest
  in
  loop [] lines

let parser2_formatter_fixture_supported = fun path ->
  let path = Path.to_string path in
  not (String.contains path "class")

let assert_lower2_fixture_matches_approved = fun path ->
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  match format2_source ~filename:path source with
  | Error err -> Error (Path.to_string path
  ^ " failed lower2 formatting: "
  ^ Krasny.format_error_to_string err)
  | Ok formatted -> (
      let expected_path = approved_snapshot_path path in
      let expected = Fs.read expected_path |> Result.expect ~msg:"approved fixture snapshot should exist" in
      if not (String.equal expected formatted) then
        Error (Path.to_string path
        ^ " lower2 output did not match approved formatter snapshot\nexpected:\n"
        ^ expected
        ^ "\nactual:\n"
        ^ formatted)
      else
        match format2_source ~filename:path formatted with
        | Error err -> Error (Path.to_string path
        ^ " formatted once but failed to format again: "
        ^ Krasny.format_error_to_string err)
        | Ok reformatted ->
            if formatted = reformatted then
              Ok ()
            else
              Error (Path.to_string path
              ^ " is not lower2-idempotent after one format\nfirst:\n"
              ^ formatted
              ^ "\nsecond:\n"
              ^ reformatted)
    )

let assert_lower2_existing_fixture_subset = fun () ->
  let fixtures = [
    Path.v "packages/krasny/tests/fixtures/0100_atoms_and_basic_expressions.ml";
    Path.v "packages/krasny/tests/fixtures/0101_apply_list_trailing_separator.ml";
    Path.v "packages/krasny/tests/fixtures/0200_operators_and_parens.ml";
    Path.v "packages/krasny/tests/fixtures/0300_bindings_and_control_flow.ml";
    Path.v "packages/krasny/tests/fixtures/0310_let_mutual_and_bindings.ml";
    Path.v "packages/krasny/tests/fixtures/0320_field_assign_expression.ml";
    Path.v "packages/krasny/tests/fixtures/0330_let_mutual_trailing_comment.ml";
    Path.v "packages/krasny/tests/fixtures/0340_typed_let_with_locally_abstract_type.ml";
    Path.v "packages/krasny/tests/fixtures/0341_labeled_wildcard_after_positional_parameter.ml";
    Path.v "packages/krasny/tests/fixtures/0342_constructor_pattern_before_labeled_parameters.ml";
    Path.v "packages/krasny/tests/fixtures/0343_let_parameter_with_comment_pipeline.ml";
    Path.v "packages/krasny/tests/fixtures/0344_typed_let_alias_pattern_parameter.ml";
    Path.v "packages/krasny/tests/fixtures/0350_typed_function_binding_headers.ml";
    Path.v "packages/krasny/tests/fixtures/0360_local_open_record_pattern.ml";
    Path.v "packages/krasny/tests/fixtures/0370_section_comment_order.ml";
    Path.v "packages/krasny/tests/fixtures/0380_long_grouped_bench_list.ml";
    Path.v "packages/krasny/tests/fixtures/0400_functions_match_and_patterns.ml";
    Path.v "packages/krasny/tests/fixtures/0410_negative_literal_patterns.ml";
    Path.v "packages/krasny/tests/fixtures/0411_match_case_char_guard.ml";
    Path.v "packages/krasny/tests/fixtures/0412_parenthesized_match_or_pattern.ml";
    Path.v "packages/krasny/tests/fixtures/0413_parenthesized_operator_value.ml";
    Path.v "packages/krasny/tests/fixtures/0414_typed_match_scrutinee.ml";
    Path.v "packages/krasny/tests/fixtures/0415_nested_fun_parameter_stability.ml";
    Path.v "packages/krasny/tests/fixtures/0416_local_open_unary_operator_value.ml";
    Path.v "packages/krasny/tests/fixtures/0417_local_open_operator_spacing.ml";
    Path.v "packages/krasny/tests/fixtures/0418_keyword_operator_binding_pattern.ml";
    Path.v "packages/krasny/tests/fixtures/0419_typed_constructor_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0420_typed_expression_parenthesized.ml";
    Path.v "packages/krasny/tests/fixtures/0421_labeled_arg_poly_variant_then_label.ml";
    Path.v "packages/krasny/tests/fixtures/0422_top_level_expression_double_semicolon_before_floating_attribute.ml";
    Path.v "packages/krasny/tests/fixtures/0423_extended_index_operators.ml";
    Path.v "packages/krasny/tests/fixtures/0424_top_level_trailing_sequence_prefix_operator.ml";
    Path.v "packages/krasny/tests/fixtures/0425_prefix_parenthesized_field_access.ml";
    Path.v "packages/krasny/tests/fixtures/0426_top_level_trailing_sequence_before_let.ml";
    Path.v "packages/krasny/tests/fixtures/0427_let_in_sequence_after_multiline_apply.ml";
    Path.v "packages/krasny/tests/fixtures/0428_top_level_phrase_separator_after_supported_item.ml";
    Path.v "packages/krasny/tests/fixtures/0429_qualified_local_open_record_literal.ml";
    Path.v "packages/krasny/tests/fixtures/0430_signature_last_docstring.mli";
    Path.v "packages/krasny/tests/fixtures/0431_type_mutual_docstring_between_members.mli";
    Path.v "packages/krasny/tests/fixtures/0432_nested_poly_variant_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0433_apply_parenthesized_poly_variant_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0434_poly_variant_local_open_pattern_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0500_labeled_and_optional_arguments.ml";
    Path.v "packages/krasny/tests/fixtures/0510_typed_labeled_parameter_arrow.ml";
    Path.v "packages/krasny/tests/fixtures/0511_optional_parameter_default_preservation.ml";
    Path.v "packages/krasny/tests/fixtures/0512_labeled_arg_literal_then_positional_before_infix.ml";
    Path.v "packages/krasny/tests/fixtures/0513_labeled_string_then_or_chain.ml";
    Path.v "packages/krasny/tests/fixtures/0514_labeled_tuple_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0515_optional_arg_parenthesized_payload_then_unit_before_tuple.ml";
    Path.v "packages/krasny/tests/fixtures/0516_labeled_arg_then_optional_then_unit_before_tuple.ml";
    Path.v "packages/krasny/tests/fixtures/0700_types_and_type_declarations.ml";
    Path.v "packages/krasny/tests/fixtures/0710_first_class_module_types.ml";
    Path.v "packages/krasny/tests/fixtures/0720_signature_external_declaration.mli";
    Path.v "packages/krasny/tests/fixtures/0721_poly_variant_inherit_type_alias.mli";
    Path.v "packages/krasny/tests/fixtures/0722_poly_variant_union_type_alias.ml";
    Path.v "packages/krasny/tests/fixtures/0723_variant_constructor_poly_variant_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0724_type_extension_poly_variant_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0725_signature_type_alias_docstring.mli";
    Path.v "packages/krasny/tests/fixtures/0726_signature_consecutive_type_aliases.mli";
    Path.v "packages/krasny/tests/fixtures/0727_signature_abstract_type_then_value_docstrings.mli";
    Path.v "packages/krasny/tests/fixtures/0728_external_declaration_attribute.ml";
    Path.v "packages/krasny/tests/fixtures/0729_shortcut_extension_declaration_items.ml";
    Path.v "packages/krasny/tests/fixtures/0730_signature_operator_value_declarations.mli";
    Path.v "packages/krasny/tests/fixtures/0731_signature_docstring_after_open.mli";
    Path.v "packages/krasny/tests/fixtures/0732_signature_section_between_value_docstrings.mli";
    Path.v "packages/krasny/tests/fixtures/0733_signature_value_docstring_then_section_then_type_docstrings.mli";
    Path.v "packages/krasny/tests/fixtures/0734_signature_type_trailing_doc_then_heading.mli";
    Path.v "packages/krasny/tests/fixtures/0735_signature_record_trailing_doc_then_heading.mli";
    Path.v "packages/krasny/tests/fixtures/0736_signature_module_overview_before_open_then_type_doc.mli";
    Path.v "packages/krasny/tests/fixtures/0737_signature_sum_type_trailing_doc_then_next_type_leading_doc.mli";
    Path.v "packages/krasny/tests/fixtures/0738_signature_nested_module_adjacent_type_docs_and_section_heading.mli";
    Path.v "packages/krasny/tests/fixtures/0739_signature_record_type_leading_doc_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0740_signature_abstract_type_doc_then_creation_heading_then_values.mli";
    Path.v "packages/krasny/tests/fixtures/0741_signature_abstract_type_then_error_type_then_section_heading.mli";
    Path.v "packages/krasny/tests/fixtures/0742_signature_protocol_sections_and_alias_docs.mli";
    Path.v "packages/krasny/tests/fixtures/0743_signature_variant_constructor_doc_before_type_level_doc.mli";
    Path.v "packages/krasny/tests/fixtures/0744_signature_record_field_doc_before_type_level_doc.mli";
    Path.v "packages/krasny/tests/fixtures/0800_modules_signatures_and_functors.ml";
    Path.v "packages/krasny/tests/fixtures/0801_module_with_exception_declarations.ml";
    Path.v "packages/krasny/tests/fixtures/0810_local_module_parameter_spacing.ml";
    Path.v "packages/krasny/tests/fixtures/0820_module_let_parameter_stability.ml";
    Path.v "packages/krasny/tests/fixtures/0821_nested_module_body_attribute_relift_fallback.ml";
    Path.v "packages/krasny/tests/fixtures/0900_trivia_and_mixed_top_level.ml";
    Path.v "packages/krasny/tests/fixtures/0901_exception_followed_by_section_comment.ml";
    Path.v "packages/krasny/tests/fixtures/0902_docstring_between_multiline_lets.ml";
    Path.v "packages/krasny/tests/fixtures/0903_docstring_after_function_binding.ml";
    Path.v "packages/krasny/tests/fixtures/0904_comments_inside_if_binding.ml";
    Path.v "packages/krasny/tests/fixtures/0905_docstring_after_explicit_fun_with_inner_comment.ml";
    Path.v "packages/krasny/tests/fixtures/0906_docstring_after_fun_with_local_exception.ml";
    Path.v "packages/krasny/tests/fixtures/0907_docstring_after_fun_with_local_exception_and_try.ml";
    Path.v "packages/krasny/tests/fixtures/0908_trailing_inline_comment_in_module.ml";
    Path.v "packages/krasny/tests/fixtures/0909_comment_stability_between_lets.ml";
    Path.v "packages/krasny/tests/fixtures/0910_docstring_before_local_open_let.ml";
    Path.v "packages/krasny/tests/fixtures/0911_comment_before_sequence_let.ml";
    Path.v "packages/krasny/tests/fixtures/0912_banner_comments_followed_by_plain_comment.ml";
    Path.v "packages/krasny/tests/fixtures/0913_structure_docstring_after_open.ml";
    Path.v "packages/krasny/tests/fixtures/0914_docstring_then_plain_comment_before_let.ml";
    Path.v "packages/krasny/tests/fixtures/0915_signature_type_trailing_docstring_at_eof.mli";
    Path.v "packages/krasny/tests/fixtures/0916_signature_type_trailing_comment_at_eof.mli";
    Path.v "packages/krasny/tests/fixtures/0917_signature_type_docstring_between_declarations.mli";
    Path.v "packages/krasny/tests/fixtures/0918_nested_signature_terminal_doc_before_end.mli";
    Path.v "packages/krasny/tests/fixtures/0919_nested_structure_terminal_comment_before_end.ml";
    Path.v "packages/krasny/tests/fixtures/0920_signature_grouped_type_comment_before_and_member.mli";
    Path.v "packages/krasny/tests/fixtures/0921_signature_grouped_type_doc_before_and_member.mli";
    Path.v "packages/krasny/tests/fixtures/0922_signature_variant_inline_record_terminal_doc_before_closing_brace.mli";
    Path.v "packages/krasny/tests/fixtures/0923_nested_signature_prefix_docs.mli";
    Path.v "packages/krasny/tests/fixtures/0924_type_declaration_attribute.ml";
    Path.v "packages/krasny/tests/fixtures/0925_comment_before_and_binding.ml";
    Path.v "packages/krasny/tests/fixtures/0926_if_branch_comments.ml";
    Path.v "packages/krasny/tests/fixtures/0927_leading_comment_dedup.ml";
    Path.v "packages/krasny/tests/fixtures/0928_polyvariant_record_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0929_labeled_alias_parameter.ml";
    Path.v "packages/krasny/tests/fixtures/0930_structure_attribute_before_let.ml";
    Path.v "packages/krasny/tests/fixtures/0933_comment_attribute_between_lets.ml";
    Path.v "packages/krasny/tests/fixtures/0934_typed_labeled_binding_header.ml";
    Path.v "packages/krasny/tests/fixtures/0935_unary_value_declaration.mli";
    Path.v "packages/krasny/tests/fixtures/0936_nested_signature_value.mli";
    Path.v "packages/krasny/tests/fixtures/0937_match_case_body_comment.ml";
    Path.v "packages/krasny/tests/fixtures/0944_exception_separator_comments.ml";
    Path.v "packages/krasny/tests/fixtures/0950_extensible_type_spacing.mli";
    Path.v "packages/krasny/tests/fixtures/0951_record_expression_field_spacing.ml";
    Path.v "packages/krasny/tests/fixtures/0952_multiline_list_expression_no_trailing_separator.ml";
    Path.v "packages/krasny/tests/fixtures/0953_type_alias_function_spacing.mli";
    Path.v "packages/krasny/tests/fixtures/0954_record_type_terminal_semicolon.mli";
    Path.v "packages/krasny/tests/fixtures/0955_signature_type_doc_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0956_record_type_field_docs_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0957_signature_value_doc_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0958_inline_record_type_field_docs_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0959_signature_module_type_doc_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0960_signature_external_doc_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0961_signature_exception_doc_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0962_signature_value_trailing_comment_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0963_signature_module_type_doc_between_items.mli";
    Path.v "packages/krasny/tests/fixtures/0964_nested_signature_value_docs_not_duplicated.mli";
    Path.v "packages/krasny/tests/fixtures/0965_nested_signature_value_trailing_comment_not_duplicated.mli";
    Path.v "packages/krasny/tests/fixtures/0966_inline_record_constructor_terminal_semicolon.ml";
    Path.v "packages/krasny/tests/fixtures/0967_type_and_doc_preserved.mli";
    Path.v "packages/krasny/tests/fixtures/0968_terminal_variant_constructor_doc.mli";
    Path.v "packages/krasny/tests/fixtures/0969_terminal_variant_payload_doc.mli";
    Path.v "packages/krasny/tests/fixtures/0970_signature_value_tight_colon.mli";
    Path.v "packages/krasny/tests/fixtures/0971_external_tight_colon.ml";
    Path.v "packages/krasny/tests/fixtures/0972_module_signature_tight_colon.mli";
    Path.v "packages/krasny/tests/fixtures/0973_record_type_field_tight_colon.mli";
    Path.v "packages/krasny/tests/fixtures/0974_expression_ascription_parenthesized.ml";
    Path.v "packages/krasny/tests/fixtures/0975_typed_pattern_tight_colon.ml";
    Path.v "packages/krasny/tests/fixtures/0976_object_type_field_tight_colon.mli";
    Path.v "packages/krasny/tests/fixtures/0977_class_type_field_tight_colon.mli";
    Path.v "packages/krasny/tests/fixtures/0978_variant_result_tight_colon.mli";
    Path.v "packages/krasny/tests/fixtures/0979_nested_redundant_parens.ml";
    Path.v "packages/krasny/tests/fixtures/0980_fun_breaks_after_arrow.ml";
    Path.v "packages/krasny/tests/fixtures/0981_top_level_letrec_blank_line.ml";
    Path.v "packages/krasny/tests/fixtures/0982_inline_fun_head_with_multiline_body.ml";
    Path.v "packages/krasny/tests/fixtures/0983_record_expression_final_nested_record_separator.ml";
    Path.v "packages/krasny/tests/fixtures/0984_record_expression_final_if_separator.ml";
    Path.v "packages/krasny/tests/fixtures/0985_inline_record_expression_spacing.ml";
    Path.v "packages/krasny/tests/fixtures/0986_inline_record_update_spacing.ml";
    Path.v "packages/krasny/tests/fixtures/0987_inline_constructor_record_spacing.mli";
  ]
  in
  let rec loop errors = function
    | [] -> (
        match List.reverse errors with
        | [] -> Ok ()
        | errors -> Error (String.concat "\n\n" errors)
      )
    | path :: rest -> (
        match assert_lower2_fixture_matches_approved path with
        | Ok () -> loop errors rest
        | Error error -> loop (error :: errors) rest
      )
  in
  loop [] (List.filter fixtures ~fn:parser2_formatter_fixture_supported)

let assert_lower2_manifest_fixtures = fun () ->
  let rec loop errors = function
    | [] -> (
        match List.reverse errors with
        | [] -> Ok ()
        | errors -> Error (String.concat "\n\n" errors)
      )
    | path :: rest -> (
        match assert_lower2_fixture_matches_approved path with
        | Ok () -> loop errors rest
        | Error error -> loop (error :: errors) rest
      )
  in
  loop []
    (manifest_fixture_paths () |> List.filter ~fn:parser2_formatter_fixture_supported)

let tests = [
  Test.case
    "lower2 keeps empty implementations empty"
    (fun _ctx -> assert_format2_ml ~expected:"" "");
  Test.case
    "lower2 formats simple let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let x = 1 + 2\n" "let x = 1 + 2\n");
  Test.case
    "lower2 formats parameterized let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let id x = x\n" "let id x = x\n");
  Test.case
    "lower2 formats typed let binding heads"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [ "let value: int = 1"; "let id x: int = x"; "let keep_pattern (x: int) = x" ])
        "let value : int = 1\nlet id x : int = x\nlet keep_pattern (x : int) = x\n");
  Test.case
    "lower2 formats quoted poly let annotations"
    (fun _ctx ->
      assert_format2_ml ~expected:"let id:\n  'a 'b. 'a ->\n  'b ->\n  'a = fun x _ -> x\n" "let id : 'a 'b. 'a -> 'b -> 'a = fun x _ -> x\n");
  Test.case
    "lower2 keeps locally abstract type binding prefixes"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let make:\n  type socket err. reader:(socket, err) reader ->\n  writer:(socket, err) writer ->\n  of_io_error:(err -> error) ->\n  uri:uri ->\n  t = fun ~reader ~writer ~of_io_error ~uri -> make_conn reader writer of_io_error uri\n"
        "let make : type socket err. reader:(socket, err) reader -> writer:(socket, err) writer -> of_io_error:(err -> error) -> uri:uri -> t = fun ~reader ~writer ~of_io_error ~uri -> make_conn reader writer of_io_error uri\n");
  Test.case
    "lower2 formats mutual recursive let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let rec f = g\n\nand g = f\n" "let rec f = g\nand g = f\n");
  Test.case
    "lower2 formats local let expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let x =\n  let y = 1 in\n  y\n" "let x = let y = 1 in y\n");
  Test.case
    "lower2 formats function expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let id = fun x -> x\n" "let id = fun x -> x\n");
  Test.case
    "lower2 formats function expressions with return annotations"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let boxed = fun (value: int): int -> value\n"
        "let boxed = fun (value: int) : int -> value\n");
  Test.case
    "lower2 formats match expressions"
    (fun _ctx ->
      assert_format2_ml ~expected:"let value =\n  match x with\n  | 0 -> 1\n  | _ -> 2\n" "let value = match x with | 0 -> 1 | _ -> 2\n");
  Test.case
    "lower2 formats sequence expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let run =\n  first;\n  second\n" "let run = first; second\n");
  Test.case
    "lower2 preserves trailing sequence bodies in and-bindings"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let rec f () =\n  log \"f\";\n\nand g () =\n  log \"g\";\n"
        "let rec f () = log \"f\";\nand g () = log \"g\";\n");
  Test.case
    "lower2 formats list and array expressions"
    (fun _ctx ->
      assert_format2_ml ~expected:(top_level [ "let values = [ 1; 2 ]"; "let array = [|1; 2|]" ]) "let values = [1; 2]\nlet array = [|1; 2|]\n");
  Test.case
    "lower2 preserves parens around function application arguments"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let folded = List.fold_left (fun acc doc -> (indent, doc) :: acc) rest\n"
        "let folded = List.fold_left (fun acc doc -> (indent, doc) :: acc) rest\n");
  Test.case
    "lower2 formats labels and optional labels"
    (fun _ctx -> assert_format2_ml ~expected:"let f ~x ?y = g ~x ?y\n" "let f ~x ?y = g ~x ?y\n");
  Test.case
    "lower2 formats polymorphic variants"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [ "let ok = `Ok 1"; "let classify = function\n  | `Ok value -> value\n  | `Error -> 0" ])
        "let ok = `Ok 1\nlet classify = function | `Ok value -> value | `Error -> 0\n");
  Test.case
    "lower2 preserves parens around polymorphic variant payload apply arguments"
    (fun _ctx -> assert_format2_ml ~expected:"let wrapped = Some (`Tag { value = 1 })\n" "let wrapped = Some (`Tag { value = 1 })\n");
  Test.case
    "lower2 keeps simple polymorphic variant payload args bare"
    (fun _ctx ->
      assert_format2_ml ~expected:"let color_escape color = Color.to_escape_seq ~mode:`fg color\n" "let color_escape color = Color.to_escape_seq ~mode:`fg color\n");
  Test.case
    "lower2 formats expression and pattern attributes"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level [ "let value = target [@inline always]"; "let (x [@foo]) = value" ])
        "let value = target [@inline always]\nlet (x [@foo]) = value\n");
  Test.case
    "lower2 formats expression pattern and item extensions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let value = [%expr payload]";
            "let [%pat payload] = value";
            "[%%item payload]";
            "[@@@warning \"-32\"]";
          ])
        "let value = [%expr payload]\nlet [%pat payload] = value\n[%%item payload]\n[@@@warning \"-32\"]\n");
  Test.case
    "lower2 formats signature extension and attribute items"
    (fun _ctx ->
      assert_format2_mli
        ~expected:(top_level [ "[%%foo payload]"; "[@@@warning \"-32\"]"; "val id: int" ])
        "[%%foo payload]\n[@@@warning \"-32\"]\nval id : int\n");
  Test.case
    "lower2 formats selectors and index expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [ "let field = value.name"; "let item = values.(index)"; "let char = text.[index]" ])
        "let field = value.name\nlet item = values.(index)\nlet char = text.[index]\n");
  Test.case
    "lower2 keeps index expressions bare in apply arguments"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let char_ok = identifier_character source.[index - 1]";
            "let item_ok = consume values.(index)"
          ])
        "let char_ok = identifier_character source.[index - 1]\nlet item_ok = consume values.(index)\n");
  Test.case
    "lower2 formats record expressions and patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let record = { x = 1; y }";
            "let updated = { base with x = 2; y }";
            "let qualified = { History.name = name; statistics = stats }";
            "let { x; y = z; _ } = record";
          ])
        "let record = { x = 1; y }\nlet updated = { base with x = 2; y }\nlet qualified = { History.name = name; statistics = stats }\nlet { x; y = z; _ } = record\n");
  Test.case
    "lower2 formats binding operator expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let value = let* x = fetch in let+ y = decode in pair x y";
            "let both = let+ x = a and+ y = b in pair x y";
          ])
        "let value = let* x = fetch in let+ y = decode in pair x y\nlet both = let+ x = a and+ y = b in pair x y\n");
  Test.case
    "lower2 formats local open expressions and patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let value = let open Foo.Bar in result";
            "let scoped = send pid Server.(Telemetry (Stop { reply_to = self (); request_id }))";
            "let store = Contentstore.create ~root:Path.(tmpdir / Path.v \"cache\") ~ns:(namespace parts)";
            "let Foo.Bar.(x) = value"
          ])
        "let value = let open Foo.Bar in result\nlet scoped = send pid Server.(Telemetry (Stop { reply_to = self (); request_id }))\nlet store = Contentstore.create ~root:Path.(tmpdir / Path.v \"cache\") ~ns:(namespace parts)\nlet Foo.Bar.(x) = value\n");
  Test.case
    "lower2 formats first-class module expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let packed = (module Foo.Bar)";
            "let typed = (module Foo : S.T)";
            "let advanced = (module Foo : S with type t = item and type state = state)";
          ])
        "let packed = (module Foo.Bar)\nlet typed = (module Foo : S.T)\nlet advanced = (module Foo : S with type t = item and type state = state)\n");
  Test.case
    "lower2 formats locally abstract and first-class module patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let f (type a b) (module M : S.T) = value";
            "let g (module _) = value";
            "let h (module N : S with type t = item) = value";
          ])
        "let f (type a b) (module M : S.T) = value\nlet g (module _) = value\nlet h (module N : S with type t = item) = value\n");
  Test.case
    "lower2 formats package type value declarations"
    (fun _ctx ->
      assert_format2_mli
        ~expected:"val get: (module ConfigSpec with type t = 'a) -> ('a, error) result\n"
        "val get: (module ConfigSpec with type t = 'a) -> ('a, error) result\n");
  Test.case
    "lower2 formats let module expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let value = let module M = Foo.Bar in result";
            "let empty = let module Empty = struct end in done_";
            "let packed = let module D = (val driver : Driver with type t = _) in body";
          ])
        "let value = let module M = Foo.Bar in result\nlet empty = let module Empty = struct end in done_\nlet packed = let module D = (val driver : Driver with type t = _) in body\n");
  Test.case
    "lower2 formats let exception expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let value = let exception Local of int * Foo.t in result";
            "let bare = let exception Done in done_";
          ])
        "let value = let exception Local of int * Foo.t in result\nlet bare = let exception Done in done_\n");
  Test.case
    "lower2 formats unreachable expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let value =\n  match maybe with\n  | Some value -> value\n  | None -> .\n"
        "let value = match maybe with | Some value -> value | None -> .\n");
  Test.case
    "lower2 formats assertion and lazy expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level [ "let _ = assert ready"; "let later = lazy compute" ])
        "let _ = assert ready\nlet later = lazy compute\n");
  Test.case
    "lower2 formats try expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let value =\n  try read () with\n  | Failure -> 0\n" "let value = try read () with | Failure -> 0\n");
  Test.case
    "lower2 formats while and for loops"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let poll = while ready do step () done";
            "let up = for i = 0 to n do step i done";
            "let down = for i = n downto 0 do step i done";
          ])
        "let poll = while ready do step () done\nlet up = for i = 0 to n do step i done\nlet down = for i = n downto 0 do step i done\n");
  Test.case
    "lower2 formats lazy exception and interval patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let force = function\n  | lazy value -> value";
            "let recovered =\n  match read () with\n  | exception Failure -> 0\n  | value -> value";
            "let classify = function\n  | 'a' .. 'z' -> 1\n  | _ -> 0";
          ])
        "let force = function | lazy value -> value\nlet recovered = match read () with | exception Failure -> 0 | value -> value\nlet classify = function | 'a' .. 'z' -> 1 | _ -> 0\n");
  Test.case
    "lower2 adds a final newline"
    (fun _ctx -> assert_format2_ml ~expected:"let x = 1\n" "let x = 1");
  Test.case
    "lower2 formats open declarations"
    (fun _ctx -> assert_format2_ml ~expected:"open Foo.Bar\n" "open Foo.Bar\n");
  Test.case
    "lower2 formats simple include external and exception declarations"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "include Foo.Bar";
            "external id: 'a -> 'a = \"%identity\" \"caml_id\"";
            "exception Boom"
          ])
        "include Foo.Bar\nexternal id : 'a -> 'a = \"%identity\" \"caml_id\"\nexception Boom\n");
  Test.case
    "lower2 formats type extensions and structured exception rhs"
    (fun _ctx ->
      assert_format2_mli
        ~expected:(top_level
          [
            "type 'a box +=\n  | More of 'a";
            "exception Parse_error of string";
            "exception Nested = Std.Result.Error";
          ])
        "type 'a box += | More of 'a\nexception Parse_error of string\nexception Nested = Std.Result.Error\n");
  Test.case
    "lower2 formats simple module and module type declarations"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "module Alias = Foo.Bar";
            "module Empty = struct end";
            "module type S = Foo.S";
            "module type Empty = sig end";
          ])
        "module Alias = Foo.Bar\nmodule Empty = struct end\nmodule type S = Foo.S\nmodule type Empty = sig end\n");
  Test.case
    "lower2 formats constrained module type declarations"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [ "module type S = Driver with type config = int and module Nested = Impl"; ])
        "module type S = Driver with type config = int and module Nested = Impl\n");
  Test.case
    "lower2 formats simple signature module declarations"
    (fun _ctx ->
      assert_format2_mli
        ~expected:(top_level
          [
            "module Alias: Foo.S";
            "module Http1: module type of Foo.Bar";
            "module Empty: sig end";
            "module type S = Foo.S";
            "module type Abstract"
          ])
        "module Alias : Foo.S\nmodule Http1 : module type of Foo.Bar\nmodule Empty : sig end\nmodule type S = Foo.S\nmodule type Abstract\n");
  Test.case
    "lower2 formats type aliases with parameters"
    (fun _ctx -> assert_format2_mli ~expected:"type 'a t = 'a list\n" "type 'a t = 'a list\n");
  Test.case
    "lower2 formats tuple type separators structurally"
    (fun _ctx ->
      assert_format2_mli
        ~expected:"type ('a, 'e) result_like = ('a, 'e) result\ntype pair = int * string\n"
        "type ('a, 'e) result_like = ('a, 'e) result\ntype pair = int * string\n");
  Test.case
    "lower2 formats simple value declarations"
    (fun _ctx -> assert_format2_mli ~expected:"val id: 'a -> 'a\n" "val id : 'a -> 'a\n");
  Test.case
    "lower2 rejects unsupported shapes instead of replaying source"
    (fun _ctx -> assert_format2_ml_fails "let object_value = object end\n");
  Test.case
    ~size:Large "lower2 formats the existing fixture corpus idempotently"
    (fun _ctx -> assert_lower2_manifest_fixtures ());
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"krasny:lower2" ~tests ~args ())
    ~args:Env.args
    ()
