open Std

(* OCaml syntax node kinds for Ceibo green trees *)
type t =
  (* ========================================================================= *)
  (* TRIVIA - Whitespace and comments *)
  (* ========================================================================= *)
  | WHITESPACE
  | COMMENT
  | DOCSTRING
  (* ========================================================================= *)
  (* LITERALS *)
  (* ========================================================================= *)
  | INT_LITERAL
  | FLOAT_LITERAL
  | STRING_LITERAL
  | CHAR_LITERAL
  | BOOL_LITERAL
  | UNIT_LITERAL
  (* ========================================================================= *)
  (* EXPRESSIONS *)
  (* ========================================================================= *)
  | IDENT_EXPR
  | PATH_EXPR (* Module.path.to.value *)
  | APPLY_EXPR (* f x y *)
  | INFIX_EXPR (* x + y *)
  | PREFIX_EXPR (* -x, !ref *)
  | IF_EXPR (* if c then e1 else e2 *)
  | MATCH_EXPR (* match e with ... *)
  | FUN_EXPR (* fun x -> e *)
  | FUNCTION_EXPR (* function | p1 -> e1 | ... *)
  | LET_EXPR (* let x = e1 in e2 *)
  | LET_REC_EXPR (* let rec f x = e1 in e2 *)
  | SEQUENCE_EXPR (* e1; e2; e3 *)
  | PAREN_EXPR (* (e) *)
  | TUPLE_EXPR (* (e1, e2, e3) *)
  | LIST_EXPR (* [e1; e2; e3] *)
  | ARRAY_EXPR (* [|e1; e2; e3|] *)
  | RECORD_EXPR (* { field1 = e1; field2 = e2 } *)
  | RECORD_UPDATE_EXPR (* { record with field = e } *)
  | FIELD_ACCESS_EXPR (* record.field *)
  | ARRAY_INDEX_EXPR (* arr.(i) *)
  | STRING_INDEX_EXPR (* s.[i] *)
  | CONSTRUCTOR_EXPR (* Some e, Ok value *)
  | POLY_VARIANT_EXPR (* `Tag or `Tag value *)
  | ASSERT_EXPR (* assert e *)
  | LAZY_EXPR (* lazy e *)
  | WHILE_EXPR (* while c do e done *)
  | FOR_EXPR (* for x = e1 to e2 do e3 done *)
  | TRY_EXPR (* try e with | p1 -> e1 | ... *)
  | TYPED_EXPR (* (e : t) *)
  | COERCE_EXPR (* (e :> t) or (e : t1 :> t2) *)
  (* ========================================================================= *)
  (* PATTERNS *)
  (* ========================================================================= *)
  | IDENT_PATTERN
  | WILDCARD_PATTERN (* _ *)
  | LITERAL_PATTERN
  | CONSTRUCTOR_PATTERN (* Some x *)
  | TUPLE_PATTERN (* (x, y, z) *)
  | LIST_PATTERN (* [x; y; z] *)
  | ARRAY_PATTERN (* [|x; y; z|] *)
  | CONS_PATTERN (* x :: xs *)
  | RECORD_PATTERN (* { field1; field2 = p } *)
  | OR_PATTERN (* p1 | p2 *)
  | AS_PATTERN (* p as x *)
  | TYPED_PATTERN (* (p : t) *)
  | LAZY_PATTERN (* lazy p *)
  | EXCEPTION_PATTERN (* exception p *)
  | PAREN_PATTERN (* (p) *)
  | POLY_VARIANT_PATTERN (* `Tag or `Tag p *)
  (* ========================================================================= *)
  (* TYPE EXPRESSIONS *)
  (* ========================================================================= *)
  | TYPE_VAR (* 'a, 'b *)
  | TYPE_CONSTR (* int, string, list *)
  | TYPE_ARROW (* int -> string *)
  | TYPE_TUPLE (* int * string *)
  | TYPE_PAREN (* (int -> string) *)
  | TYPE_POLY_VARIANT (* [`A | `B] *)
  | TYPE_PARAM (* 'a in type params *)
  | TYPE_PARAMS (* ('a, 'b) *)
  | TYPE_VARIANT_CONSTR (* A | B of int *)
  | TYPE_RECORD_FIELD (* field: int *)
  | TYPE_CONSTRAINT (* constraint 'a = int *)
  (* ========================================================================= *)
  (* TOP-LEVEL DECLARATIONS *)
  (* ========================================================================= *)
  | LET_BINDING (* let x = e *)
  | LET_REC_BINDING (* let rec f x = e *)
  | TYPE_DECL (* type t = ... *)
  | EXCEPTION_DECL (* exception E of t *)
  | MODULE_DECL (* module M = struct ... end *)
  | MODULE_TYPE_DECL (* module type S = sig ... end *)
  | OPEN_STMT (* open Module *)
  | INCLUDE_STMT (* include Module *)
  | EXTERNAL_DECL (* external name : type = "c_name" *)
  (* ========================================================================= *)
  (* STRUCTURAL ELEMENTS *)
  (* ========================================================================= *)
  | SOURCE_FILE (* Top-level file *)
  | STRUCTURE (* struct ... end *)
  | SIGNATURE (* sig ... end *)
  | MATCH_CASE (* | pattern -> expr *)
  | PATTERN_GUARD (* when expr *)
  | RECORD_FIELD (* field = expr *)
  | RECORD_FIELD_PATTERN (* field = pattern *)
  | PARAMETER (* Function parameter *)
  | ARGUMENT (* Function argument *)
  (* ========================================================================= *)
  (* ERROR RECOVERY *)
  (* ========================================================================= *)
  | ERROR (* Unparseable content *)
  | MISSING (* Expected but missing token/node *)

let to_string = function
  | WHITESPACE -> "WHITESPACE"
  | COMMENT -> "COMMENT"
  | DOCSTRING -> "DOCSTRING"
  | INT_LITERAL -> "INT_LITERAL"
  | FLOAT_LITERAL -> "FLOAT_LITERAL"
  | STRING_LITERAL -> "STRING_LITERAL"
  | CHAR_LITERAL -> "CHAR_LITERAL"
  | BOOL_LITERAL -> "BOOL_LITERAL"
  | UNIT_LITERAL -> "UNIT_LITERAL"
  | IDENT_EXPR -> "IDENT_EXPR"
  | PATH_EXPR -> "PATH_EXPR"
  | APPLY_EXPR -> "APPLY_EXPR"
  | INFIX_EXPR -> "INFIX_EXPR"
  | PREFIX_EXPR -> "PREFIX_EXPR"
  | IF_EXPR -> "IF_EXPR"
  | MATCH_EXPR -> "MATCH_EXPR"
  | FUN_EXPR -> "FUN_EXPR"
  | FUNCTION_EXPR -> "FUNCTION_EXPR"
  | LET_EXPR -> "LET_EXPR"
  | LET_REC_EXPR -> "LET_REC_EXPR"
  | SEQUENCE_EXPR -> "SEQUENCE_EXPR"
  | PAREN_EXPR -> "PAREN_EXPR"
  | TUPLE_EXPR -> "TUPLE_EXPR"
  | LIST_EXPR -> "LIST_EXPR"
  | ARRAY_EXPR -> "ARRAY_EXPR"
  | RECORD_EXPR -> "RECORD_EXPR"
  | RECORD_UPDATE_EXPR -> "RECORD_UPDATE_EXPR"
  | FIELD_ACCESS_EXPR -> "FIELD_ACCESS_EXPR"
  | ARRAY_INDEX_EXPR -> "ARRAY_INDEX_EXPR"
  | STRING_INDEX_EXPR -> "STRING_INDEX_EXPR"
  | CONSTRUCTOR_EXPR -> "CONSTRUCTOR_EXPR"
  | POLY_VARIANT_EXPR -> "POLY_VARIANT_EXPR"
  | ASSERT_EXPR -> "ASSERT_EXPR"
  | LAZY_EXPR -> "LAZY_EXPR"
  | WHILE_EXPR -> "WHILE_EXPR"
  | FOR_EXPR -> "FOR_EXPR"
  | TRY_EXPR -> "TRY_EXPR"
  | TYPED_EXPR -> "TYPED_EXPR"
  | COERCE_EXPR -> "COERCE_EXPR"
  | IDENT_PATTERN -> "IDENT_PATTERN"
  | WILDCARD_PATTERN -> "WILDCARD_PATTERN"
  | LITERAL_PATTERN -> "LITERAL_PATTERN"
  | CONSTRUCTOR_PATTERN -> "CONSTRUCTOR_PATTERN"
  | TUPLE_PATTERN -> "TUPLE_PATTERN"
  | LIST_PATTERN -> "LIST_PATTERN"
  | ARRAY_PATTERN -> "ARRAY_PATTERN"
  | CONS_PATTERN -> "CONS_PATTERN"
  | RECORD_PATTERN -> "RECORD_PATTERN"
  | OR_PATTERN -> "OR_PATTERN"
  | AS_PATTERN -> "AS_PATTERN"
  | TYPED_PATTERN -> "TYPED_PATTERN"
  | LAZY_PATTERN -> "LAZY_PATTERN"
  | EXCEPTION_PATTERN -> "EXCEPTION_PATTERN"
  | PAREN_PATTERN -> "PAREN_PATTERN"
  | POLY_VARIANT_PATTERN -> "POLY_VARIANT_PATTERN"
  | TYPE_VAR -> "TYPE_VAR"
  | TYPE_CONSTR -> "TYPE_CONSTR"
  | TYPE_ARROW -> "TYPE_ARROW"
  | TYPE_TUPLE -> "TYPE_TUPLE"
  | TYPE_PAREN -> "TYPE_PAREN"
  | TYPE_POLY_VARIANT -> "TYPE_POLY_VARIANT"
  | TYPE_PARAM -> "TYPE_PARAM"
  | TYPE_PARAMS -> "TYPE_PARAMS"
  | TYPE_VARIANT_CONSTR -> "TYPE_VARIANT_CONSTR"
  | TYPE_RECORD_FIELD -> "TYPE_RECORD_FIELD"
  | TYPE_CONSTRAINT -> "TYPE_CONSTRAINT"
  | LET_BINDING -> "LET_BINDING"
  | LET_REC_BINDING -> "LET_REC_BINDING"
  | TYPE_DECL -> "TYPE_DECL"
  | EXCEPTION_DECL -> "EXCEPTION_DECL"
  | MODULE_DECL -> "MODULE_DECL"
  | MODULE_TYPE_DECL -> "MODULE_TYPE_DECL"
  | OPEN_STMT -> "OPEN_STMT"
  | INCLUDE_STMT -> "INCLUDE_STMT"
  | EXTERNAL_DECL -> "EXTERNAL_DECL"
  | SOURCE_FILE -> "SOURCE_FILE"
  | STRUCTURE -> "STRUCTURE"
  | SIGNATURE -> "SIGNATURE"
  | MATCH_CASE -> "MATCH_CASE"
  | PATTERN_GUARD -> "PATTERN_GUARD"
  | RECORD_FIELD -> "RECORD_FIELD"
  | RECORD_FIELD_PATTERN -> "RECORD_FIELD_PATTERN"
  | PARAMETER -> "PARAMETER"
  | ARGUMENT -> "ARGUMENT"
  | ERROR -> "ERROR"
  | MISSING -> "MISSING"
