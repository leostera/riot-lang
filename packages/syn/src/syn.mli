open Std

(** Syn - OCaml Lexer and Parser

    Syn provides lexical analysis and parsing for OCaml source code, producing
    lossless Ceibo syntax trees that preserve all source information including
    whitespace and comments.

    # Architecture

    Syn is built in layers:

    - **Cursor**: Low-level character stream abstraction
    - **Lexer**: Tokenizes source into a flat token stream
    - **Parser**: Builds Ceibo green trees from tokens
    - **Syntax Trees**: Ceibo red-green trees with full fidelity

    # Key Properties

    - **Lossless**: Every byte of input is preserved in the tree
    - **Error Recovery**: Parser never fails, creates ERROR nodes for problems
    - **Structured Diagnostics**: Machine-readable error information
    - **Red-Green Trees**: Efficient immutable trees with structural sharing

    # Example Usage

    ## Lexing Only

    ```ocaml let source = "let x = 42" in let tokens = Syn.tokenize source in
    List.iter (fun tok -> Printf.printf "%s\n" (Token.show_kind tok.kind) )
    tokens ```

    ## Full Parsing

    ```ocaml let source = "let x = 1 + 2" in let result = Syn.parse source in

    (* Check for errors *) if result.diagnostics != [] then List.iter (fun diag
    -> print ("Error: " ^ (Diagnostic.to_string diag) ^ "\n") )
    result.diagnostics;

    (* Work with the tree *) let root = Ceibo.Red.new_root result.tree in (* ...
    traverse tree ... *) ```

    ## Working with Parse Errors

    The parser never fails - it produces a tree even from malformed code:

    ```ocaml let source = "let x = " in (* incomplete *) let result = Syn.parse
    source in

    (* result.tree is a valid tree with ERROR/MISSING nodes *) (*
    result.diagnostics contains structured error info *)

    match result.diagnostics with | [] -> Printf.printf "No errors\n" | errs ->
    List.iter (fun err -> Printf.printf "%s at %s\n" (Diagnostic.to_string err)
    (Ceibo.Span.to_string err.span) ) errs ``` *)

(** # Module Exports *)

module Ceibo : module type of Ceibo
(** Red-green syntax tree library. See `Ceibo` module documentation. *)

module Error : module type of Error

module Token : module type of Token
(** Token types and utilities. *)

module Keyword : module type of Keyword
(** OCaml keyword definitions. *)

module Cursor : module type of Cursor
(** Low-level character stream cursor. *)

module Lexer : module type of Lexer
(** Lexical analyzer (tokenizer). *)

module SyntaxKind : module type of Syntax_kind
(** Syntax node kind enumeration for OCaml. *)

module Diagnostic : module type of Diagnostic
(** Structured parse error types. *)

module DiagnosticReporter : module type of Diagnostic_reporter

module Parser : module type of Parser
(** OCaml parser that produces Ceibo trees. *)

(** # High-Level API *)

val tokenize : string -> Token.t list
(** `tokenize source` lexes source code into a flat list of tokens.

    This includes all trivia (whitespace, comments) to enable lossless
    reconstruction of the original source.

    Example: ```ocaml let tokens = Syn.tokenize "let x = 42" in (* tokens =
    [Keyword Let; Whitespace; Ident "x"; Whitespace; Eq; ...] *) ``` *)

val parse_interface : string -> Parser.parse_result
(** `parse_interface source` parses .mli source code into a Ceibo green tree
    with diagnostics. *)

val parse_implementation : string -> Parser.parse_result
(** `parse_implementation source` parses .ml source code into a Ceibo green tree
    with diagnostics. *)

val parse : filename:string -> string -> Parser.parse_result
(** `parse ~filename source` parses source code into a Ceibo green tree with
    diagnostics.

    Automatically chooses parse_interface or parse_implementation based on
    filename extension.

    This function **never fails**. Even with syntax errors, it returns a tree
    structure with ERROR/MISSING nodes and a list of diagnostics.

    Returns:
    - `tree`: A Ceibo green tree (lossless, immutable)
    - `diagnostics`: List of parse errors found

    Example: ```ocaml let result = Syn.parse "let x = 1 + 2" in

    (* Use the green tree directly *) Printf.printf "Tree width: %d\n"
    result.tree.width;

    (* Or create a red view for positioned traversal *) let root =
    Ceibo.Red.new_root result.tree in let span = Ceibo.Red.SyntaxNode.span root
    in Printf.printf "Covers: %s\n" (Ceibo.Span.to_string span) ``` *)
