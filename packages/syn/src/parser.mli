open Std

(** OCaml Parser - Token Stream to Syntax Tree

    This module implements a recursive descent parser that converts a flat token
    stream into a lossless Ceibo syntax tree.

    # Key Characteristics

    - **Never Fails**: Always returns a tree, even from malformed input
    - **Error Recovery**: Creates ERROR/MISSING nodes for syntax errors
    - **Lossless**: Preserves all source information (whitespace, comments)
    - **Structured Errors**: Reports problems as structured diagnostics

    # Architecture

    The parser is a single-pass recursive descent parser that: 1. Consumes
    tokens from the lexer 2. Builds green tree nodes bottom-up 3. Collects
    diagnostics for any problems encountered 4. Never backtracks or fails

    # Error Recovery Strategy

    When the parser encounters unexpected input, it:

    - **Missing Tokens**: Inserts a MISSING node (zero-width placeholder)
    - **Unexpected Tokens**: Wraps in an ERROR node and continues
    - **Invalid Constructs**: Creates ERROR node and skips to recovery point

    This allows the parser to continue and report multiple errors in one pass.

    # Example Usage

    ## Basic Parsing

    ```ocaml let source = "let x = 1 + 2" in let tokens = Lexer.tokenize source
    in let result = Parser.parse ~source tokens in

    (* Always succeeds - check diagnostics for errors *) match
    result.diagnostics with | [] -> Printf.printf "Parsed successfully!\n"; (*
    Work with result.tree *) | errors -> List.iter (fun diag -> Printf.eprintf
    "Error: %s\n" (Diagnostic.to_string diag) ) errors ```

    ## Parsing Malformed Code

    ```ocaml let source = "let x = " in (* incomplete *) let tokens =
    Lexer.tokenize source in let result = Parser.parse ~source tokens in

    (* Still returns a tree! *) Printf.printf "Tree has %d diagnostics\n"
    (List.length result.diagnostics);

    (* The tree contains ERROR/MISSING nodes *) (* Diagnostics describe what
    went wrong *) List.iter (fun diag -> let span_str = Ceibo.Span.to_string
    diag.span in Printf.eprintf "%s at %s\n" (Diagnostic.to_string diag)
    span_str ) result.diagnostics ```

    ## Working with the Result

    ```ocaml let result = Parser.parse ~source tokens in

    (* The green tree is position-independent *) let green_tree = result.tree in
    Printf.printf "Total width: %d bytes\n" green_tree.width;

    (* Create a red view for positioned queries *) let root = Ceibo.Red.new_root
    green_tree in

    (* Traverse the tree *) Ceibo.Red.SyntaxNode.preorder root (function |
    Ceibo.Red.Node node -> let kind = Ceibo.Red.SyntaxNode.kind node in let span
    = Ceibo.Red.SyntaxNode.span node in Printf.printf "%s at %s\n"
    (Syntax_kind.to_string kind) (Ceibo.Span.to_string span) | Ceibo.Red.Token
    tok -> let text = Ceibo.Red.SyntaxToken.text tok in Printf.printf " Token:
    %s\n" text ) ``` *)
(** # Types *)

(** Result of parsing.

    The parser **always** returns a parse result, even if the source code is
    malformed. The `diagnostics` field indicates whether there were any
    problems.

    This design enables:
    - IDE features that work on incomplete code
    - Reporting multiple errors in one pass
    - Incremental parsing (unchanged subtrees can be reused) *)
type parse_result = {
  source: string;
  (** The original source text that produced this parse result. *)
  tokens: Token.t list;
  (** The original lexer token stream, including token-attached trivia and
      `EOF.leading_trivia` for trailing file trivia. *)
  kind: 
    [
      | `Implementation
      | `Interface
    ];
  (** Which file grammar produced this parse result. *)
  tree: (Syntax_kind.t, string) Ceibo.Green.node;
  (** The parsed green tree.

    This is an immutable, position-independent tree that preserves all
          source information. It may contain ERROR and MISSING nodes if the
          source had syntax errors. *)
  diagnostics: Diagnostic.t list;
  (** List of parse errors and warnings.

          Empty list means no errors were found. Each diagnostic describes a
          specific problem with its location in the source. *)
}
(** # Parsing *)
(** Parse an interface file (.mli) *)
val parse_interface: source:string -> Token.t list -> parse_result

(** Parse an implementation file (.ml) *)
val parse_implementation: source:string -> Token.t list -> parse_result
