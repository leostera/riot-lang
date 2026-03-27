open Std
open Std.Collections

type format_error = Format_core.format_error =
  | Cannot_build_cst of Syn.build_cst_error

module Doc = Doc
module Solver = Solver
module Printer = Printer
module Source = Source
module Lower = Lower
module Runner = Runner
module Report = Report

let format_error_to_string = Format_core.format_error_to_string
let format = Format_core.format

(* TODO(@leostera): rewrite this hasher to use Crypto.Sha521.create () *)
let hash_green_tree tree =
  let buffer = IO.Buffer.create 1024 in
  let rec write_element = function
    | Syn.Ceibo.Green.Token _ as element -> (
        match Syn.Ceibo.Green.kind element with
        | Syn.SyntaxKind.WHITESPACE -> ()
        | kind ->
            IO.Buffer.add_string buffer "T(";
            IO.Buffer.add_string buffer (Syn.SyntaxKind.to_string kind);
            IO.Buffer.add_string buffer ":";
            IO.Buffer.add_string buffer
              (Syn.Ceibo.Green.text element |> Option.expect ~msg:"token text");
            IO.Buffer.add_string buffer ")")
    | Syn.Ceibo.Green.Node node as element ->
        IO.Buffer.add_string buffer "N(";
        IO.Buffer.add_string buffer
          (Syn.SyntaxKind.to_string (Syn.Ceibo.Green.kind element));
        IO.Buffer.add_string buffer "[";
        Array.iter write_element (Syn.Ceibo.Green.children node);
        IO.Buffer.add_string buffer "])"
  in
  write_element (Syn.Ceibo.Green.Node tree);
  IO.Buffer.contents buffer |> Crypto.hash_string |> Crypto.Digest.hex

(* TODO(@leostera): rewrite this hasher to use Crypto.Sha521.create () *)
let syntax_hash (result : Syn.Parser.parse_result) =
  hash_green_tree result.tree

let write ~writer result =
  match format result with
  | Error err -> Error (`Format err)
  | Ok formatted -> IO.write_all writer ~buf:formatted |> Result.map_error (fun err -> `Write err)
