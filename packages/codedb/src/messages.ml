open Std

type add_package ={
      package_name : Model.Package_name.t;
      package_path : Path.t;
    }

type add_module = {
      package_name : Model.Package_name.t;
      source_file : Path.t;
      module_name : Model.Module_name.t;
    }

type get_symbol = {
      caller : Pid.t;
      ref : Model.Symbol.t option Ref.t;
      sym : Model.Symbol.reference;
    }

type request =
  | AddPackage of add_package
  | AddModule of add_module
  | GetSymbol of get_symbol 

type response =
  | GetSymbolResponse of { ref : Model.Symbol.t option Ref.t; result : Model.Symbol.t option }

(** Messages the CodeDB server can receive *)
type Message.t += CodeDbRequest of request | CodeDbResponse of response

(** Response wrapper for queries *)
