module Dep_env = Env

open Std

module Ir = Ir
module Item = Ir.Item

type source_kind = Ir.source_kind =
  | Implementation
  | Interface

type source_summary = Ir.source_summary = {
  source: Path.t;
  source_hash: Crypto.hash;
  module_path: string list option;
  kind: source_kind;
  items: Item.t list;
}

let source_kind_serializer = Ir.source_kind_serializer

let source_summary_serializer = Ir.source_summary_serializer

type provider = Resolver.provider = {
  path: string list;
  free_names: string list;
  exports: string list list;
}

module Env_impl = Dep_env

module Env = struct
  type t = Env_impl.t

  let empty = Env_impl.empty

  let make = Resolver.make_env

  let add_provider = Resolver.add_provider

  let add_providers = Resolver.add_providers

  let add_path = Env_impl.add_path

  let add_external_summaries = Resolver.add_external_summaries

  let with_summaries = Resolver.with_summaries

  let providers = fun (_: t) -> []
end

module Resolution = Resolver.Resolution
module ResolvedSource = Resolver.ResolvedSource

type parse_error = Ir.parse_error =
  | Parse_diagnostics of Syn.Diagnostic.t list

type resolve_error = Resolver.error =
  | Invalid_provider of string

let analyze = Ir.analyze

let collect = Ir.collect

let resolve = Resolver.resolve
