open Std

(** Typ - Experimental library-first type analysis for Riot

    `typ` is organized into sublibraries:

    - `Typ.Model`
      shared type model, semantic layers, ids, summaries, and reusable
      persisted typing artifacts
    - `Typ.Analysis`
      analysis products such as check results and type indexes
    - `Typ.Lower`
      CST-to-semantic-tree lowering and origin/binding construction
    - `Typ.Infer`
      solver state, regions, and the prototype inference engine
    - `Typ.Event`
      structured debug/progress events emitted by rooted snapshot preparation
      and source analysis
    - `Typ.Diagnostics`
      user-facing explanations and report rendering
    - `Typ.Query`
      read-only query surface over session snapshots
    - `Typ.Session`
      long-lived host sessions, rooted snapshots, and persistence plumbing

    Convenience wrappers remain available at:

    - `Typ.Batch.check_source`
    - `Typ.Check.check_source`

    These one-shot wrappers still require host-prepared `Syn.parse` and
    successful `Syn.build_cst` artifacts; `typ` no longer reparses source text
    internally.
*)
module Model: module type of Model

module Analysis: module type of Analysis

module Lower: module type of Lower

module Infer: module type of Infer

module Event: module type of Event

module Diagnostics: module type of Diagnostics

module Query: module type of Query

module Session: module type of Session

module Store: module type of Store

module Batch: module type of Batch

module Check: module type of Check

module Config: module type of TypConfig
