open Std

(**
   Typ - Riot's type-checker engine

   `typ` is organized into sublibraries:

   - `Typ.Model`
     shared semantic ids, paths, module artifacts, and reusable persisted
     typing data
   - `Typ.Analysis`
     checked-source outputs such as reports and type indexes
   - `Typ.SourceAnalysis`
     core single-source analysis over already prepared sources
   - `Typ.PackageEnv`
     monotonic package-wide module-artifact environment
   - `Typ.ScopeView`
     cheap per-source/group visibility view over `PackageEnv`
   - `Typ.ImportedWorld`
     imported-module lookup boundary consumed by source analysis
   - `Typ.ModulePairing`
     canonical pairing of interface and implementation analyses
   - `Typ.ModuleSurface`
     module-surface qualification and public-view derivation
   - `Typ.Lower`
     CST-to-semantic-tree lowering and origin construction
   - `Typ.Infer`
     solver state, environments, and inference
   - `Typ.Event`
     structured timing and progress events from the checker engine
   - `Typ.Diagnostics`
     user-facing explanations and report rendering
   - `Typ.Query`
     read-only query surface over immutable snapshots
   - `Typ.Session`
     long-lived session and snapshot machinery for query/editor workflows
   - `Typ.Check`
     the authoritative incremental package-check engine

   The hot build-check path is package-oriented and incremental. Query and
   editor workflows remain snapshot-oriented, but they share the same
   semantics and module artifacts.
*)
type config = TypConfig.t

type source = Model.Source.t

type checked_source = Analysis.Check_result.t

(**
   Check one already prepared source.

   The caller is responsible for supplying a [Source.t] that already carries
   the parse result and CST. `typ` does not accept raw source text at this
   boundary.
*)
val check: config:config -> source:source -> checked_source

module Model : module type of Model

module Analysis : module type of Analysis

module SourceAnalysis : module type of SourceAnalysis

module PackageEnv : module type of PackageEnv

module ScopeView : module type of ScopeView

module ImportedWorld : module type of ImportedWorld

module ModulePairing : module type of ModulePairing

module ModuleSurface : module type of ModuleSurface

module Lower : module type of Lower

module Infer : module type of Infer

module Event : module type of Event

module Diagnostics : module type of Diagnostics

module MissingRequirements : module type of MissingRequirements

module Query : module type of Query

module Session : module type of Session

module Store : module type of Store

module Check : module type of Check

module Config : module type of TypConfig
