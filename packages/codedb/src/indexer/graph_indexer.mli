(** Graph Indexer - Extracts module graph information and stores in Poneglyph *)

(** Index all packages in the workspace and store facts in Poneglyph *)
val index_workspace : config:Config.t -> db:Poneglyph.t -> unit

(** Index a single package *)
val index_package :
  config:Config.t -> db:Poneglyph.t -> Tusk_model.Package.t -> unit

(** Index a single file (for file watcher) *)
val index_file : config:Config.t -> db:Poneglyph.t -> file_path:Std.Path.t -> unit

(** Mark a file as deleted by stating a deleted_at fact *)
val mark_file_deleted : config:Config.t -> db:Poneglyph.t -> file_path:Std.Path.t -> unit
