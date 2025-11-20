open Std

(** CodeDB Schema - Attribute URIs for facts *)
module Codedb : sig
  (** Symbol Attributes *)
  
  val kind : Poneglyph.Uri.t
  (** Attribute: Symbol kind (module, value, type, interface) *)
  
  val package : Poneglyph.Uri.t
  (** Attribute: Module belongs to package (value is package URI) *)
  
  val package_name : Poneglyph.Uri.t
  (** Attribute: Package name as string *)
  
  (** File Attributes *)
  
  val path : Poneglyph.Uri.t
  (** Attribute: File path *)
  
  val sha256 : Poneglyph.Uri.t
  (** Attribute: SHA256 hash of file content *)
  
  val size : Poneglyph.Uri.t
  (** Attribute: File size in bytes *)
  
  val modified_at : Poneglyph.Uri.t
  (** Attribute: Last modified timestamp *)
  
  val deleted_at : Poneglyph.Uri.t
  (** Attribute: Deletion timestamp *)
  
  (** Relationships *)
  
  val provided_by : Poneglyph.Uri.t
  (** Attribute: Symbol is provided by a File *)
  
  (** Analysis Attributes *)
  
  val analysis_of : Poneglyph.Uri.t
  (** Attribute: Analysis is of a specific file *)
  
  val analyzed_at : Poneglyph.Uri.t
  (** Attribute: When the analysis was performed *)
  
  val source : Poneglyph.Uri.t
  (** Source URI for all CodeDB facts *)
  
  (** URI Constructors *)
  
  module File : sig
    val uri : path:string -> sha256:string -> Poneglyph.Uri.t
    (** Create a file entity URI with SHA256 *)
  end
  
  module Analysis : sig
    val uri : sha256:string -> Poneglyph.Uri.t
    (** Create an analysis entity URI *)
  end
end

(** OCaml-specific schema attributes *)
module OCaml : sig
  val simple_name : Poneglyph.Uri.t
  val canonical_name : Poneglyph.Uri.t
  val qualified_name : Poneglyph.Uri.t
  val namespace : Poneglyph.Uri.t
  val is_module : Poneglyph.Uri.t
  val implementation_file : Poneglyph.Uri.t
  (** Attribute: URI of the .ml file entity *)
  val interface_file : Poneglyph.Uri.t
  (** Attribute: URI of the .mli file entity *)
  
  module Module : sig
    val uri : string -> Poneglyph.Uri.t
    (** Create a module entity URI from qualified name *)
  end

  module Symbol : sig
    val uri : string -> Poneglyph.Uri.t
    (** Create a symbol entity URI *)
    
    val kind : Poneglyph.Uri.t
  end
end

module Tusk : sig
  module Package : sig
    val uri : string -> Poneglyph.Uri.t
    (** Create a package entity URI from package name *)
  end
end
