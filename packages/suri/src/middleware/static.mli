open Std

(**
   {1 Static Files Middleware}

   Serve static files from a directory with security, caching, and optional
   directory browsing.

   {2 Quick Start}

   {[
     (* Serve from ./public at /public URL *)
     let app = Middleware.[
       logger;
       Static.middleware ~at:"/public" (Path.v "./public") ();
       router routes;
     ]
   ]}

   {2 Features}

   - ✅ {b Security}: Path traversal protection, dotfile blocking
   - ✅ {b Performance}: ETag and Last-Modified caching, 304 responses
   - ✅ {b MIME Types}: Automatic detection for 30+ file types
   - ✅ {b Directory Browsing}: Optional HTML listings
   - ✅ {b Custom Headers}: Add security headers, CORS, etc.

   {2 Security}

   The middleware prevents common security issues:
   - {b Path Traversal}: Blocks [../../../etc/passwd] attempts
   - {b Dotfiles}: Blocks [.env], [.git/config] by default
   - {b Symlinks}: Follows or denies based on config
   - {b File Types}: Only serves regular files, not special files

   {2 Examples}

   Basic usage:
   {[
     let app = Middleware.[
       Static.middleware ~at:"/assets" (Path.v "./public") ();
       router routes;
     ]
   ]}

   Custom configuration:
   {[
     let config = Static.{
       default_config with
       show_directory = true;
       dotfiles = AllowDotfiles;
       cache_control = Some "public, max-age=31536000, immutable";
       headers = [
         ("x-content-type-options", "nosniff");
         ("x-frame-options", "DENY");
       ];
     }

     let app = Middleware.[
       Static.middleware ~at:"/public" ~config (Path.v "./public") ();
       router routes;
     ]
   ]}

   Multiple static directories:
   {[
     let app = Middleware.[
       Static.middleware ~at:"/images" (Path.v "./storage/images") ();
       Static.middleware ~at:"/uploads" (Path.v "./uploads") ();
       Static.middleware ~at:"/assets" (Path.v "./public") ();
       router routes;
     ]
   ]}
*)

(** {1 Configuration} *)

type dotfiles =
  | AllowDotfiles
  | DenyDotfiles
  | IgnoreDotfiles
type symlinks =
  | FollowSymlinks
  | DenySymlinks
type config = {
  show_directory: bool;
  (** Enable directory browsing with HTML listings. Default: [false] *)
  index_files: string list;
  (** Index files to try for directories. Default: [["index.html"; "index.htm"]] *)
  dotfiles: dotfiles;
  (** How to handle dotfiles (.env, .git, etc). Default: [DenyDotfiles] *)
  symlinks: symlinks;
  (** How to handle symbolic links. Default: [FollowSymlinks] *)
  headers: (string * string) list;
  (** Additional headers to add to all responses. Default: [[]] *)
  cache_control: string option;
  (** Cache-Control header value. Default: [Some "public, max-age=3600"] *)
}
type path_error =
  | InvalidRoot of {
      root: Path.t;
      error: Fs.error;
    }
  | MissingPath of {
      path: Path.t;
    }
  | PathTraversal of {
      root: Path.t;
      requested: Path.t;
      candidate: Path.t;
      resolved: Path.t option;
    }
  | SymlinkDenied of {
      root: Path.t;
      requested: Path.t;
      symlink: Path.t;
    }
  | InvalidPath of {
      path: Path.t;
      canonicalize_error: Fs.error;
      exists_error: Fs.error option;
    }

(**
   Static file serving configuration.

   {3 Field descriptions}

   - [show_directory]: When [true], displays HTML directory listings for
     directories without index files. When [false], returns 403 Forbidden.

   - [index_files]: Files to try when a directory is requested. Tries in order
     until one is found.

   - [dotfiles]: Controls access to files starting with [.]:
     - [AllowDotfiles] - Serve dotfiles normally
     - [DenyDotfiles] - Return 403 Forbidden for dotfiles
     - [IgnoreDotfiles] - Return 404 Not Found for dotfiles

   - [symlinks]: Controls symlink handling:
     - [FollowSymlinks] - Follow symlinks (after security checks)
     - [DenySymlinks] - Return 403 Forbidden for symlinks

   - [headers]: Additional headers added to all file responses. Useful for
     security headers like [x-content-type-options].

   - [cache_control]: Cache-Control header. Use [None] for no caching,
     [Some "public, max-age=3600"] for 1 hour, or
     [Some "public, max-age=31536000, immutable"] for fingerprinted assets.
*)
val default_config: config

(**
   Default configuration:
   - [show_directory = false] - No directory browsing
   - [index_files = ["index.html"; "index.htm"]]
   - [dotfiles = DenyDotfiles] - Block dotfiles
   - [symlinks = FollowSymlinks] - Follow symlinks
   - [headers = []] - No additional headers
   - [cache_control = Some "public, max-age=3600"] - 1 hour cache
*)
(** {1 Middleware} *)

val middleware: ?config:config -> at:string -> Path.t -> unit -> Pipeline.middleware

val path_has_dot_segment: Path.t -> bool

val path_is_within_root: root:Path.t -> Path.t -> bool

val normalize_path: config -> Path.t -> Path.t -> (Path.t, path_error) result

val matches_mount: at:string -> request_path:string -> bool

val directory_listing_html:
  request_path:string ->
  path:Path.t ->
  entries:(string * bool * int * float) list ->
  string

(**
   Create static file serving middleware.

   {3 Parameters}

   - [config]: Optional configuration (uses {!default_config} if not provided)
   - [at]: URL prefix to match (e.g. ["/public"], ["/assets"])
   - [root]: Filesystem directory to serve from (e.g. [Path.v "./public"])

   {3 Behavior}

   The middleware:
   1. Checks if the request path starts with [at]
   2. If not, calls [next] (passes to next middleware)
   3. If yes, removes [at] prefix and looks for file in [root]
   4. Validates path security (no traversal, dotfile checks)
   5. Serves file with appropriate MIME type and caching headers
   6. Returns 404 if file not found, 403 if access denied

   {3 Examples}

   Serve from [./public] at [/public] URL:
   {[
     Static.middleware ~at:"/public" (Path.v "./public") ()
   ]}

   Custom cache for CDN:
   {[
     let config = Static.{ default_config with
       cache_control = Some "public, max-age=31536000, immutable";
     } in
     Static.middleware ~at:"/assets" ~config (Path.v "./dist") ()
   ]}

   Enable directory browsing:
   {[
     let config = Static.{ default_config with show_directory = true } in
     Static.middleware ~at:"/files" ~config (Path.v "./files") ()
   ]}
*)
