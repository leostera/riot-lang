open Std

type dotfiles =
  | AllowDotfiles
  | DenyDotfiles
  | IgnoreDotfiles

type symlinks =
  | FollowSymlinks
  | DenySymlinks

type config = {
  show_directory: bool;
  index_files: string list;
  dotfiles: dotfiles;
  symlinks: symlinks;
  headers: (string * string) list;
  cache_control: string option;
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

let default_config = {
  show_directory = false;
  index_files = [ "index.html"; "index.htm" ];
  dotfiles = DenyDotfiles;
  symlinks = FollowSymlinks;
  headers = [];
  cache_control = Some "public, max-age=3600";
}

(** MIME type detection from file extension *)
module Mime = struct
  let extension_map = [
    ("html", "text/html; charset=utf-8");
    ("htm", "text/html; charset=utf-8");
    ("css", "text/css; charset=utf-8");
    ("js", "text/javascript; charset=utf-8");
    ("mjs", "text/javascript; charset=utf-8");
    ("json", "application/json");
    ("xml", "application/xml");
    ("txt", "text/plain; charset=utf-8");
    ("md", "text/markdown; charset=utf-8");
    ("csv", "text/csv");
    ("png", "image/png");
    ("jpg", "image/jpeg");
    ("jpeg", "image/jpeg");
    ("gif", "image/gif");
    ("svg", "image/svg+xml");
    ("webp", "image/webp");
    ("ico", "image/x-icon");
    ("bmp", "image/bmp");
    ("tiff", "image/tiff");
    ("woff", "font/woff");
    ("woff2", "font/woff2");
    ("ttf", "font/ttf");
    ("otf", "font/otf");
    ("eot", "application/vnd.ms-fontobject");
    ("pdf", "application/pdf");
    ("zip", "application/zip");
    ("tar", "application/x-tar");
    ("gz", "application/gzip");
    ("7z", "application/x-7z-compressed");
    ("rar", "application/vnd.rar");
    ("mp3", "audio/mpeg");
    ("wav", "audio/wav");
    ("ogg", "audio/ogg");
    ("m4a", "audio/mp4");
    ("mp4", "video/mp4");
    ("webm", "video/webm");
    ("ogv", "video/ogg");
    ("avi", "video/x-msvideo");
    ("mov", "video/quicktime");
  ]

  let from_extension = fun ext ->
    (* Remove leading dot if present *)
    let ext =
      if String.starts_with ~prefix:"." ext then
        String.sub ext ~offset:1 ~len:(String.length ext - 1)
      else
        ext
    in
    Std.Collections.Proplist.get extension_map ~key:(String.lowercase_ascii ext)
    |> Option.unwrap_or ~default:"application/octet-stream"
end

(** Security checks for path traversal and dotfiles *)
module Security = struct
  let normalize_mount = fun at ->
    let at =
      if at = "" then
        "/"
      else if String.starts_with ~prefix:"/" at then
        at
      else
        "/" ^ at
    in
    if String.length at > 1 && String.ends_with ~suffix:"/" at then
      String.sub at ~offset:0 ~len:(String.length at - 1)
    else
      at

  let matches_mount = fun ~at ~request_path ->
    let at = normalize_mount at in
    let request_path =
      if String.starts_with ~prefix:"/" request_path then
        request_path
      else
        "/" ^ request_path
    in
    at = "/" || request_path = at || String.starts_with ~prefix:(at ^ "/") request_path

  let path_has_dot_segment = fun path ->
    Path.components path
    |> List.exists
      (fun segment ->
        let segment = Path.to_string segment in
        not (segment = "/" || segment = "." || segment = "..")
        && String.length segment > 0
        && String.get_unchecked segment ~at:0 = '.')

  let check_dotfile = fun config path ->
    if not (path_has_dot_segment path) then
      true
    else
      match config.dotfiles with
      | AllowDotfiles -> true
      | DenyDotfiles -> false
      | IgnoreDotfiles -> false

  let path_is_within_root = fun ~root path ->
    let root = Path.normalize root in
    let path = Path.normalize path in
    Path.equal path root || (
      match Path.strip_prefix path ~prefix:root with
      | Ok _ -> true
      | Error _ -> false
    )

  let rec first_symlink_between = fun ~root path ->
    match Fs.symlink_metadata path with
    | Ok meta when Fs.Metadata.is_symlink meta -> Some path
    | _ ->
        if Path.equal path root then
          None
        else
          match Path.parent path with
          | Some parent -> first_symlink_between ~root parent
          | None -> None

  let first_symlink = fun root requested_path ->
    let root = Path.normalize root in
    let path =
      Path.join root requested_path
      |> Path.normalize
    in
    first_symlink_between ~root path

  let canonicalize_candidate = fun ~root ~requested ~candidate ->
    match Fs.canonicalize candidate with
    | Ok canonical ->
        if path_is_within_root ~root canonical then
          Ok canonical
        else
          Error (
            PathTraversal {
              root;
              requested;
              candidate;
              resolved = Some canonical;
            }
          )
    | Error canonicalize_error -> (
        match Fs.exists candidate with
        | Ok false -> Error (MissingPath { path = candidate })
        | Ok true ->
            Error (InvalidPath { path = candidate; canonicalize_error; exists_error = None })
        | Error exists_error ->
            Error (InvalidPath {
              path = candidate;
              canonicalize_error;
              exists_error = Some exists_error;
            })
      )

  let normalize_path = fun config root requested_path ->
    match Fs.canonicalize root with
    | Error error -> Error (InvalidRoot { root; error })
    | Ok root_canonical ->
        let candidate =
          Path.join root_canonical requested_path
          |> Path.normalize
        in
        if not (path_is_within_root ~root:root_canonical candidate) then
          Error (
            PathTraversal {
              root = root_canonical;
              requested = requested_path;
              candidate;
              resolved = None;
            }
          )
        else if config.symlinks = DenySymlinks then (
          match first_symlink root_canonical requested_path with
          | Some symlink ->
              Error (SymlinkDenied { root = root_canonical; requested = requested_path; symlink })
          | None -> canonicalize_candidate ~root:root_canonical ~requested:requested_path ~candidate
        ) else
          canonicalize_candidate ~root:root_canonical ~requested:requested_path ~candidate

  let is_safe_path = fun config root path ->
    check_dotfile config path && match normalize_path config root path with
    | Ok _ -> true
    | Error _ -> false
end

(** HTTP caching helpers *)
module Cache = struct
  let weekday = fun n ->
    match n with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | _ -> "Sun"

  let month = fun n ->
    match n with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> "Jan"

  let to_hex = fun n ->
    let rec loop acc n =
      if n = 0 then (
        if acc = "" then
          "0"
        else
          acc
      ) else
        let digit = n land 0xf in
        let char =
          if digit < 10 then
            Char.from_int_unchecked (Char.code '0' + digit)
          else
            Char.from_int_unchecked (Char.code 'a' + (digit - 10))
        in
        loop (String.make ~len:1 ~char ^ acc) (n lsr 4)
    in
    loop "" n

  let etag = fun meta ->
    (* Simple ETag from size and mtime *)
    let size = Fs.Metadata.len meta in
    let mtime = int_of_float (Fs.Metadata.modified meta) in
    String.concat "" [ "\""; to_hex size; "-"; to_hex mtime; "\""; ]

  let pad2 = fun n ->
    if n < 10 then
      "0" ^ string_of_int n
    else
      string_of_int n

  let pad4 = fun n ->
    let s = string_of_int n in
    let len = String.length s in
    if len >= 4 then
      s
    else
      String.make ~len:(4 - len) ~char:'0' ^ s

  let last_modified = fun timestamp ->
    (* Format as HTTP date: Sun, 06 Nov 1994 08:49:37 GMT *)
    let tm = Time.gmtime timestamp in
    String.concat
      ""
      [
        weekday tm.tm_wday;
        ", ";
        pad2 tm.tm_mday;
        " ";
        month tm.tm_mon;
        " ";
        pad4 (tm.tm_year + 1_900);
        " ";
        pad2 tm.tm_hour;
        ":";
        pad2 tm.tm_min;
        ":";
        pad2 tm.tm_sec;
        " GMT";
      ]

  let parse_http_date = fun date_str ->
    (* Simple HTTP date parser - accepts RFC 1123 format *)
    try
      let parts = String.split_on_char ' ' date_str in
      match parts with
      | [ _day; day_str; month_str; year_str; time_str; "GMT" ] ->
          let day = Int.from_string day_str in
          let month =
            match month_str with
            | "Jan" -> 0
            | "Feb" -> 1
            | "Mar" -> 2
            | "Apr" -> 3
            | "May" -> 4
            | "Jun" -> 5
            | "Jul" -> 6
            | "Aug" -> 7
            | "Sep" -> 8
            | "Oct" -> 9
            | "Nov" -> 10
            | "Dec" -> 11
            | _ -> 0
          in
          let year = Int.from_string year_str in
          let time_parts = String.split_on_char ':' time_str in
          let (hour, min, sec) =
            match time_parts with
            | [ h; m; s ] -> (Int.from_string h, Int.from_string m, Int.from_string s)
            | _ -> (0, 0, 0)
          in
          let tm: Time.tm = {
            tm_sec = sec;
            tm_min = min;
            tm_hour = hour;
            tm_mday = day;
            tm_mon = month;
            tm_year = year - 1_900;
            tm_wday = 0;
            tm_yday = 0;
            tm_isdst = false;
          }
          in
          let (unix_time, _) = Time.mktime tm in
          Some unix_time
      | _ -> None
    with
    | _ -> None

  let check_not_modified = fun conn meta ->
    let headers = Conn.headers conn in
    (* Check If-None-Match (ETag) *)
    let etag_match =
      Net.Http.Header.get headers "if-none-match"
      |> Option.map ~fn:(fun client_etag -> client_etag = etag meta)
      |> Option.unwrap_or ~default:false
    in
    (* Check If-Modified-Since *)
    let modified_match =
      match Net.Http.Header.get headers "if-modified-since" with
      | Some date_str -> (
          match parse_http_date date_str with
          | Some client_time -> client_time >= Fs.Metadata.modified meta
          | None -> false
        )
      | None -> false
    in
    etag_match || modified_match
end

(** Directory listing HTML generation *)
module Directory = struct
  type entry = { name: string; is_dir: bool; size: int; modified: float }

  let html_escape = fun str ->
    let buf = IO.Buffer.create ~size:(String.length str) in
    String.iter
      (fun __tmp1 ->
        match __tmp1 with
        | '&' -> IO.Buffer.add_string buf "&amp;"
        | '<' -> IO.Buffer.add_string buf "&lt;"
        | '>' -> IO.Buffer.add_string buf "&gt;"
        | '"' -> IO.Buffer.add_string buf "&quot;"
        | '\'' -> IO.Buffer.add_string buf "&#39;"
        | c -> IO.Buffer.add_char buf c)
      str;
    IO.Buffer.contents buf

  let format_float_1dp = fun f ->
    let whole = int_of_float f in
    let frac = int_of_float ((f -. float whole) *. 10.0) in
    string_of_int whole ^ "." ^ string_of_int frac

  let format_size = fun size ->
    if size < 1_024 then
      string_of_int size ^ " B"
    else if size < 1_024 * 1_024 then
      format_float_1dp (float size /. 1_024.0) ^ " KB"
    else if size < 1_024 * 1_024 * 1_024 then
      format_float_1dp (float size /. (1_024.0 *. 1_024.0)) ^ " MB"
    else
      format_float_1dp (float size /. (1_024.0 *. 1_024.0 *. 1_024.0)) ^ " GB"

  let format_date = fun timestamp ->
    let tm = Time.localtime timestamp in
    let pad2 n =
      if n < 10 then
        "0" ^ string_of_int n
      else
        string_of_int n
    in
    let pad4 n =
      let s = string_of_int n in
      let len = String.length s in
      if len >= 4 then
        s
      else
        String.make ~len:(4 - len) ~char:'0' ^ s
    in
    String.concat
      ""
      [
        pad4 (tm.tm_year + 1_900);
        "-";
        pad2 (tm.tm_mon + 1);
        "-";
        pad2 tm.tm_mday;
        " ";
        pad2 tm.tm_hour;
        ":";
        pad2 tm.tm_min;
      ]

  let collect_entries = fun config path ->
    match Fs.read_dir path with
    | Error _ -> []
    | Ok iter ->
        let entries = ref [] in
        Iter.MutIterator.for_each
          iter
          ~fn:(fun entry_path ->
            (* entry_path from read_dir is just the filename - join with directory *)
            let name = Path.to_string entry_path in
            let full_path = Path.join path entry_path in
            (* Check dotfile policy *)
            let include_entry =
              if String.length name > 0 && String.get_unchecked name ~at:0 = '.' then
                match config.dotfiles with
                | AllowDotfiles -> true
                | DenyDotfiles -> false
                | IgnoreDotfiles -> false
              else
                true
            in
            if include_entry then
              match Fs.metadata full_path with
              | Ok meta ->
                  let entry = {
                    name;
                    is_dir = Fs.Metadata.is_dir meta;
                    size = Fs.Metadata.len meta;
                    modified = Fs.Metadata.modified meta;
                  }
                  in
                  entries := entry :: !entries
              | Error _ -> ());
        List.sort ~compare:(fun a b -> String.compare a.name b.name) !entries

  let entry_row = fun request_path entry ->
    (* Build absolute path by appending entry name to request path *)
    let request_path_str =
      if String.ends_with ~suffix:"/" request_path then
        request_path
      else
        request_path ^ "/"
    in
    let href = String.concat "" [ request_path_str; Net.Uri.percent_encode entry.name ] in
    let display_name =
      if entry.is_dir then
        entry.name ^ "/"
      else
        entry.name
    in
    let size_str =
      if entry.is_dir then
        "-"
      else
        format_size entry.size
    in
    let date_str = format_date entry.modified in
    let href = html_escape href in
    let display_name = html_escape display_name in
    String.concat
      ""
      [
        "    <tr>\n";
        "      <td><a href=\"";
        href;
        "\">";
        display_name;
        "</a></td>\n";
        "      <td class=\"size\">";
        size_str;
        "</td>\n";
        "      <td class=\"date\">";
        date_str;
        "</td>\n";
        "    </tr>";
      ]

  let generate_html = fun request_path path entries ->
    let path_str =
      Path.to_string path
      |> html_escape
    in
    (* Build parent path by removing last segment *)
    let parent_href =
      if String.ends_with ~suffix:"/" request_path then
        String.sub request_path ~offset:0 ~len:(String.length request_path - 1)
      else
        request_path
    in
    let parent_href =
      match String.last_index parent_href '/' with
      | Some idx -> String.sub parent_href ~offset:0 ~len:(idx + 1)
      | None -> "/"
    in
    let parent_href = html_escape parent_href in
    let entries_html = String.concat "\n" (List.map ~fn:(entry_row request_path) entries) in
    String.concat
      ""
      [
        {|<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Index of |};
        path_str;
        {|</title>
  <style>
    body { font-family: monospace; margin: 40px; background: #fafafa; }
    h1 { border-bottom: 2px solid #ddd; padding-bottom: 10px; color: #333; }
    table { border-collapse: collapse; width: 100%%; background: white; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
    th { text-align: left; padding: 12px; border-bottom: 2px solid #ddd; background: #f5f5f5; font-weight: 600; }
    td { padding: 10px 12px; border-bottom: 1px solid #eee; }
    tr:hover { background: #f9f9f9; }
    a { text-decoration: none; color: #0066cc; }
    a:hover { text-decoration: underline; }
    .size { text-align: right; color: #666; }
    .date { color: #888; font-size: 0.9em; }
  </style>
</head>
<body>
  <h1>Index of |};
        path_str;
        {|</h1>
  <table>
    <tr>
      <th>Name</th>
      <th class="size">Size</th>
      <th>Modified</th>
    </tr>
    <tr>
      <td><a href="|};
        parent_href;
        {|">../</a></td>
      <td class="size">-</td>
      <td class="date">-</td>
    </tr>
|};
        entries_html;
        {|
  </table>
</body>
</html>|};
      ]
end

let path_has_dot_segment = Security.path_has_dot_segment

let path_is_within_root = Security.path_is_within_root

let normalize_path = Security.normalize_path

let matches_mount = Security.matches_mount

let directory_listing_html = fun ~request_path ~path ~entries ->
  let entries =
    List.map
      ~fn:(fun (name, is_dir, size, modified) ->
        Directory.{
          name;
          is_dir;
          size;
          modified;
        })
      entries
  in
  Directory.generate_html request_path path entries

(** Core file serving logic *)
let find_index_file = fun config path ->
  config.index_files
  |> List.filter_map
    ~fn:(fun index_name ->
      let index_path = Path.join path (Path.v index_name) in
      match Fs.exists index_path with
      | Ok true -> (
          match Fs.metadata index_path with
          | Ok meta when Fs.Metadata.is_file meta -> Some index_path
          | _ -> None
        )
      | _ -> None)
  |> List.head

let path_error_status_and_body = fun error ->
  match error with
  | MissingPath _ -> (Net.Http.Status.NotFound, "404 Not Found")
  | InvalidRoot _ -> (Net.Http.Status.Forbidden, "invalid static root")
  | InvalidPath _ -> (Net.Http.Status.Forbidden, "invalid path")
  | PathTraversal _ -> (Net.Http.Status.Forbidden, "path traversal blocked")
  | SymlinkDenied _ -> (Net.Http.Status.Forbidden, "symlinks are not allowed")

let dotfile_status_and_body = fun config ->
  match config.dotfiles with
  | IgnoreDotfiles -> (Net.Http.Status.NotFound, "404 Not Found")
  | AllowDotfiles -> (Net.Http.Status.Ok, "")
  | DenyDotfiles -> (Net.Http.Status.Forbidden, "access to dotfiles denied")

let rec serve_file = fun config root requested_path conn ->
  if not (Security.check_dotfile config requested_path) then
    let (status, body) = dotfile_status_and_body config in
    conn
    |> Conn.respond ~status ~body
    |> Conn.halt
  else
    (* Normalize and validate path *)
    match Security.normalize_path config root requested_path with
    | Error error ->
        let (status, body) = path_error_status_and_body error in
        conn
        |> Conn.respond ~status ~body
        |> Conn.halt
    | Ok full_path -> (
        (* Get file metadata *)
        match Fs.metadata full_path with
        | Error _ ->
            conn
            |> Conn.respond ~status:Net.Http.Status.NotFound ~body:"404 Not Found"
            |> Conn.halt
        | Ok meta when Fs.Metadata.is_dir meta -> serve_directory config root full_path conn
        | Ok meta when Fs.Metadata.is_file meta -> serve_regular_file config full_path meta conn
        | Ok _ ->
            conn
            |> Conn.respond ~status:Net.Http.Status.Forbidden ~body:"cannot serve special files"
            |> Conn.halt
      )

and serve_directory = fun config root path conn ->
  (* Try index files first *)
  match find_index_file config path with
  | Some index_path -> (
      match Fs.metadata index_path with
      | Ok meta -> serve_regular_file config index_path meta conn
      | Error _ ->
          conn
          |> Conn.respond
            ~status:Net.Http.Status.InternalServerError
            ~body:"failed to read index file"
          |> Conn.halt
    )
  | None ->
      (* No index file found *)
      if config.show_directory then
        let entries = Directory.collect_entries config path in
        let request_path = Conn.path conn in
        let html = Directory.generate_html request_path path entries in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:html
        |> Conn.with_header "content-type" "text/html; charset=utf-8"
        |> Conn.send
      else
        conn
        |> Conn.respond ~status:Net.Http.Status.Forbidden ~body:"directory listing disabled"
        |> Conn.halt

and serve_regular_file = fun config path meta conn ->
  (* Check if client has cached version *)
  if Cache.check_not_modified conn meta then
    conn
    |> Conn.respond ~status:Net.Http.Status.NotModified
    |> Conn.halt
  else
    (* Read file contents *)
    match Fs.read path with
    | Error _ ->
        conn
        |> Conn.respond ~status:Net.Http.Status.InternalServerError ~body:"failed to read file"
        |> Conn.halt
    | Ok content ->
        (* Determine MIME type *)
        let mime_type =
          Path.extension path
          |> Option.map ~fn:Mime.from_extension
          |> Option.unwrap_or ~default:"application/octet-stream"
        in
        (* Build headers *)
        let headers =
          [
            ("content-type", mime_type);
            ("content-length", string_of_int (String.length content));
            ("etag", Cache.etag meta);
            ("last-modified", Cache.last_modified (Fs.Metadata.modified meta));
          ]
          @ config.headers
        in
        let headers =
          match config.cache_control with
          | Some cc -> ("cache-control", cc) :: headers
          | None -> headers
        in
        (* Send response *)
        let conn = Conn.respond conn ~status:Net.Http.Status.Ok ~body:content in
        let conn =
          List.fold_left
            headers
            ~init:conn
            ~fn:(fun c (name, value) ->
              Conn.with_header name value c)
        in
        Conn.send conn

(** Middleware function *)
let middleware = fun ?(config = default_config) ~at root () ->
  let at = Security.normalize_mount at in
  fun ~conn ~next ->
    let request_path = Conn.path conn in
    (* Check if path matches our prefix *)
    if not (Security.matches_mount ~at ~request_path) then
      next conn
    else
      (* Remove prefix to get relative path *)
      let relative =
        let at_len = String.length at in
        let path_len = String.length request_path in
        if at_len >= path_len then
          ""
        else
          String.sub request_path ~offset:at_len ~len:(path_len - at_len)
      in
      (* Handle empty or root path *)
      let requested_path =
        if relative = "" || relative = "/" then
          Path.v "."
        else
          (* Remove leading slash if present *)
          let relative =
            if String.length relative > 0 && String.get_unchecked relative ~at:0 = '/' then
              String.sub relative ~offset:1 ~len:(String.length relative - 1)
            else
              relative
          in
          Path.v relative
      in
      serve_file config root requested_path conn
