(** # Data.CSV - CSV parsing and serialization

    A CSV (Comma-Separated Values) parser and serializer for reading and writing
    tabular data. Supports standard CSV format with customizable delimiters and
    quote characters.

    The primary interface uses incremental parsing through mutable iterators,
    allowing memory-efficient processing of large CSV files.

    ## Examples

    Reading a CSV file incrementally (recommended):

    ```ocaml open Std

    let iter = Data.Csv.read (Path.v "data.csv") in

    let rec process_rows () = match Iter.MutIterator.next iter with | Some (Ok
    row) -> println "Row: %s" (String.concat "," row); process_rows () | Some
    (Error err) -> Log.error "Parse error: %s" (Data.Csv.error_to_string err) |
    None -> println "Done processing CSV" in process_rows () ```

    Processing with headers:

    ```ocaml let iter = Data.Csv.read (Path.v "users.csv") in

    (* Get headers first *) let headers = match Iter.MutIterator.next iter with
    | Some (Ok row) -> row | _ -> [] in

    (* Process data rows *) let rec process_data () = match
    Iter.MutIterator.next iter with | Some (Ok row) -> let name = List.nth row 0
    in let age = List.nth row 1 in println "%s is %s years old" name age;
    process_data () | Some (Error err) -> Log.error "Parse error: %s"
    (Data.Csv.error_to_string err) | None -> () in process_data () ```

    Counting rows without loading into memory:

    ```ocaml let iter = Data.Csv.read (Path.v "large.csv") in let count = cell 0
    in

    let rec count_rows () = match Iter.MutIterator.next iter with | Some (Ok
    _row) -> Sync.Cell.set count (Sync.Cell.get count + 1); count_rows () | Some (Error _)
    -> Sync.Cell.get count | None -> Sync.Cell.get count in

    println "Total rows: %d" (count_rows ()) ```

    Writing CSV:

    ```ocaml let headers = ["name"; "age"; "city"] in let data =
    [ ["Alice"; "30"; "NYC"]; ["Bob"; "25"; "SF"] ] in Csv.write ~headers ~data
    (Path.v "output.csv") |> Result.unwrap ```

    Custom delimiters (TSV):

    ```ocaml let config = Csv.config ~delimiter:'\t' () in let iter = Csv.read
    ~config (Path.v "data.tsv") in (* process TSV file *) ```

    Parsing from string (for testing or small data):

    ```ocaml let csv_str = "name,age\nAlice,30\nBob,25" in let iter =
    Csv.of_string csv_str in

    match Iter.MutIterator.next iter with | Some (Ok row) -> (* process row *) |
    _ -> () ```

    Loading entire CSV into memory:

    ```ocaml let iter = Csv.read (Path.v "data.csv") in let rows =
    Iter.MutIterator.to_list iter in (* rows is a list of (row, error) result *)

    (* Or filter out errors: *) let valid_rows = rows |> List.filter_map
    (function Ok row -> Some row | Error _ -> None) in ``` *)

(** {1 Types} *)

type row = string list
(** A CSV row is a list of field values *)

type t = row list
(** A CSV document is a list of rows *)

type config = {
  delimiter : char;
  quote : char;
  escape : char;
  trim_fields : bool;
}
(** Configuration for CSV parsing and serialization:
    - [delimiter]: Character separating fields (default: ',')
    - [quote]: Character for quoting fields (default: '"')
    - [escape]: Character for escaping quotes (default: '"')
    - [trim_fields]: Whether to trim whitespace from fields (default: false) *)

type error =
  | Unterminated_quote of { line : int; column : int }
  | Invalid_escape_sequence of { line : int; column : int }
  | Empty_input
  | Unknown_error of string  (** CSV parsing errors with position information *)

(** {1 Configuration} *)

val default_config : config
(** Default CSV configuration:
    - delimiter: ','
    - quote: '"'
    - escape: '"'
    - trim_fields: false *)

val config :
  ?delimiter:char ->
  ?quote:char ->
  ?escape:char ->
  ?trim_fields:bool ->
  unit ->
  config
(** Creates a custom CSV configuration.

    ## Examples

    ```ocaml let tsv_config = Csv.config ~delimiter:'\t' () let excel_config =
    Csv.config ~delimiter:',' ~trim_fields:true () ``` *)

(** {1 Reading CSV Files} *)

val read : ?config:config -> Path.t -> (row, error) result Iter.MutIterator.t
(** Reads a CSV file incrementally, returning a mutable iterator over rows. This
    is the recommended way to read CSV files as it processes rows one at a time
    without loading the entire file into memory.

    Each iterator item is a [Result] containing either a successfully parsed row
    or a parse error. The iterator stops at the first parse error.

    ## Examples

    Basic file reading:

    ```ocaml let iter = Csv.read (Path.v "data.csv") in

    let rec process () = match Iter.MutIterator.next iter with | Some (Ok row)
    -> (* Process row *) process () | Some (Error err) -> Log.error "Parse
    error: %s" (Csv.error_to_string err) | None -> () in process () ```

    With custom delimiter (TSV):

    ```ocaml let config = Csv.config ~delimiter:'\t' () in let iter = Csv.read
    ~config (Path.v "data.tsv") in ```

    ## Errors

    Returns an iterator that yields [Error] items if:
    - Parse errors occur (unterminated quotes, invalid escape sequences, etc.)

    Note: File I/O errors (file not found, permission denied) will cause the
    function itself to panic - wrap in try/catch if you need to handle these. *)

val of_string :
  ?config:config -> string -> (row, error) result Iter.MutIterator.t
(** Parses a CSV string incrementally, returning a mutable iterator over rows.
    Useful for parsing CSV data from strings, network responses, or testing.

    ## Examples

    ```ocaml let csv_str = "name,age\nAlice,30\nBob,25" in let iter =
    Csv.of_string csv_str in

    match Iter.MutIterator.next iter with | Some (Ok row) -> (* ["name"; "age"]
    *) | _ -> () ```

    With custom config:

    ```ocaml let config = Csv.config ~delimiter:';' () in let iter =
    Csv.of_string ~config "a;b;c\n1;2;3" in ``` *)

val error_to_string : error -> string
(** Converts a parse error to a human-readable message.

    ## Examples

    ```ocaml match Csv.of_string bad_input with | Ok _ -> () | Error err ->
    Log.error "CSV parse failed: %s" (Csv.error_to_string err) ``` *)

(** {1 Writing CSV Files} *)

val write :
  ?config:config ->
  ?headers:string list ->
  data:string list list ->
  Path.t ->
  (unit, Fs.error) result
(** Writes CSV rows to a file. Fields containing delimiters, quotes, or newlines
    are automatically quoted.

    ## Examples

    Basic writing:

    ```ocaml let data = [ ["Alice"; "30"; "NYC"]; ["Bob"; "25"; "SF"] ] in
    Csv.write ~data (Path.v "output.csv") |> Result.unwrap ```

    With headers:

    ```ocaml let headers = ["name"; "age"; "city"] in let data =
    [ ["Alice"; "30"; "NYC"]; ["Bob"; "25"; "SF"] ] in Csv.write ~headers ~data
    (Path.v "output.csv") |> Result.unwrap ```

    With custom delimiter (TSV):

    ```ocaml let config = Csv.config ~delimiter:'\t' () in Csv.write ~config
    ~headers ~data (Path.v "output.tsv") |> Result.unwrap ``` *)

val to_string :
  ?config:config -> ?headers:string list -> string list list -> string
(** Serializes CSV rows to a string. Fields containing delimiters, quotes, or
    newlines are automatically quoted.

    ## Examples

    ```ocaml Csv.to_string [["a"; "b"]; ["c"; "d"]] (* "a,b\nc,d\n" *)

    Csv.to_string [["quoted,field"; "normal"]] (* "\"quoted,field\",normal\n" *)
    ```

    With headers:

    ```ocaml let headers = ["col1"; "col2"] in let data =
    [["a"; "b"]; ["c"; "d"]] in Csv.to_string ~headers data (*
    "col1,col2\na,b\nc,d\n" *) ```

    With custom config:

    ```ocaml let config = Csv.config ~delimiter:';' () in Csv.to_string ~config
    ~headers data (* "col1;col2\na;b\nc;d\n" *) ``` *)
