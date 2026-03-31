open Global
open Collections

type pre_release_segment =
  Numeric of int
  | Alphanumeric of string

type t = {
  major: int;
  minor: int;
  patch: int;
  pre: pre_release_segment list;
  build: string option;
}

type comparison =
  Lt
  | Eq
  | Gt

type requirement_op =
  | ReqEq
  | ReqNeq
  | ReqGt
  | ReqGte
  | ReqLt
  | ReqLte
  | ReqTilde

type requirement = requirement_op * t

type parse_error =
  | Invalid_format of string
  | Invalid_version_segment of string
  | Invalid_pre_release_segment of string

(* Parsing helpers *)

let is_digit = fun c -> c >= '0' && c <= '9'

let is_alphanumeric = fun c ->
    is_digit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '-'

let parse_int = fun s ->
    try Some (int_of_string s) with
    | Failure _ -> None

let split_on_char = fun delimiter str ->
    let len = String.length str in
    let rec loop acc current i =
      if i >= len then
        List.rev (current :: acc)
      else
        let c = String.get str i in
        if c = delimiter then
          loop (current :: acc) "" (i + 1)
        else
          loop acc (current ^ String.make 1 c) (i + 1)
    in
    loop [] "" 0

let parse_pre_release_segment = fun s ->
    match parse_int s with
    | Some n -> Ok (Numeric n)
    | None ->
        if String.length s > 0 && String.for_all is_alphanumeric s then
          Ok (Alphanumeric s)
        else
          Error (Invalid_pre_release_segment s)

let parse_pre_release_identifiers = fun s ->
    let segments = split_on_char '.' s in
    let rec parse_all = fun acc ->
        function
        | [] -> Ok (List.rev acc)
        | seg :: rest -> (
            match parse_pre_release_segment seg with
            | Ok segment -> parse_all (segment :: acc) rest
            | Error e -> Error e
          )
    in
    parse_all [] segments

(* Main parsing *)

let parse = fun version_string ->
    let len = String.length version_string in
    (* Find build metadata separator (+) *)
    let build_pos =
      try Some (String.index version_string '+') with
      | Not_found -> None
    in
    let core_and_pre, build =
      match build_pos with
      | None -> (version_string, None)
      | Some pos ->
          let core = String.sub version_string 0 pos in
          let build_meta = String.sub version_string (pos + 1) (len - pos - 1) in
          (core, Some build_meta)
    in
    (* Find pre-release separator (-) *)
    let pre_pos =
      try Some (String.index core_and_pre '-') with
      | Not_found -> None
    in
    let core, pre_string =
      match pre_pos with
      | None -> (core_and_pre, None)
      | Some pos ->
          let c = String.sub core_and_pre 0 pos in
          let p = String.sub core_and_pre (pos + 1) (String.length core_and_pre - pos - 1) in
          (c, Some p)
    in
    (* Parse core version (major.minor.patch) *)
    let parts = split_on_char '.' core in
    match parts with
    | [major_s;minor_s;patch_s] -> (
        match (parse_int major_s, parse_int minor_s, parse_int patch_s) with
        | Some major, Some minor, Some patch when major >= 0 && minor >= 0 && patch >= 0 -> (
            (* Parse pre-release identifiers if present *)
            match pre_string with
            | None -> Ok {major; minor; patch; pre = []; build}
            | Some pre_str -> (
                match parse_pre_release_identifiers pre_str with
                | Ok pre -> Ok {major; minor; patch; pre; build}
                | Error e -> Error e
              )
          )
        | _ -> Error (Invalid_version_segment core)
      )
    | _ -> Error (Invalid_format version_string)

(* Conversion *)

let pre_release_segment_to_string = function
  | Numeric n -> string_of_int n
  | Alphanumeric s -> s

let to_string = fun v ->
    let base = string_of_int v.major ^ "." ^ string_of_int v.minor ^ "." ^ string_of_int v.patch in
    let with_pre =
      match v.pre with
      | [] -> base
      | pre ->
          let pre_str = String.concat "." (List.map pre_release_segment_to_string pre) in
          base ^ "-" ^ pre_str
    in
    match v.build with
    | None -> with_pre
    | Some build -> with_pre ^ "+" ^ build

(* Comparison *)

let compare_pre_release_segment = fun s1 s2 ->
    match (s1, s2) with
    | Numeric n1, Numeric n2 ->
        if n1 < n2 then
          Lt
        else if n1 > n2 then
          Gt
        else
          Eq
    | Numeric _, Alphanumeric _ -> Lt
    | Alphanumeric _, Numeric _ -> Gt
    | Alphanumeric a1, Alphanumeric a2 ->
        if a1 < a2 then
          Lt
        else if a1 > a2 then
          Gt
        else
          Eq

let rec compare_pre_release_lists = fun l1 l2 ->
    match (l1, l2) with
    | [], [] ->
        Eq
    | [], _ :: _ ->
        Lt
    | _ :: _, [] ->
        Gt
    | h1 :: t1, h2 :: t2 -> (
        match compare_pre_release_segment h1 h2 with
        | Eq -> compare_pre_release_lists t1 t2
        | other -> other
      )

let compare = fun v1 v2 ->
    if v1.major < v2.major then
      Lt
    else if v1.major > v2.major then
      Gt
    else if v1.minor < v2.minor then
      Lt
    else if v1.minor > v2.minor then
      Gt
    else if v1.patch < v2.patch then
      Lt
    else if v1.patch > v2.patch then
      Gt
    else
      match (v1.pre, v2.pre) with
      | [], [] -> Eq
      | [], _ :: _ -> Gt
      | _ :: _, [] -> Lt
      | pre1, pre2 -> compare_pre_release_lists pre1 pre2

let equal = fun v1 v2 -> compare v1 v2 = Eq

let lt = fun v1 v2 -> compare v1 v2 = Lt

let lte = fun v1 v2 ->
    let c = compare v1 v2 in
    c = Lt || c = Eq

let gt = fun v1 v2 -> compare v1 v2 = Gt

let gte = fun v1 v2 ->
    let c = compare v1 v2 in
    c = Gt || c = Eq

(* Requirements *)

let parse_requirement = fun req_string ->
    let s = String.trim req_string in
    let len = String.length s in
    if len < 2 then
      Error (Invalid_format "Requirement too short")
    else
      let op, version_start =
        if String.length s >= 2 && String.sub s 0 2 = "~>" then
          (ReqTilde, 2)
        else if String.length s >= 2 && String.sub s 0 2 = "==" then
          (ReqEq, 2)
        else if String.length s >= 2 && String.sub s 0 2 = "!=" then
          (ReqNeq, 2)
        else if String.length s >= 2 && String.sub s 0 2 = ">=" then
          (ReqGte, 2)
        else if String.length s >= 2 && String.sub s 0 2 = "<=" then
          (ReqLte, 2)
        else if String.get s 0 = '>' then
          (ReqGt, 1)
        else if String.get s 0 = '<' then
          (ReqLt, 1)
        else
          (* Default to equality if no operator *)
          (ReqEq, 0)
      in
      let version_str = String.trim (String.sub s version_start (len - version_start)) in
      match parse version_str with
      | Ok version -> Ok (op, version)
      | Error e -> Error e

let matches = fun ((op, req_version)) test_version ->
    let cmp = compare test_version req_version in
    match op with
    | ReqEq ->
        cmp = Eq
    | ReqNeq ->
        cmp != Eq
    | ReqGt ->
        cmp = Gt
    | ReqGte ->
        cmp = Gt || cmp = Eq
    | ReqLt ->
        cmp = Lt
    | ReqLte ->
        cmp = Lt || cmp = Eq
    | ReqTilde ->
        (* ~> allows changes at the most specific level provided *)
        (* If patch is specified: >= version and < next minor *)
        (* If only major.minor: >= version and < next major *)
        let at_least = gte test_version req_version in
        let below_next =
          let next_minor = {req_version with minor = req_version.minor + 1; patch = 0; pre = []; } in
          lt test_version next_minor
        in
        at_least && below_next

(* Constructors *)

let make = fun ~major ~minor ~patch ?(pre = []) ?build () -> {major; minor; patch; pre; build}
