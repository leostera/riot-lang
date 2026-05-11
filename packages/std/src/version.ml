open Global
open Collections

type pre_release_segment =
  | Numeric of int
  | Alphanumeric of string

type t = {
  major: int;
  minor: int;
  patch: int;
  pre: pre_release_segment list;
  build: string option;
}

type requirement_op =
  | ReqEq
  | ReqNeq
  | ReqGt
  | ReqGte
  | ReqLt
  | ReqLte
  | ReqTilde

type requirement =
  | Any
  | Requirement of requirement_op * t
  | PrefixMajor of int
  | PrefixMinor of int * int

type requirement_view =
  | AnyRequirement
  | ExactRequirement of t
  | NotEqualRequirement of t
  | GreaterThanRequirement of t
  | GreaterThanOrEqualRequirement of t
  | LessThanRequirement of t
  | LessThanOrEqualRequirement of t
  | TildeRequirement of t
  | PrefixMajorRequirement of int
  | PrefixMinorRequirement of int * int

type parse_error =
  | Invalid_format of string
  | Invalid_version_segment of string
  | Invalid_pre_release_segment of string

(* Parsing helpers *)

let is_digit = fun c -> c >= '0' && c <= '9'

let is_alphanumeric = fun c -> is_digit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c
= '-'

let parse_int = Int.parse

let split_on_char = fun delimiter str ->
  let len = String.length str in
  let rec loop acc current i =
    if i >= len then
      List.reverse (current :: acc)
    else
      let c = String.get_unchecked str ~at:i in
      if c = delimiter then
        loop (current :: acc) "" (i + 1)
      else
        loop acc (current ^ String.make ~len:1 ~char:c) (i + 1)
  in
  loop [] "" 0

let parse_pre_release_segment = fun s ->
  match parse_int s with
  | Some n -> Ok (Numeric n)
  | None ->
      if String.length s > 0 && String.for_all s ~fn:is_alphanumeric then
        Ok (Alphanumeric s)
      else
        Error (Invalid_pre_release_segment s)

let parse_pre_release_identifiers = fun s ->
  let segments = split_on_char '.' s in
  let rec parse_all = fun acc ->
    fun __tmp1 ->
      match __tmp1 with
      | [] -> Ok (List.reverse acc)
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
  let build_pos = String.index_of version_string ~char:'+' in
  let (core_and_pre, build) =
    match build_pos with
    | None -> (version_string, None)
    | Some pos ->
        let core = String.sub version_string ~offset:0 ~len:pos in
        let build_meta = String.sub version_string ~offset:(pos + 1) ~len:(len - pos - 1) in
        (core, Some build_meta)
  in
  (* Find pre-release separator (-) *)
  let pre_pos = String.index_of core_and_pre ~char:'-' in
  let (core, pre_string) =
    match pre_pos with
    | None -> (core_and_pre, None)
    | Some pos ->
        let c = String.sub core_and_pre ~offset:0 ~len:pos in
        let p =
          String.sub core_and_pre ~offset:(pos + 1) ~len:(String.length core_and_pre - pos - 1)
        in
        (c, Some p)
  in
  (* Parse core version (major.minor.patch) *)
  let parts = split_on_char '.' core in
  match parts with
  | [ major_s; minor_s; patch_s ] -> (
      match (parse_int major_s, parse_int minor_s, parse_int patch_s) with
      | (Some major, Some minor, Some patch) when major >= 0 && minor >= 0 && patch >= 0 -> (
          (* Parse pre-release identifiers if present *)
          match pre_string with
          | None ->
              Ok {
                major;
                minor;
                patch;
                pre = [];
                build;
              }
          | Some pre_str -> (
              match parse_pre_release_identifiers pre_str with
              | Ok pre ->
                  Ok {
                    major;
                    minor;
                    patch;
                    pre;
                    build;
                  }
              | Error e -> Error e
            )
        )
      | _ -> Error (Invalid_version_segment core)
    )
  | _ -> Error (Invalid_format version_string)

(* Conversion *)

let pre_release_segment_to_string = fun __tmp1 ->
  match __tmp1 with
  | Numeric n -> Int.to_string n
  | Alphanumeric s -> s

let to_string = fun v ->
  let base = Int.to_string v.major ^ "." ^ Int.to_string v.minor ^ "." ^ Int.to_string v.patch in
  let with_pre =
    match v.pre with
    | [] -> base
    | pre ->
        let pre_str = String.concat "." (List.map ~fn:pre_release_segment_to_string pre) in
        base ^ "-" ^ pre_str
  in
  match v.build with
  | None -> with_pre
  | Some build -> with_pre ^ "+" ^ build

let requirement_op_to_string = fun __tmp1 ->
  match __tmp1 with
  | ReqEq -> "=="
  | ReqNeq -> "!="
  | ReqGt -> ">"
  | ReqGte -> ">="
  | ReqLt -> "<"
  | ReqLte -> "<="
  | ReqTilde -> "~>"

(* Comparison *)

let compare_pre_release_segment = fun s1 s2 ->
  match (s1, s2) with
  | (Numeric n1, Numeric n2) ->
      if n1 < n2 then
        Order.LT
      else if n1 > n2 then
        Order.GT
      else
        Order.EQ
  | (Numeric _, Alphanumeric _) -> Order.LT
  | (Alphanumeric _, Numeric _) -> Order.GT
  | (Alphanumeric a1, Alphanumeric a2) ->
      if a1 < a2 then
        Order.LT
      else if a1 > a2 then
        Order.GT
      else
        Order.EQ

let rec compare_pre_release_lists = fun l1 l2 ->
  match (l1, l2) with
  | ([], []) -> Order.EQ
  | ([], _ :: _) -> Order.LT
  | (_ :: _, []) -> Order.GT
  | (h1 :: t1, h2 :: t2) -> (
      match compare_pre_release_segment h1 h2 with
      | Order.EQ -> compare_pre_release_lists t1 t2
      | other -> other
    )

let compare = fun v1 v2 ->
  if v1.major < v2.major then
    Order.LT
  else if v1.major > v2.major then
    Order.GT
  else if v1.minor < v2.minor then
    Order.LT
  else if v1.minor > v2.minor then
    Order.GT
  else if v1.patch < v2.patch then
    Order.LT
  else if v1.patch > v2.patch then
    Order.GT
  else
    match (v1.pre, v2.pre) with
    | ([], []) -> Order.EQ
    | ([], _ :: _) -> Order.GT
    | (_ :: _, []) -> Order.LT
    | (pre1, pre2) -> compare_pre_release_lists pre1 pre2

let equal = fun v1 v2 -> compare v1 v2 = Order.EQ

let lt = fun v1 v2 -> compare v1 v2 = Order.LT

let lte = fun v1 v2 ->
  let c = compare v1 v2 in
  c = Order.LT || c = Order.EQ

let gt = fun v1 v2 -> compare v1 v2 = Order.GT

let gte = fun v1 v2 ->
  let c = compare v1 v2 in
  c = Order.GT || c = Order.EQ

(* Requirements *)

let parse_requirement = fun req_string ->
  let s = String.trim req_string in
  if String.equal s "*" then
    Ok Any
  else
    let len = String.length s in
    let starts_with prefix = String.starts_with ~prefix s in
    let (op, version_start) =
      if starts_with "~>" then
        (Some ReqTilde, 2)
      else if starts_with "==" then
        (Some ReqEq, 2)
      else if starts_with "!=" then
        (Some ReqNeq, 2)
      else if starts_with ">=" then
        (Some ReqGte, 2)
      else if starts_with "<=" then
        (Some ReqLte, 2)
      else if len > 0 then
        match String.get_unchecked s ~at:0 with
        | '>' -> (Some ReqGt, 1)
        | '<' -> (Some ReqLt, 1)
        | _ -> (None, 0)
      else
        (None, 0)
    in
    if version_start > len then
      Error (Invalid_format s)
    else
      let version_str =
        String.trim (String.sub s ~offset:version_start ~len:(len - version_start))
      in
      match op with
      | Some op -> (
          match parse version_str with
          | Ok version -> Ok (Requirement (op, version))
          | Error e -> Error e
        )
      | None -> (
          match parse version_str with
          | Ok version -> Ok (Requirement (ReqEq, version))
          | Error _ -> (
              match split_on_char '.' version_str with
              | [ major_s ] -> (
                  match parse_int major_s with
                  | Some major when major >= 0 -> Ok (PrefixMajor major)
                  | _ -> Error (Invalid_format version_str)
                )
              | [ major_s; minor_s ] -> (
                  match (parse_int major_s, parse_int minor_s) with
                  | (Some major, Some minor) when major >= 0 && minor >= 0 ->
                      Ok (PrefixMinor (major, minor))
                  | _ -> Error (Invalid_format version_str)
                )
              | _ -> Error (Invalid_format version_str)
            )
        )

let any = Any

let requirement_to_string = fun __tmp1 ->
  match __tmp1 with
  | Any -> "*"
  | Requirement (op, version) -> requirement_op_to_string op ^ " " ^ to_string version
  | PrefixMajor major -> Int.to_string major
  | PrefixMinor (major, minor) -> Int.to_string major ^ "." ^ Int.to_string minor

let view_requirement = fun __tmp1 ->
  match __tmp1 with
  | Any -> AnyRequirement
  | PrefixMajor major -> PrefixMajorRequirement major
  | PrefixMinor (major, minor) -> PrefixMinorRequirement (major, minor)
  | Requirement (ReqEq, version) -> ExactRequirement version
  | Requirement (ReqNeq, version) -> NotEqualRequirement version
  | Requirement (ReqGt, version) -> GreaterThanRequirement version
  | Requirement (ReqGte, version) -> GreaterThanOrEqualRequirement version
  | Requirement (ReqLt, version) -> LessThanRequirement version
  | Requirement (ReqLte, version) -> LessThanOrEqualRequirement version
  | Requirement (ReqTilde, version) -> TildeRequirement version

let matches = fun requirement test_version ->
  match requirement with
  | Any -> true
  | PrefixMajor major -> Int.equal test_version.major major
  | PrefixMinor (major, minor) ->
      Int.equal test_version.major major && Int.equal test_version.minor minor
  | Requirement (op, req_version) ->
      let cmp = compare test_version req_version in
      match op with
      | ReqEq -> cmp = Order.EQ
      | ReqNeq -> cmp != Order.EQ
      | ReqGt -> cmp = Order.GT
      | ReqGte -> cmp = Order.GT || cmp = Order.EQ
      | ReqLt -> cmp = Order.LT
      | ReqLte -> cmp = Order.LT || cmp = Order.EQ
      | ReqTilde ->
          let at_least = gte test_version req_version in
          let below_next =
            let next_minor = {
              req_version with
              minor = req_version.minor + 1;
              patch = 0;
              pre = [];
            }
            in
            lt test_version next_minor
          in
          at_least && below_next

(* Constructors *)

let make = fun ~major ~minor ~patch ?(pre = []) ?build () ->
  {
    major;
    minor;
    patch;
    pre;
    build;
  }

module Tests = struct
  let test_requirement_to_string () =
    match parse_requirement ">= 1.2.3" with
    | Ok requirement ->
        if String.equal (requirement_to_string requirement) ">= 1.2.3" then
          Ok ()
        else
          Error "expected requirement_to_string to preserve operator and version"
    | Error _ -> Error "expected requirement to parse" [@test]

  let test_any_requirement_roundtrip () =
    match parse_requirement "*" with
    | Ok requirement ->
        if
          requirement_to_string requirement = "*"
          && matches requirement (make ~major:999 ~minor:0 ~patch:0 ())
        then
          Ok ()
        else
          Error "expected '*' to roundtrip as unconstrained requirement"
    | Error _ -> Error "expected '*' requirement to parse" [@test]

  let test_prefix_minor_requirement_matches_patch_range () =
    match parse_requirement "0.2" with
    | Ok requirement ->
        if
          String.equal (requirement_to_string requirement) "0.2"
          && matches requirement (make ~major:0 ~minor:2 ~patch:0 ())
          && matches requirement (make ~major:0 ~minor:2 ~patch:99 ())
          && not (matches requirement (make ~major:0 ~minor:3 ~patch:0 ()))
        then
          Ok ()
        else
          Error "expected bare major.minor requirement to match only that patch range"
    | Error _ -> Error "expected bare major.minor requirement to parse" [@test]

  let test_prefix_major_requirement_matches_minor_and_patch_range () =
    match parse_requirement "0" with
    | Ok requirement ->
        if
          String.equal (requirement_to_string requirement) "0"
          && matches requirement (make ~major:0 ~minor:0 ~patch:0 ())
          && matches requirement (make ~major:0 ~minor:99 ~patch:99 ())
          && not (matches requirement (make ~major:1 ~minor:0 ~patch:0 ()))
        then
          Ok ()
        else
          Error "expected bare major requirement to match only that major range"
    | Error _ -> Error "expected bare major requirement to parse" [@test]
end
