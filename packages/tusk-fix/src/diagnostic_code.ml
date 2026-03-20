open Std

type t =
  | DirectUnixUsage
  | DirectSysUsage
  | DirectStdlibUsage
  | DirectPervasivesUsage

type entry = {
  code : t;
  title : string;
  body : string;
}

let to_id = function
  | DirectUnixUsage -> "F0001"
  | DirectSysUsage -> "F0002"
  | DirectStdlibUsage -> "F0003"
  | DirectPervasivesUsage -> "F0004"

let of_id = function
  | "F0001" -> Some DirectUnixUsage
  | "F0002" -> Some DirectSysUsage
  | "F0003" -> Some DirectStdlibUsage
  | "F0004" -> Some DirectPervasivesUsage
  | _ -> None

let title = function
  | DirectUnixUsage -> "Direct Unix usage"
  | DirectSysUsage -> "Direct Sys usage"
  | DirectStdlibUsage -> "Direct Stdlib usage"
  | DirectPervasivesUsage -> "Direct Pervasives usage"

let body = function
  | DirectUnixUsage ->
      {|
Direct calls into Unix bypass Riot's scheduling and portability boundaries.

Why this rule exists:
- Riot code runs on top of a cooperative actor runtime.
- A blocking Unix call can stall the scheduler and delay unrelated actors.
- Direct Unix usage also hard-codes platform details into packages that should stay platform-agnostic.

What to do instead:
- Prefer package-owned Riot abstractions when they exist.
- Push true OS boundaries down into the packages that are supposed to own them, like kernel.
- If you really need a Unix boundary, introduce it deliberately instead of sprinkling Unix calls through application code.

Examples:
  Bad:    let home = Unix.getenv "HOME"
  Bad:    let now = Unix.gettimeofday ()
  Better: move the OS interaction behind a package-owned API and call that API from the rest of the system.

This rule exists to keep scheduler-sensitive code honest. A direct Unix call may work today and still be the wrong architectural seam.
|}
  | DirectSysUsage ->
      {|
Direct Sys usage reaches into process-global runtime state instead of going through Riot-owned boundaries.

Why this rule exists:
- Sys exposes host and runtime details directly from OCaml.
- That makes portability and policy decisions leak into packages that should not own them.
- It also makes it harder to keep behavior consistent across the ecosystem.

What to do instead:
- Prefer Riot wrappers for system information and runtime behavior.
- Keep process-global and platform-global logic in boundary-owning packages.

Examples:
  Bad:    let args = Sys.argv
  Bad:    let is_win = Sys.win32
  Better: depend on a Riot-owned API that exposes the specific system fact you need.

This rule keeps packages from silently depending on ambient runtime state.
|}
  | DirectStdlibUsage ->
      {|
Code outside the runtime boundary should go through Riot's Std layer instead of referencing Stdlib directly.

Why this rule exists:
- Riot is trying to provide a coherent programming stack, not just a pile of packages.
- Routing code through Std gives the ecosystem one owned surface instead of ad hoc direct references into Stdlib.
- That leaves room for better defaults, portability adjustments, and package-wide conventions.

What to do instead:
- Replace Stdlib references with Std when the Riot surface already owns that API.
- If Std does not yet expose something important, that is usually a signal to extend Std deliberately rather than bypass it forever.

Examples:
  Bad:    open Stdlib
  Bad:    let cmp = Stdlib.compare a b
  Better: open Std
  Better: let cmp = Std.compare a b

This rule is about keeping the ecosystem designed, not accidental.
|}
  | DirectPervasivesUsage ->
      {|
Pervasives is the historical pre-Stdlib module and should not appear in modern Riot code.

Why this rule exists:
- Pervasives is legacy OCaml surface area.
- Riot code should point at the current owned surface, not historic compatibility layers.

What to do instead:
- Replace direct Pervasives references with Std.

Examples:
  Bad:    let cmp = Pervasives.compare a b
  Better: let cmp = Std.compare a b

This rule exists mostly for consistency and modernization.
|}

let rule_id _ = "no-stdlib"

let message = function
  | DirectStdlibUsage ->
      "Direct usage of Stdlib is discouraged. Use Std instead."
  | DirectPervasivesUsage ->
      "Direct usage of Pervasives is discouraged. Use Std instead."
  | DirectUnixUsage ->
      "Direct usage of Unix is discouraged. Use package-owned Riot abstractions instead."
  | DirectSysUsage ->
      "Direct usage of Sys is discouraged. Use package-owned Riot abstractions instead."

let no_stdlib_code_for_module = function
  | "Unix" -> Some DirectUnixUsage
  | "Sys" -> Some DirectSysUsage
  | "Stdlib" -> Some DirectStdlibUsage
  | "Pervasives" -> Some DirectPervasivesUsage
  | _ -> None

let explain code =
  match of_id code with
  | Some code ->
      Some {
        code;
        title = title code;
        body = body code;
      }
  | None -> None

let format_explanation entry =
  to_id entry.code ^ " - " ^ entry.title ^ "\n\n" ^ entry.body ^ "\n"
