open Prelude

module FFI = struct
  external architecture: unit -> string = "kernel_new_host_arch"

  external vendor: unit -> string = "kernel_new_host_vendor"

  external os: unit -> string = "kernel_new_host_os"

  external abi: unit -> string = "kernel_new_host_abi"
end

module Host = struct
  type t = {
    architecture: string;
    vendor: string;
    os: string;
    abi: string option;
  }

  let equal = fun left right ->
    left.architecture = right.architecture
    && left.vendor = right.vendor
    && left.os = right.os
    && left.abi = right.abi

  let to_string = fun value ->
    let base = String.concat "" [ value.architecture; "-"; value.vendor; "-"; value.os ] in
    match value.abi with
    | Some abi when abi != "" -> String.concat "" [ base; "-"; abi ]
    | _ -> base

  let substring = fun value ~offset ~len ->
    Bytes.sub_string (Bytes.of_string value) offset len

  let rec reverse_parts = function
    | [] -> []
    | head :: tail ->
        let rec prepend_tail rest =
          match rest with
          | [] -> [ head ]
          | next :: remaining -> next :: prepend_tail remaining
        in
        prepend_tail (reverse_parts tail)

  let split_triplet = fun value ->
    let length = String.length value in
    let rec loop start index acc =
      if index >= length then
        Result.Ok (reverse_parts (substring value ~offset:start ~len:(length - start) :: acc))
      else if String.get value index = '-' then
        let part = substring value ~offset:start ~len:(index - start) in
        loop (index + 1) (index + 1) (part :: acc)
      else
        loop start (index + 1) acc
    in
    if length = 0 then
      Result.Error "invalid host triplet format: "
    else
      loop 0 0 []

  let from_string = fun value ->
    match split_triplet value with
    | Result.Error _ -> Result.Error (String.concat "" [ "invalid host triplet format: "; value ])
    | Result.Ok [architecture;vendor;os] -> Result.Ok { architecture; vendor; os; abi = None }
    | Result.Ok [architecture;vendor;os;abi] -> Result.Ok {
      architecture;
      vendor;
      os;
      abi = Some abi
    }
    | Result.Ok _ -> Result.Error (String.concat "" [ "invalid host triplet format: "; value ])

  let current =
    let abi = FFI.abi () in
    {
      architecture = FFI.architecture ();
      vendor = FFI.vendor ();
      os = FFI.os ();
      abi =
        if abi = "" then
          None
        else
          Some abi;
    }
end

module OS = struct
  type t =
    | Unix
    | Win32
    | Cygwin

  let current =
    match Host.current.os with
    | "windows" -> Win32
    | "cygwin" -> Cygwin
    | _ -> Unix

  let to_string = function
    | Unix -> "Unix"
    | Win32 -> "Win32"
    | Cygwin -> "Cygwin"

  let is_unix =
    match current with
    | Unix -> true
    | Win32
    | Cygwin -> false

  let is_win32 =
    match current with
    | Win32 -> true
    | Unix
    | Cygwin -> false

  let is_cygwin =
    match current with
    | Cygwin -> true
    | Unix
    | Win32 -> false
end

let host_triplet = Host.current

let os_type = OS.to_string OS.current

let unix = OS.is_unix

let win32 = OS.is_win32

let cygwin = OS.is_cygwin
