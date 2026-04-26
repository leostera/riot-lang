(* ************************************************************************ *)

(* *)

(* OCaml *)

(* *)

(* KC Sivaramakrishnan, Indian Institute of Technology, Madras *)

(* Stephen Dolan, University of Cambridge *)

(* Tom Kelly, OCaml Labs Consultancy *)

(* *)

(* Copyright 2019 Indian Institute of Technology, Madras *)

(* Copyright 2014 University of Cambridge *)

(* Copyright 2021 OCaml Labs Consultancy Ltd *)

(* *)

(* All rights reserved.  This file is distributed under the terms of *)

(* the GNU Lesser General Public License version 2.1, with the *)

(* special exception on linking described in the file LICENSE. *)

(* *)

(* ************************************************************************ *)

open Prelude

module Raw = struct
  type id = private int

  type 'value state =
    | Running
    | Finished of ('value, exn * Exception.raw_backtrace) result [@warning "-unused-constructor"]

  type 'value term_sync = {
    mutable state: 'value state;
    mut: Sync.Mutex.t;
    cond: Sync.Condition.t;
  }

  external spawn: (unit -> 'value) -> 'value term_sync -> id = "caml_domain_spawn"

  external get_recommended_domain_count: unit -> int = "caml_recommended_domain_count" [@@noalloc]
end

let available_parallelism =
  let count = Raw.get_recommended_domain_count () in
  if count < 1 then
    1
  else
    count

type 'value t = {
  domain: Raw.id;
  term_sync: 'value Raw.term_sync;
}

external dangerously_cast_value: 'original -> 'casted = "%identity"

external opaque_identity: 'value -> 'value = "%opaque"

module DLS = struct
  module Obj_opt = struct
    type t = unit

    let none: t = dangerously_cast_value [||]

    let some = fun value -> dangerously_cast_value value

    let is_some = fun value -> not (Ptr.equal value none)

    let unsafe_get = fun value -> dangerously_cast_value value
  end

  type dls_state = Obj_opt.t array

  external get_dls_state: unit -> dls_state = "%dls_get"

  external set_dls_state: dls_state -> unit = "caml_domain_dls_set" [@@noalloc]

  external compare_and_set_dls_state: dls_state -> dls_state -> bool
    = "caml_domain_dls_compare_and_set" [@@noalloc]

  let create_dls = fun () ->
    let state = Array.make ~count:8 ~value:Obj_opt.none in
    set_dls_state state

  let _ = create_dls ()

  type 'value key = int * (unit -> 'value)

  type key_initializer =
    | KI: 'value key * ('value -> 'value) -> key_initializer

  let key_counter = Sync.Atomic.make 0

  let parent_keys = Sync.Atomic.make (([]: key_initializer list))

  let rec add_parent_key = fun key ->
    let keys = Sync.Atomic.get parent_keys in
    if not (Sync.Atomic.compare_and_set parent_keys keys (key :: keys)) then
      add_parent_key key

  let new_key = fun ?split_from_parent init_orphan ->
    let index = Sync.Atomic.fetch_and_add key_counter 1 in
    let key = (index, init_orphan) in
    (
      match split_from_parent with
      | None -> ()
      | Some split -> add_parent_key (KI (key, split))
    );
    key

  let rec maybe_grow = fun index ->
    let state = get_dls_state () in
    let size = Array.length state in
    if index < size then
      state
    else
      let rec next_size size =
        if index < size then
          size
        else
          next_size (size * 2)
      in
      let grown = Array.make ~count:(next_size size) ~value:Obj_opt.none in
      Array.blit state ~src_offset:0 ~dst:grown ~dst_offset:0 ~len:size;
      if compare_and_set_dls_state state grown then
        grown
      else
        maybe_grow index

  let set = fun ((index, _init): 'value key) value ->
    let state = maybe_grow index in
    Array.set_unchecked state ~at:index ~value:(Obj_opt.some (opaque_identity value))

  let array_compare_and_set = fun values index current next ->
    let seen = Array.get_unchecked values ~at:index in
    if Ptr.equal seen current then
      (
        Array.set_unchecked values ~at:index ~value:next;
        true
      )
    else
      false

  let get = fun ((index, init): 'value key) ->
    let state = maybe_grow index in
    let value = Array.get_unchecked state ~at:index in
    if Obj_opt.is_some value then
      ((Obj_opt.unsafe_get value): 'value)
    else
      let initialized = init () in
      let packed = Obj_opt.some (opaque_identity initialized) in
      let state = get_dls_state () in
      if array_compare_and_set state index value packed then
        initialized
      else
        let updated = Array.get_unchecked state ~at:index in
        if Obj_opt.is_some updated then
          ((Obj_opt.unsafe_get updated): 'value)
        else
          System_error.panic "Thread.DLS.get observed an uninitialized slot after compare-and-set"

  type key_value =
    | KV: 'value key * 'value -> key_value

  let get_initial_keys = fun () ->
    List.map (Sync.Atomic.get parent_keys) ~fn:(fun (KI (key, split)) -> KV (key, split (get key)))

  let set_initial_keys = fun values ->
    List.for_each values ~fn:(fun (KV (key, value)) -> set key value)
end

external raise_with_backtrace: exn -> Exception.raw_backtrace -> 'value = "%raise_with_backtrace"

external sleep_ns: int64 -> unit = "kernel_new_thread_sleep_ns"

let spawn = fun fn ->
  let initial_keys = DLS.get_initial_keys () in
  let term_sync =
    Raw.{ state = Running; mut = Sync.Mutex.create (); cond = Sync.Condition.create () } in
  let body () =
    DLS.create_dls ();
    DLS.set_initial_keys initial_keys;
    fn ()
  in
  let domain = Raw.spawn body term_sync in
  { domain; term_sync }

let join = fun { term_sync; _ } ->
  Sync.Mutex.lock term_sync.mut;
  let rec await () =
    match term_sync.state with
    | Raw.Running ->
        Sync.Condition.wait term_sync.cond term_sync.mut;
        await ()
    | Raw.Finished result ->
        Sync.Mutex.unlock term_sync.mut;
        result
  in
  match await () with
  | Ok value -> value
  | Error (exn, backtrace) -> raise_with_backtrace exn backtrace
