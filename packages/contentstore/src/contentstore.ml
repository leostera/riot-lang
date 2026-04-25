open Std

module Policy = Policy

module Namespace = Namespace

module Store = Store

type t = Store.t

let create = Store.create

let root = Store.root

let namespace = Store.namespace

let policy = Store.policy

let hash_dir_of = Store.hash_dir_of

let exists = Store.exists

let commit_dir = Store.commit_dir

let save_object = Store.save_object

let save_file = Store.save_file

let open_object = Store.open_object

let save_named_object = Store.save_named_object

let save_named_file = Store.save_named_file

let open_named_object = Store.open_named_object
