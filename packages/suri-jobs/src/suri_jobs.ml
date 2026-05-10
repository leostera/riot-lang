open Std

type ('ok, 'error) result = ('ok, 'error) Std.result

(** Typed, database-backed durable work queues. *)
module Error = Error
module JobId = Job_id
module QueueId = Queue_id
module WorkerId = Worker_id
module FanoutId = Fanout_id
module UniqueKey = Unique_key
module State = State
module Queue = Queue
module Job = Job
module Worker = Worker
module Fanout = Fanout
module Database = Database
module Memory = Memory
module Schema = Schema
module Sqlx_backend = Sqlx_backend
module Config = Jobs_config
module Routes = Routes
module Supervisor = Jobs_supervisor
module Runner = Runner

type queue = Queue.packed

let queue = Queue.pack

let submit = Runner.submit

let child_spec = Runner.child_spec

let child_spec_with_config = Runner.child_spec_with_config

let start_link = Runner.start_link

let start_link_with_config = Runner.start_link_with_config

let routes = Supervisor.routes
