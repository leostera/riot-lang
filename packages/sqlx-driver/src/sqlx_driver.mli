(**
   SQLx driver interface library.

   This library defines the common types and interfaces that all SQLx database
   drivers must implement. It provides a uniform way to interact with different
   database systems while maintaining type safety.

   ## Architecture

   The library is organized into three main modules:

   - Value: Type-safe database values
   - Row: Database row representation and access
   - Driver: The interface that database drivers must implement

   ## Usage for Driver Authors

   If you're implementing a new database driver:

   ```ocaml
   open Sqlx_driver

   module MyDriver = struct
     type config = { ... }

     include (Driver.Intf with type config := config)

     let connect config = ...
     let execute stmt params = ...
     (* implement all required functions *)
   end
   ```

   ## Usage for SQLx Users

   End users typically won't interact with this library directly.
   Instead, they'll use the main SQLx library along with specific driver implementations:

   ```ocaml
   open Sqlx

   let pool = Sqlx.connect
     ~driver:(module Postgres.Driver)
     Postgres.Config.{ host = "localhost"; ... }
   ```
*)

(** Database value types and conversions. *)
module Value = Value

(** Database row representation and typed field access. *)
module Row = Row

(** Database driver interface that all drivers must implement. *)
module Driver = Driver
