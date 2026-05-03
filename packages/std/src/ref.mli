open Global

(**
   Unique runtime type identifiers.

   References with unique identifiers for type-safe runtime type checking
   and witness patterns. Each reference has a globally unique ID that can
   be used to distinguish types at runtime.

   ## Examples

   Creating unique type witnesses:

   ```ocaml
   open Std

   module UserDb = struct
     type user = { name : string; age : int }

     let user_type : user Ref.t = Ref.make ()

     let create name age =
       ({ name; age }, user_type)

     let get_name (user, ref) =
       if Ref.equal ref user_type then
         Some user.name
       else
         None
   end
   ```

   Type-safe heterogeneous collections:

   ```ocaml
   type dyn_value =
     | DynValue : 'a Ref.t * 'a -> dyn_value

   let store = Hashtbl.create 16 in

   let string_ref = Ref.make () in
   let int_ref = Ref.make () in

   (* Store values *)
   Hashtbl.add store "name" (DynValue (string_ref, "Alice"));
   Hashtbl.add store "age" (DynValue (int_ref, 30));

   (* Retrieve with type checking *)
   match Hashtbl.find_opt store "name" with
   | Some (DynValue (ref, value)) ->
       (match Ref.cast ref string_ref value with
       | Some s -> Printf.printf "Name: %s\n" s
       | None -> Printf.printf "Type mismatch\n")
   | None -> ()
   ```

   ## Common Use Cases

   - Runtime type witnesses
   - Type-safe heterogeneous data structures
   - Dynamic typing with safety
   - Capability tokens
   - Unique resource identifiers
*)

(**
   A unique identifier for a type ['a]. Each call to [make] creates a fresh
   identifier that is distinct from all others.
*)
type 'a t

(**
   Creates a new unique reference identifier.

   Each call returns a fresh identifier that is guaranteed to be different from
   all other references ever created.

   ## Examples

   ```ocaml let ref1 : int Ref.t = Ref.make () in let ref2 : int Ref.t =
   Ref.make () in

   Ref.equal ref1 ref2 (* false - different references *) Ref.equal ref1 ref1
   (* true - same reference *) ```

   ## Use Case

   Use this to create witness types that prove ownership or capability at
   runtime.
*)
val make: unit -> 'a t

(**
   Checks if two references are the same, regardless of their type parameters.
   Returns [true] only if they were created by the same [make] call.

   ## Examples

   ```ocaml let ref1 = Ref.make () in let ref2 = Ref.make () in

   Ref.equal ref1 ref1 (* true *) Ref.equal ref1 ref2 (* false *) ```
*)
val equal: 'a t -> 'b t -> bool

(**
   Checks if two references are equal and, if so, returns a type equality
   witness proving that ['a] and ['b] are the same type.

   ## Examples

   ```ocaml
   let ref1 : int Ref.t = Ref.make () in
   let ref2 : string Ref.t = Ref.make () in

   match Ref.type_equal ref1 ref2 with
   | Some Type.Eq -> (* proved int = string -- dangerous! *)
   | None -> ()

   Ref.type_equal ref1 ref2 (* None - different refs *)
   ```
*)
val type_equal: 'a t -> 'b t -> ('a, 'b) Type.eq option

(**
   Attempts to cast a value from type ['a] to type ['b] if the references are
   equal. Returns [Some value] if the cast succeeds, [None] otherwise.

   This is a safe runtime type cast using references as witnesses.

   ## Examples

   ```ocaml let string_ref = Ref.make () in let int_ref = Ref.make () in

   (* Cast succeeds - same reference *) Ref.cast string_ref string_ref "hello"
   (* Some "hello" *)

   (* Cast fails - different references *) Ref.cast string_ref int_ref "hello"
   (* None *) ```
*)
val cast: 'a t -> 'b t -> 'a -> 'b option

(**
   Returns [true] if the first reference was created after the second.

   References have a creation order based on when [make] was called.

   ## Examples

   ```ocaml let old_ref = Ref.make () in let new_ref = Ref.make () in

   Ref.is_newer new_ref old_ref (* true *) Ref.is_newer old_ref new_ref (*
   false *) ```
*)
val is_newer: 'a t -> 'b t -> bool

(**
   Returns a hash value for the reference.

   References with the same identity hash to the same value. Useful for using
   references as keys in hash tables.

   ## Examples

   ```ocaml let ref = Ref.make () in let h = Ref.hash ref in

   let table = Hashtbl.create 16 in Hashtbl.add table h "value" ```
*)
val hash: 'a t -> int
