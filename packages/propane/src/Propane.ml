open Std

(** Main module for Propane property testing library *)
module Generator = Generator
module Shrinker = Shrinker
module Printer = Printer
module Arbitrary = Arbitrary
module Property = Property

(* Convenience re-exports *)
let property = Property.property

let for_all = Property.for_all

let implies = Property.implies

let assume = Property.assume

let assume_fail = Property.assume_fail

let fail = Property.fail
