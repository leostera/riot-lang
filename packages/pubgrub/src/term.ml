open Std

type package = string
type version_ranges = Version.t Ranges.t
type t = { package : package; ranges : version_ranges; positive : bool }

let package t = t.package
let ranges t = t.ranges
let is_positive t = t.positive
let positive pkg ranges = { package = pkg; ranges; positive = true }
let negative pkg ranges = { package = pkg; ranges; positive = false }
