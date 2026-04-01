open Std

type package = string

type version_ranges = Version.t Ranges.t

type t = {
  package: package;
  ranges: version_ranges;
  positive: bool;
}

let version_compare = fun a b ->
  match Version.compare a b with
  | Lt -> (-1)
  | Eq -> 0
  | Gt -> 1

let package = fun t -> t.package

let ranges = fun t -> t.ranges

let is_positive = fun t -> t.positive

let positive = fun pkg ranges -> {package = pkg;ranges;positive = true;}

let negative = fun pkg ranges -> {package = pkg;ranges;positive = false;}

let is_any = fun t ->
  (t.positive && Ranges.is_empty t.ranges) || ((not t.positive) && t.ranges = Ranges.full)

let negate = fun t -> {t with positive = not t.positive;}

let union = fun t1 t2 ->
  if t1.package != t2.package then
    panic "Cannot union terms for different packages"
  else
    match (t1.positive, t2.positive) with
    | true, true ->
        {
          package = t1.package;
          ranges = Ranges.union ~compare_v:version_compare t1.ranges t2.ranges;
          positive = true;
        }
    | false, false ->
        {
          package = t1.package;
          ranges = Ranges.intersection ~compare_v:version_compare t1.ranges t2.ranges;
          positive = false;
        }
    | (true, false)
    | (false, true) ->
        let pos_ranges =
          if t1.positive then
            t1.ranges
          else
            t2.ranges
        in
        let neg_ranges =
          if t1.positive then
            t2.ranges
          else
            t1.ranges
        in
        if Ranges.subset_of ~compare_v:version_compare pos_ranges neg_ranges then
          {package = t1.package;ranges = Ranges.empty;positive = true;}
        else
          {
            package = t1.package;
            ranges = Ranges.union
              ~compare_v:version_compare
              pos_ranges
              (Ranges.complement ~compare_v:version_compare neg_ranges);
            positive = true;
          }

let intersection = fun t1 t2 ->
  if t1.package != t2.package then
    panic "Cannot intersect terms for different packages"
  else
    match (t1.positive, t2.positive) with
    | true, true -> {
      package = t1.package;
      ranges = Ranges.intersection ~compare_v:version_compare t1.ranges t2.ranges;
      positive = true;
    }
    | false, false -> {
      package = t1.package;
      ranges = Ranges.union ~compare_v:version_compare t1.ranges t2.ranges;
      positive = false;
    }
    | true, false -> {
      package = t1.package;
      ranges = Ranges.intersection
        ~compare_v:version_compare
        t1.ranges
        (Ranges.complement ~compare_v:version_compare t2.ranges);
      positive = true;
    }
    | false, true -> {
      package = t1.package;
      ranges = Ranges.intersection
        ~compare_v:version_compare
        (Ranges.complement ~compare_v:version_compare t1.ranges)
        t2.ranges;
      positive = true;
    }
