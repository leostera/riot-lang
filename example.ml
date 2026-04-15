type t = {
  mutable aha_its_using_the_field_name: bool;
}

let foo = fun t -> [%atomic.loc.aha_its_using_the_field_name t.aha_its_using_the_field_name]
