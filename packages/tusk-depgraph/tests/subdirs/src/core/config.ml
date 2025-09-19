(* core/config.ml - depends on types *)
type t = {
  id: Types.id;
  name: Types.name;
  debug: bool;
}