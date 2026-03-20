open Std

type text_edit = Tusk_fix_api.Fix.text_edit = {
  span : Syn.Ceibo.Span.t;
  new_text : string;
}

type fix = Tusk_fix_api.Fix.fix = {
  title : string;
  edits : text_edit list;
}

let make_text_edit = Tusk_fix_api.Fix.make_text_edit
let make = Tusk_fix_api.Fix.make
let title = Tusk_fix_api.Fix.title
let edits = Tusk_fix_api.Fix.edits
let apply_edit = Tusk_fix_api.Fix.apply_edit
let apply_fix = Tusk_fix_api.Fix.apply_fix
let apply_fixes = Tusk_fix_api.Fix.apply_fixes
let validate_fix = Tusk_fix_api.Fix.validate_fix

let text_edit_to_json edit =
  let open Data.Json in
  Object
    [
      ( "span",
        Object
          [
            ("start", Int edit.span.start);
            ("end", Int edit.span.end_);
          ] );
      ("new_text", String edit.new_text);
    ]

let to_json fix =
  let open Data.Json in
  Object
    [
      ("title", String fix.title);
      ("edits", Array (List.map text_edit_to_json fix.edits));
    ]
