open Std

let iter_fold = fun fold value ~fn ->
  fold
    value
    ~init:()
    ~fn:(fun item () ->
      fn item;
      Syn.Ast.Continue ())

module Vector = Collections.Vector

let ( let* ) value fn = Result.and_then value ~fn

let vector_to_list = fun vector ->
  Vector.to_array vector
  |> Array.to_list

let strip_prefix = fun prefix text ->
  if String.starts_with ~prefix text then
    String.sub text ~offset:(String.length prefix) ~len:(String.length text - String.length prefix)
  else
    text

let strip_suffix = fun suffix text ->
  if String.ends_with ~suffix text then
    String.sub text ~offset:0 ~len:(String.length text - String.length suffix)
  else
    text

let clean_docstring = fun raw ->
  raw
  |> String.split ~by:"\n"
  |> List.map
    ~fn:(fun line ->
      let trimmed =
        line
        |> String.trim
        |> strip_prefix "(**"
        |> strip_suffix "*)"
        |> String.trim
      in
      if String.starts_with ~prefix:"*" trimmed then
        trimmed
        |> strip_prefix "*"
        |> String.trim
      else
        trimmed)
  |> String.concat "\n"
  |> String.trim

let first_nonempty_line = fun text ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> ""
    | line :: rest ->
        if String.trim line = "" then
          loop rest
        else
          String.trim line
  in
  loop (String.split ~by:"\n" text)

let docstring_of_docs = fun __tmp1 ->
  match __tmp1 with
  | [] -> None
  | docs -> Some (String.concat "\n\n" docs)

let combine_docstrings = fun left right ->
  match (left, right) with
  | (Some left, Some right) when not (String.equal left "") && not (String.equal right "") ->
      Some (left ^ "\n\n" ^ right)
  | (Some left, _) when not (String.equal left "") -> Some left
  | (_, Some right) when not (String.equal right "") -> Some right
  | _ -> None

let slugify = fun text ->
  text
  |> String.map
    ~fn:(fun ch ->
      match ch with
      | '/'
      | '\\'
      | '.'
      | ' '
      | '-'
      | ':'
      | '('
      | ')' -> '_'
      | _ -> ch)

let snippet_of_node = fun node ->
  Syn.Ast.Node.text node
  |> String.trim

let strip_comments = fun text ->
  let (depth, pending, acc) =
    String.fold_left
      ~fn:(fun (depth, pending, acc) ch ->
        match pending with
        | None -> (depth, Some ch, acc)
        | Some prev ->
            if prev = '(' && ch = '*' then
              (depth + 1, None, acc)
            else if prev = '*' && ch = ')' && depth > 0 then
              (depth - 1, None, acc)
            else
              let acc =
                if depth = 0 || prev = '\n' || prev = '\r' then
                  String.make ~len:1 ~char:prev :: acc
                else
                  acc
              in
              (depth, Some ch, acc))
      ~init:(0, None, [])
      text
  in
  let acc =
    match pending with
    | Some ch when depth = 0 || ch = '\n' || ch = '\r' -> String.make ~len:1 ~char:ch :: acc
    | _ -> acc
  in
  List.reverse acc
  |> String.concat ""
  |> String.trim

let clean_member_signature = fun text ->
  text
  |> String.trim
  |> strip_prefix "|"
  |> String.trim
  |> strip_suffix ";"
  |> String.trim

let module_docstring = fun outer_doc inner_doc ->
  match (outer_doc, inner_doc) with
  | (Some outer, Some inner) when not (String.equal outer "") && not (String.equal inner "") ->
      Some (outer ^ "\n\n" ^ inner)
  | (Some outer, _) when not (String.equal outer "") -> Some outer
  | (_, Some inner) when not (String.equal inner "") -> Some inner
  | _ -> None

let is_ident_char = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '\'' -> true
  | _ -> false

let read_identifier = fun text idx ->
  if idx >= String.length text then
    None
  else if not (is_ident_char (String.get_unchecked text ~at:idx)) then
    None
  else
    let rec loop cursor =
      if cursor < String.length text && is_ident_char (String.get_unchecked text ~at:cursor) then
        loop (cursor + 1)
      else
        cursor
    in
    let finish = loop idx in
    Some (String.sub text ~offset:idx ~len:(finish - idx), finish)

let extract_percent_macro_names = fun line ->
  let rec loop idx acc =
    if idx >= String.length line then
      List.reverse acc
    else if String.get_unchecked line ~at:idx = '%' then
      match read_identifier line (idx + 1) with
      | Some (name, next_idx) -> loop next_idx (name :: acc)
      | None -> loop (idx + 1) acc
    else
      loop (idx + 1) acc
  in
  loop 0 []

let find_substring = fun text pattern ->
  let text_length = String.length text in
  let pattern_length = String.length pattern in
  let rec loop idx =
    if idx + pattern_length > text_length then
      None
    else if String.sub text ~offset:idx ~len:pattern_length = pattern then
      Some idx
    else
      loop (idx + 1)
  in
  loop 0

let extract_deriving_macro_names = fun line ->
  match find_substring line "@@deriving" with
  | None -> []
  | Some idx ->
      let rec loop cursor acc =
        if cursor >= String.length line then
          List.reverse acc
        else
          match String.get_unchecked line ~at:cursor with
          | ' '
          | '\t'
          | ','
          | '['
          | ']'
          | '('
          | ')' -> loop (cursor + 1) acc
          | _ -> (
              match read_identifier line cursor with
              | Some (name, next_idx) -> loop next_idx (name :: acc)
              | None -> List.reverse acc
            )
      in
      loop (idx + String.length "@@deriving") []

let macro_items_of_snippet = fun ?docstring snippet ->
  let signature = first_nonempty_line snippet in
  let names =
    extract_deriving_macro_names snippet @ extract_percent_macro_names snippet
    |> List.unique ~compare:String.compare
  in
  names
  |> List.map
    ~fn:(fun name ->
      {
        Doctree.kind = Doctree.Macro_item;
        name;
        anchor = slugify ("macro_" ^ name);
        signature;
        snippet;
        docstring;
        detail_groups = [];
      })

let make_item = fun ?docstring ?(detail_groups = []) ~kind ~name snippet ->
  {
    Doctree.kind;
    name;
    anchor = slugify (Doctree.item_kind_label kind ^ "_" ^ name);
    signature = first_nonempty_line snippet;
    snippet;
    docstring;
    detail_groups;
  }

let make_detail = fun ?docstring ~name signature ->
  ({ name = name; signature = clean_member_signature signature; docstring = docstring }:
    Doctree.item_detail)

let token_text = fun __tmp1 ->
  match __tmp1 with
  | Some token -> Syn.Ast.Token.text token
  | None -> ""

let ident_text = fun __tmp1 ->
  match __tmp1 with
  | Some ident -> Syn.Ast.Ident.text ident
  | None -> ""

let leading_docstrings = fun node ->
  let docs = Vector.with_capacity ~size:2 in
  (
    match Syn.Ast.Node.first_descendant_token node with
    | None -> ()
    | Some token ->
        iter_fold
          Syn.Ast.Token.fold_leading_trivia_item
          token
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Syn.Ast.Token.Docstring doc ->
                let text = clean_docstring doc.content in
                if not (String.equal text "") then
                  Vector.push docs ~value:text
            | Syn.Ast.Token.Whitespace
            | Syn.Ast.Token.Comment _ -> ())
  );
  vector_to_list docs

let leading_docstring = fun node ->
  leading_docstrings node
  |> docstring_of_docs

let collect_signature_items = fun for_each ->
  let items = Vector.with_capacity ~size:16 in
  for_each ~fn:(fun item -> Vector.push items ~value:item);
  vector_to_list items

let split_initial_docstrings = fun ~is_source_root docs ->
  if is_source_root then
    match docs with
    | [] -> (None, None)
    | [ doc ] -> (Some doc, None)
    | _ ->
        let reversed = List.reverse docs in
        let item_doc =
          match reversed with
          | head :: _ -> Some head
          | [] -> None
        in
        let overview_docs =
          match reversed with
          | _ :: tail -> List.reverse tail
          | [] -> []
        in
        let overview =
          if overview_docs = [] then
            None
          else
            Some (String.concat "\n\n" overview_docs)
        in
        (overview, item_doc)
  else
    (None, docstring_of_docs docs)

let type_member_name = fun member fallback ->
  match Syn.Ast.TypeDeclaration.Member.name member with
  | Some ident -> Syn.Ast.Ident.text ident
  | None -> fallback

let variant_constructor_details = fun variant ->
  let details = Vector.with_capacity ~size:4 in
  iter_fold
    Syn.Ast.VariantType.fold_constructor
    variant
    ~fn:(fun constructor ->
      let name = ident_text (Syn.Ast.VariantConstructor.name constructor) in
      if not (String.equal name "") then
        Vector.push
          details
          ~value:(
            make_detail
              ?docstring:(leading_docstring (Syn.Ast.VariantConstructor.as_node constructor))
              ~name
              (
                snippet_of_node (Syn.Ast.VariantConstructor.as_node constructor)
                |> strip_comments
              )
          ));
  vector_to_list details

let record_field_details = fun record ->
  let details = Vector.with_capacity ~size:4 in
  iter_fold
    Syn.Ast.RecordType.fold_field
    record
    ~fn:(fun field ->
      let name = ident_text (Syn.Ast.RecordField.name field) in
      if not (String.equal name "") then
        Vector.push
          details
          ~value:(
            make_detail
              ?docstring:(leading_docstring (Syn.Ast.RecordField.as_node field))
              ~name
              (
                snippet_of_node (Syn.Ast.RecordField.as_node field)
                |> strip_comments
              )
          ));
  vector_to_list details

let detail_groups_of_type_member = fun member ->
  match (
    Syn.Ast.TypeDeclaration.Member.variant_type member,
    Syn.Ast.TypeDeclaration.Member.record_type member
  ) with
  | (Some variant, _) ->
      let details = variant_constructor_details variant in
      if details = [] then
        []
      else
        [
          ({ title = "Constructors"; details = details }: Doctree.item_detail_group);
        ]
  | (None, Some record) ->
      let details = record_field_details record in
      if details = [] then
        []
      else
        [
          ({ title = "Fields"; details = details }: Doctree.item_detail_group);
        ]
  | (None, None) -> []

let items_of_type_declaration = fun ?docstring decl ->
  let raw_snippet = snippet_of_node (Syn.Ast.TypeDeclaration.as_node decl) in
  let snippet = strip_comments raw_snippet in
  let fallback_name = ident_text (Syn.Ast.TypeDeclaration.name decl) in
  let items = Vector.with_capacity ~size:2 in
  iter_fold
    Syn.Ast.TypeDeclaration.fold_member
    decl
    ~fn:(fun member ->
      let name = type_member_name member fallback_name in
      if not (String.equal name "") then
        Vector.push
          items
          ~value:(make_item
            ?docstring
            ~detail_groups:(detail_groups_of_type_member member)
            ~kind:Doctree.Type_item
            ~name
            snippet));
  if Vector.is_empty items && not (String.equal fallback_name "") then
    Vector.push
      items
      ~value:(make_item ?docstring ~kind:Doctree.Type_item ~name:fallback_name snippet);
  vector_to_list items @ macro_items_of_snippet ?docstring raw_snippet

let value_item_of_declaration = fun ?docstring decl ->
  let snippet = snippet_of_node (Syn.Ast.ValueDeclaration.as_node decl) in
  let name = ident_text (Syn.Ast.ValueDeclaration.name decl) in
  if String.equal name "" then
    macro_items_of_snippet ?docstring snippet
  else
    make_item ?docstring ~kind:Doctree.Function_item ~name snippet
    :: macro_items_of_snippet ?docstring snippet

let external_item_of_declaration = fun ?docstring decl ->
  let snippet = snippet_of_node (Syn.Ast.ExternalDeclaration.as_node decl) in
  let name = ident_text (Syn.Ast.ExternalDeclaration.name decl) in
  if String.equal name "" then
    macro_items_of_snippet ?docstring snippet
  else
    make_item ?docstring ~kind:Doctree.Function_item ~name snippet
    :: macro_items_of_snippet ?docstring snippet

let module_path_segments = fun decl ->
  let body_ident =
    match Syn.Ast.ModuleDeclaration.body_ident decl with
    | Some ident -> Some ident
    | None -> Syn.Ast.ModuleDeclaration.typeof_body_ident decl
  in
  match body_ident with
  | None -> []
  | Some ident ->
      let segments = Vector.with_capacity ~size:(Syn.Ast.Ident.segment_count ident) in
      iter_fold
        Syn.Ast.Ident.fold_segment
        ident
        ~fn:(fun token -> Vector.push segments ~value:(Syn.Ast.Token.text token));
      vector_to_list segments

let module_doc_of_empty = fun ~source_path ~path ?docstring ~snippet () ->
  let name =
    match List.reverse path with
    | head :: _ -> head
    | [] -> ""
  in
  Ok {
    Doctree.name = name;
    path;
    source_path;
    docstring;
    snippet;
    items = [];
    modules = [];
  }

let rec module_of_signature_items = fun
  ~lookup ~source ~source_path ~path ?docstring ~is_source_root ~snippet items ->
  let acc_items = Vector.with_capacity ~size:(List.length items) in
  let acc_modules = Vector.with_capacity ~size:4 in
  let overview = ref None in
  let seen_item = ref false in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] ->
        let name =
          match List.reverse path with
          | head :: _ -> head
          | [] -> ""
        in
        Ok {
          Doctree.name = name;
          path;
          source_path;
          docstring = module_docstring docstring !overview;
          snippet;
          items = vector_to_list acc_items;
          modules = vector_to_list acc_modules;
        }
    | item :: rest -> (
        let declaration = Syn.Ast.SignatureItem.declaration item in
        let leading_docs =
          match declaration with
          | Some node -> leading_docstrings node
          | None -> []
        in
        let (initial_overview, attached_doc) =
          if not !seen_item then
            split_initial_docstrings ~is_source_root leading_docs
          else
            (None, docstring_of_docs leading_docs)
        in
        if !overview = None then
          overview := initial_overview;
        seen_item := true;
        match Syn.Ast.SignatureItem.view item with
        | Syn.Ast.SignatureItem.Module decl ->
            let child_name = ident_text (Syn.Ast.ModuleDeclaration.name decl) in
            let child_path = path @ [ child_name ] in
            let child_snippet = snippet_of_node (Syn.Ast.ModuleDeclaration.as_node decl) in
            let nested_items =
              collect_signature_items (iter_fold Syn.Ast.ModuleDeclaration.fold_signature_item decl)
            in
            let* child =
              if nested_items != [] then
                module_of_signature_items
                  ~lookup
                  ~source
                  ~source_path
                  ~path:child_path
                  ?docstring:attached_doc
                  ~is_source_root:false
                  ~snippet:child_snippet
                  nested_items
              else
                match module_path_segments decl with
                | [] ->
                    module_doc_of_empty
                      ~source_path
                      ~path:child_path
                      ?docstring:attached_doc
                      ~snippet:child_snippet
                      ()
                | target_path -> (
                    match Source.resolve_module_path lookup ~current_path:path ~target_path with
                    | Some child_source ->
                        of_interface_source
                          ~lookup
                          ~path:child_path
                          ?docstring:attached_doc
                          child_source
                    | None ->
                        module_doc_of_empty
                          ~source_path
                          ~path:child_path
                          ?docstring:attached_doc
                          ~snippet:child_snippet
                          ()
                  )
            in
            Vector.push acc_modules ~value:child;
            loop rest
        | Syn.Ast.SignatureItem.ModuleType decl ->
            let name = ident_text (Syn.Ast.ModuleTypeDeclaration.name decl) in
            if not (String.equal name "") then
              Vector.push
                acc_items
                ~value:(make_item
                  ?docstring:attached_doc
                  ~kind:Doctree.Module_item
                  ~name
                  (snippet_of_node (Syn.Ast.ModuleTypeDeclaration.as_node decl)));
            loop rest
        | Syn.Ast.SignatureItem.Type (Syn.Ast.TypeDeclarationItem decl) ->
            items_of_type_declaration ?docstring:attached_doc decl
            |> List.for_each ~fn:(fun item -> Vector.push acc_items ~value:item);
            loop rest
        | Syn.Ast.SignatureItem.Value decl ->
            value_item_of_declaration ?docstring:attached_doc decl
            |> List.for_each ~fn:(fun item -> Vector.push acc_items ~value:item);
            loop rest
        | Syn.Ast.SignatureItem.External decl ->
            external_item_of_declaration ?docstring:attached_doc decl
            |> List.for_each ~fn:(fun item -> Vector.push acc_items ~value:item);
            loop rest
        | Syn.Ast.SignatureItem.Type (Syn.Ast.TypeExtensionItem _)
        | Syn.Ast.SignatureItem.Open _
        | Syn.Ast.SignatureItem.Include _
        | Syn.Ast.SignatureItem.Exception _
        | Syn.Ast.SignatureItem.Extension _
        | Syn.Ast.SignatureItem.Attribute _
        | Syn.Ast.SignatureItem.Error _
        | Syn.Ast.SignatureItem.Unknown _ -> loop rest
      )
  in
  loop items

and of_interface_source = fun ~lookup ?path ?docstring (source_file: Source.interface_source) ->
  let* source_slice =
    IO.IoVec.IoSlice.from_string source_file.content
    |> Result.map_err
      ~fn:(fun error ->
        "failed to read source for "
        ^ Path.to_string source_file.relative_path
        ^ ": "
        ^ IO.IoSlice.error_message error)
  in
  let parsed = Syn.parse_interface source_slice in
  let source = source_file.content in
  let root = Syn.Ast.SourceFile.make parsed.tree in
  let items = collect_signature_items (iter_fold Syn.Ast.SourceFile.fold_signature_item root) in
  let path =
    match path with
    | Some path -> path
    | None -> source_file.module_path
  in
  module_of_signature_items
    ~lookup
    ~source
    ~source_path:source_file.relative_path
    ~path
    ?docstring
    ~is_source_root:true
    ~snippet:(String.trim source)
    items
