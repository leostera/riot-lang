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

let is_blank_line = fun line -> String.equal (String.trim line) ""

let trim_outer_blank_lines = fun lines ->
  let rec drop_front = fun __tmp1 ->
    match __tmp1 with
    | [] -> []
    | line :: rest when is_blank_line line -> drop_front rest
    | lines -> lines
  in
  lines
  |> drop_front
  |> List.reverse
  |> drop_front
  |> List.reverse

let leading_indent = fun line ->
  let length = String.length line in
  let rec loop index =
    if index >= length then
      index
    else
      match String.get_unchecked line ~at:index with
      | ' '
      | '\t' -> loop (index + 1)
      | _ -> index
  in
  loop 0

let remove_indent = fun count line ->
  let length = String.length line in
  let rec loop index remaining =
    if index >= length || remaining <= 0 then
      index
    else
      match String.get_unchecked line ~at:index with
      | ' '
      | '\t' -> loop (index + 1) (remaining - 1)
      | _ -> index
  in
  let offset = loop 0 count in
  String.sub line ~offset ~len:(length - offset)

let common_indent = fun lines ->
  lines
  |> List.filter ~fn:(fun line -> not (is_blank_line line))
  |> List.fold_left
    ~init:None
    ~fn:(fun acc line ->
      let indent = leading_indent line in
      match acc with
      | None -> Some indent
      | Some existing -> Some (Int.min existing indent))
  |> Option.unwrap_or ~default:0

let strip_doc_star_prefix = fun line ->
  let length = String.length line in
  let indent = leading_indent line in
  if indent < length && Char.equal (String.get_unchecked line ~at:indent) '*' then
    let offset =
      if indent + 1 < length && Char.equal (String.get_unchecked line ~at:(indent + 1)) ' ' then
        indent + 2
      else
        indent + 1
    in
    String.sub line ~offset ~len:(length - offset)
  else
    line

let has_doc_star_prefix = fun line ->
  let length = String.length line in
  let indent = leading_indent line in
  indent < length && Char.equal (String.get_unchecked line ~at:indent) '*'

let strip_doc_star_prefixes = fun lines ->
  let nonblank = List.filter lines ~fn:(fun line -> not (is_blank_line line)) in
  if not (List.is_empty nonblank) && List.all nonblank ~fn:has_doc_star_prefix then
    List.map
      lines
      ~fn:(fun line ->
        if is_blank_line line then
          line
        else
          strip_doc_star_prefix line)
  else
    lines

let strip_doc_opening = fun line ->
  let trimmed = String.trim line in
  if String.starts_with ~prefix:"(**" trimmed then
    strip_prefix "(**" trimmed
  else
    line

let strip_doc_closing = fun line ->
  let trimmed = String.trim line in
  if String.ends_with ~suffix:"*)" trimmed then
    strip_suffix "*)" trimmed
  else
    line

let clean_docstring = fun raw ->
  raw
  |> String.split ~by:"\n"
  |> (fun lines ->
    match lines with
    | [] -> []
    | first :: rest -> (
        match List.reverse rest with
        | [] ->
            [
              first
              |> strip_doc_opening
              |> strip_doc_closing
              |> String.trim;
            ]
        | last :: middle ->
            let first = strip_doc_opening first in
            let last = strip_doc_closing last in
            first :: (List.reverse middle @ [ last ])
      ))
  |> trim_outer_blank_lines
  |> (fun lines ->
    let indent = common_indent lines in
    List.map lines ~fn:(remove_indent indent))
  |> strip_doc_star_prefixes
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

let collapse_blank_lines = fun text ->
  text
  |> String.split ~by:"\n"
  |> List.filter ~fn:(fun line -> not (String.equal (String.trim line) ""))
  |> String.concat "\n"
  |> String.trim

let clean_signature = fun text ->
  text
  |> strip_comments
  |> collapse_blank_lines

let clean_member_signature = fun text ->
  text
  |> String.trim
  |> clean_signature
  |> String.trim
  |> strip_suffix ";"
  |> String.trim

let rec type_expr_is_function = fun type_expr ->
  match Syn.Ast.TypeExpr.view type_expr with
  | Syn.Ast.TypeExpr.Arrow _ -> true
  | Syn.Ast.TypeExpr.Forall { body; _ } -> type_expr_is_function body
  | _ -> false

let value_item_kind = fun annotation ->
  if type_expr_is_function annotation then
    Doctree.Function_item
  else
    Doctree.Value_item

let signature_of_value_annotation = fun name annotation ->
  let annotation =
    Syn.Ast.TypeExpr.as_node annotation
    |> snippet_of_node
    |> clean_member_signature
  in
  if String.equal annotation "" then
    name
  else
    name ^ " : " ^ annotation

let module_docstring = fun outer_doc inner_doc ->
  match (outer_doc, inner_doc) with
  | (Some outer, Some inner) when not (String.equal outer "") && not (String.equal inner "") ->
      Some (outer ^ "\n\n" ^ inner)
  | (Some outer, _) when not (String.equal outer "") -> Some outer
  | (_, Some inner) when not (String.equal inner "") -> Some inner
  | _ -> None

let make_item = fun ?docstring ?(detail_groups = []) ?signature ~kind ~name snippet ->
  {
    Doctree.kind;
    name;
    anchor = slugify (Doctree.item_kind_label kind ^ "_" ^ name);
    signature = Option.unwrap_or ~default:(first_nonempty_line snippet) signature;
    snippet;
    docstring;
    detail_groups;
  }

let make_detail = fun ?docstring ~name signature ->
  ({ name = name; signature = clean_member_signature signature; docstring = docstring }:
    Doctree.item_detail)

let type_expr_signature = fun type_expr ->
  Syn.Ast.TypeExpr.as_node type_expr
  |> snippet_of_node
  |> clean_member_signature

let record_type_signature = fun record ->
  Syn.Ast.RecordType.as_node record
  |> snippet_of_node
  |> clean_member_signature

let constructor_payload_signature = fun __tmp1 ->
  match __tmp1 with
  | Syn.Ast.VariantConstructor.TypeExpr type_expr -> type_expr_signature type_expr
  | Syn.Ast.VariantConstructor.Record record -> record_type_signature record

let constructor_signature = fun name rhs ->
  match rhs with
  | Syn.Ast.VariantConstructor.Plain -> name
  | Syn.Ast.VariantConstructor.Payload { payload; _ } ->
      let payload_signature = constructor_payload_signature payload in
      if String.equal payload_signature "" then
        name
      else
        name ^ " of " ^ payload_signature
  | Syn.Ast.VariantConstructor.Gadt { record_payload; arrow_token; result; _ } -> (
      let result_signature = type_expr_signature result in
      match record_payload with
      | Some record ->
          let record_signature = record_type_signature record in
          if String.equal record_signature "" then
            name ^ " : " ^ result_signature
          else if Option.is_some arrow_token then
            name ^ " : " ^ record_signature ^ " -> " ^ result_signature
          else
            name ^ " : " ^ record_signature
      | None ->
          if String.equal result_signature "" then
            name
          else
            name ^ " : " ^ result_signature
    )

let make_constructor_detail = fun ?docstring ~name ~signature () ->
  ({ name = name; signature = signature; docstring = docstring }: Doctree.item_detail)

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
      match Syn.Ast.VariantConstructor.view constructor with
      | Syn.Ast.VariantConstructor.Constructor { name; rhs; _ } ->
          let name = Syn.Ast.Ident.text name in
          if not (String.equal name "") then
            Vector.push
              details
              ~value:(make_constructor_detail
                ?docstring:(leading_docstring (Syn.Ast.VariantConstructor.as_node constructor))
                ~name
                ~signature:(constructor_signature name rhs)
                ())
      | Syn.Ast.VariantConstructor.Unknown _ -> ());
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
          ~value:(make_detail
            ?docstring:(leading_docstring (Syn.Ast.RecordField.as_node field))
            ~name
            (snippet_of_node (Syn.Ast.RecordField.as_node field))));
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
  let snippet = clean_signature raw_snippet in
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
  vector_to_list items

let value_item_of_declaration = fun ?docstring decl ->
  let raw_snippet = snippet_of_node (Syn.Ast.ValueDeclaration.as_node decl) in
  let name = ident_text (Syn.Ast.ValueDeclaration.name decl) in
  match Syn.Ast.ValueDeclaration.type_annotation decl with
  | None -> []
  | Some annotation ->
      if String.equal name "" then
        []
      else
        let snippet = clean_signature raw_snippet in
        let signature = signature_of_value_annotation name annotation in
        let kind = value_item_kind annotation in
        [ make_item ?docstring ~kind ~name ~signature snippet; ]

let external_item_of_declaration = fun ?docstring decl ->
  let raw_snippet = snippet_of_node (Syn.Ast.ExternalDeclaration.as_node decl) in
  let name = ident_text (Syn.Ast.ExternalDeclaration.name decl) in
  match Syn.Ast.ExternalDeclaration.type_annotation decl with
  | None -> []
  | Some annotation ->
      if String.equal name "" then
        []
      else
        let snippet = clean_signature raw_snippet in
        let signature = signature_of_value_annotation name annotation in
        let kind = value_item_kind annotation in
        [ make_item ?docstring ~kind ~name ~signature snippet; ]

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
                        from_interface_source
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

and from_interface_source = fun ~lookup ?path ?docstring (source_file: Source.interface_source) ->
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
