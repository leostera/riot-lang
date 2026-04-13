open Std

let ( let* ) value fn = Result.and_then value ~fn

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
  raw |> String.split ~by:"\n" |> List.map ~fn:(fun line ->
      let trimmed = line |> String.trim |> strip_prefix "(**" |> strip_suffix "*)" |> String.trim in
      if String.starts_with ~prefix:"*" trimmed then
        trimmed |> strip_prefix "*" |> String.trim
      else
        trimmed) |> String.concat "\n" |> String.trim

let first_nonempty_line = fun text ->
  let rec loop = function
    | [] -> ""
    | line :: rest ->
        if String.trim line = "" then
          loop rest
        else
          String.trim line
  in
  loop (String.split ~by:"\n" text)

let docstring_of_docs = function
  | [] -> None
  | docs -> Some (String.concat "\n\n" docs)

let find_substring_from = fun text pattern start_idx ->
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
  loop start_idx

let slugify = fun text ->
  text |> String.map ~fn:(fun ch ->
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

let snippet_of_node = fun source syntax_node ->
  let span = Syn.Cst.token_body_span syntax_node in
  let start_ = Int.max 0 span.start in
  let finish = Int.min (String.length source) span.end_ in
  if finish <= start_ then
    ""
  else
    String.sub source ~offset:start_ ~len:(finish - start_) |> String.trim

let strip_comments = fun text ->
  let depth, pending, acc =
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
      ~acc:(0, None, [])
      text
  in
  let acc =
    match pending with
    | Some ch when depth = 0 || ch = '\n' || ch = '\r' -> String.make ~len:1 ~char:ch :: acc
    | _ -> acc
  in
  List.reverse acc |> String.concat "" |> String.trim

let docstrings_in_range = fun source ~start_offset ~end_offset ->
  let start_offset = Int.max 0 start_offset in
  let end_offset = Int.min (String.length source) end_offset in
  if end_offset <= start_offset then
    []
  else
    let text = String.sub source ~offset:start_offset ~len:(end_offset - start_offset) in
    let rec loop idx acc =
      match find_substring_from text "(**" idx with
      | None -> List.reverse acc
      | Some open_idx -> (
          match find_substring_from text "*)" (open_idx + 3) with
          | None -> List.reverse acc
          | Some close_idx ->
              let raw = String.sub text ~offset:open_idx ~len:((close_idx + 2) - open_idx) in
              let doc = clean_docstring raw in
              let acc =
                if String.equal doc "" then
                  acc
                else
                  doc :: acc
              in
              loop (close_idx + 2) acc
        )
    in
    loop 0 []

let clean_member_signature = fun text ->
  text |> String.trim |> strip_prefix "|" |> String.trim |> strip_suffix ";" |> String.trim

let module_docstring = fun outer_doc inner_doc ->
  match outer_doc, inner_doc with
  | Some outer, Some inner when not (String.equal outer "") && not (String.equal inner "") -> Some (outer
  ^ "\n\n"
  ^ inner)
  | Some outer, _ when not (String.equal outer "") -> Some outer
  | _, Some inner when not (String.equal inner "") -> Some inner
  | _ -> None

let is_ident_char = function
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
  let names = extract_deriving_macro_names snippet @ extract_percent_macro_names snippet
  |> List.unique ~compare:String.compare in
  names |> List.map ~fn:(fun name ->
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

let value_name = fun name_tokens -> name_tokens |> List.map ~fn:Syn.Cst.Token.text |> String.concat ""

let docstrings_before_node = fun ~after_offset syntax_node ->
  Syn.Cst.leading_trivia_before_node ~after:after_offset syntax_node |> List.filter_map ~fn:(fun trivia ->
      match trivia with
      | Syn.Cst.Trivia.Docstring doc ->
          let text = clean_docstring (Syn.Cst.Docstring.text doc) in
          if String.equal text "" then
            None
          else
            Some text
      | Syn.Cst.Trivia.Comment _ -> None)

let combine_docstrings = fun left right ->
  match left, right with
  | Some left, Some right when not (String.equal left "") && not (String.equal right "") -> Some (left
  ^ "\n\n"
  ^ right)
  | Some left, _ when not (String.equal left "") -> Some left
  | _, Some right when not (String.equal right "") -> Some right
  | _ -> None

let make_detail = fun ?docstring ~name signature ->
  (
    { name = name; signature = clean_member_signature signature; docstring = docstring }: Doctree.item_detail
  )

let variant_constructor_details = fun source ~end_offset syntax_node constructors ->
  match constructors with
  | [] -> []
  | first :: rest ->
      let start_offset = (Syn.Cst.token_body_span syntax_node).start in
      let detail_of_constructor ?docstring constructor = make_detail
        ?docstring
        ~name:(Syn.Cst.VariantConstructor.name constructor)
        (snippet_of_node source (Syn.Cst.VariantConstructor.syntax_node constructor)) in
      let rec loop previous previous_doc previous_end acc = function
        | next :: tail ->
            let gap_doc = docstrings_in_range
              source
              ~start_offset:previous_end
              ~end_offset:(Syn.Cst.token_body_span (Syn.Cst.VariantConstructor.syntax_node next)).start
            |> docstring_of_docs in
            let previous_doc = combine_docstrings previous_doc gap_doc in
            let previous_detail = detail_of_constructor ?docstring:previous_doc previous in
            loop
              next
              None
              (Syn.Cst.token_body_span (Syn.Cst.VariantConstructor.syntax_node next)).end_
              (previous_detail :: acc)
              tail
        | [] ->
            let gap_doc = docstrings_in_range source ~start_offset:previous_end ~end_offset |> docstring_of_docs in
            let previous_doc = combine_docstrings previous_doc gap_doc in
            List.reverse (detail_of_constructor ?docstring:previous_doc previous :: acc)
      in
      let first_doc = docstrings_in_range
        source
        ~start_offset
        ~end_offset:(Syn.Cst.token_body_span (Syn.Cst.VariantConstructor.syntax_node first)).start
      |> docstring_of_docs in
      loop
        first
        first_doc
        (Syn.Cst.token_body_span (Syn.Cst.VariantConstructor.syntax_node first)).end_
        []
        rest

let record_field_details = fun source syntax_node fields ->
  let start_offset = (Syn.Cst.token_body_span syntax_node).start in
  let rec loop after_offset acc = function
    | [] -> List.reverse acc
    | field :: rest ->
        let field_node = Syn.Cst.RecordField.syntax_node field in
        let field_docstring = docstrings_before_node ~after_offset field_node |> docstring_of_docs in
        let detail = make_detail
          ?docstring:field_docstring
          ~name:(Syn.Cst.RecordField.name field)
          (snippet_of_node source field_node) in
        loop (Syn.Cst.token_body_span field_node).end_ (detail :: acc) rest
  in
  loop start_offset [] fields

let detail_groups_of_type_definition = fun source ~end_offset ->
  function
  | Syn.Cst.TypeDefinition.Variant { syntax_node; constructors } ->
      let details = variant_constructor_details source ~end_offset syntax_node constructors in
      if details = [] then
        []
      else
        [ ({ title = "Constructors"; details = details }: Doctree.item_detail_group) ]
  | Syn.Cst.TypeDefinition.Record { syntax_node; fields; _ } ->
      let details = record_field_details source syntax_node fields in
      if details = [] then
        []
      else
        [ ({ title = "Fields"; details = details }: Doctree.item_detail_group) ]
  | _ ->
      []

let rec items_of_type_declaration = fun ?docstring source decl ->
  let raw_snippet = snippet_of_node source (Syn.Cst.TypeDeclaration.syntax_node decl) in
  let snippet = strip_comments raw_snippet in
  let declaration_end = (Syn.Cst.token_body_span (Syn.Cst.TypeDeclaration.syntax_node decl)).end_ in
  let item = make_item
    ?docstring
    ~detail_groups:(detail_groups_of_type_definition
      source
      ~end_offset:declaration_end
      (Syn.Cst.TypeDeclaration.type_definition decl))
    ~kind:Doctree.Type_item
    ~name:(Syn.Cst.Token.text (Syn.Cst.TypeDeclaration.name_token decl))
    snippet in
  let macros = macro_items_of_snippet ?docstring raw_snippet in
  match Syn.Cst.TypeDeclaration.next_and_declaration decl with
  | Some next_decl -> item :: macros @ items_of_type_declaration source next_decl
  | None -> item :: macros

let value_item_of_declaration = fun ?docstring source decl ->
  let snippet = snippet_of_node source (Syn.Cst.ValueDeclaration.syntax_node decl) in
  let item = make_item
    ?docstring
    ~kind:Doctree.Function_item
    ~name:(value_name (Syn.Cst.ValueDeclaration.name_tokens decl))
    snippet in
  item :: macro_items_of_snippet ?docstring snippet

let external_item_of_declaration = fun ?docstring source (decl: Syn.Cst.external_declaration) ->
  let snippet = snippet_of_node source decl.syntax_node in
  let item = make_item ?docstring ~kind:Doctree.Function_item ~name:(value_name decl.name_tokens) snippet in
  item :: macro_items_of_snippet ?docstring snippet

let signature_items_of_module_type = fun module_type ->
  match Syn.CstBuilder.signature_items_of_module_type module_type with
  | Ok items -> items
  | Error _ -> []

let split_initial_docstrings = fun ~is_source_root docs ->
  if is_source_root then
    match docs with
    | [] ->
        (None, None)
    | [ doc ] ->
        (Some doc, None)
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
    (
      None,
      (
        match docs with
        | [] -> None
        | _ -> Some (String.concat "\n\n" docs)
      )
    )

let attach_docstring_to_constructor_groups = fun doc (detail_groups: Doctree.item_detail_group list) ->
  let rec loop prefix = function
    | [] -> (List.reverse prefix, false)
    | (group: Doctree.item_detail_group) :: rest ->
        if not (String.equal group.title "Constructors") then
          loop (group :: prefix) rest
        else
          match List.reverse group.details with
          | [] -> (List.reverse_append prefix (group :: rest), false)
          | (last_detail: Doctree.item_detail) :: rev_tail ->
              let updated_group = {
                group
                with details = List.reverse
                  ({
                    last_detail
                    with docstring = combine_docstrings last_detail.docstring (Some doc)
                  }
                  :: rev_tail)
              } in
              (List.reverse_append prefix (updated_group :: rest), true)
  in
  loop [] detail_groups

let attach_pending_doc_to_recent_variant = fun pending_doc (acc_items: Doctree.item list) ->
  match pending_doc with
  | None -> (acc_items, None)
  | Some doc ->
      let rec loop prefix = function
        | [] -> (List.reverse prefix, Some doc)
        | (item: Doctree.item) :: rest ->
            if item.kind = Doctree.Macro_item then
              loop (item :: prefix) rest
            else if item.kind = Doctree.Type_item then
              let updated_groups, attached = attach_docstring_to_constructor_groups doc item.detail_groups in
              if attached then
                (List.reverse_append prefix ({ item with detail_groups = updated_groups } :: rest), None)
              else
                (List.reverse_append prefix (item :: rest), Some doc)
            else
              (List.reverse_append prefix (item :: rest), Some doc)
      in
      loop [] acc_items

let split_leading_docs_for_previous_variant = fun acc_items docs ->
  match List.reverse docs with
  | [] ->
      (acc_items, docs)
  | [ _ ] ->
      (acc_items, docs)
  | current_doc :: previous_docs_rev ->
      let previous_doc = String.concat "\n\n" (List.reverse previous_docs_rev) in
      let updated_acc_items, remainder = attach_pending_doc_to_recent_variant (Some previous_doc) acc_items in
      (
        match remainder with
        | None -> (updated_acc_items, [ current_doc ])
        | Some _ -> (acc_items, docs)
      )

let rec module_expression_path_segments = function
  | Syn.Cst.ModuleExpression.Path ident -> Some (Syn.Cst.Ident.segments ident
  |> List.map ~fn:Syn.Cst.Token.text)
  | Syn.Cst.ModuleExpression.Constraint { module_expression; _ }
  | Syn.Cst.ModuleExpression.Attribute { module_expression; _ } -> module_expression_path_segments module_expression
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } -> module_expression_path_segments inner
  | _ -> None

let rec module_of_signature_items = fun ~lookup ~source ~source_path ~path ?docstring ~is_source_root ~snippet items ->
  let rec loop after_offset overview pending_doc acc_items acc_modules = function
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
          docstring = module_docstring docstring (combine_docstrings overview pending_doc);
          snippet;
          items = List.reverse acc_items;
          modules = List.reverse acc_modules;
        }
    | item :: rest -> (
        match item with
        | Syn.Cst.SignatureItem.Docstring doc ->
            let text = clean_docstring (Syn.Cst.Docstring.text doc) in
            if
              overview = None && acc_items = [] && acc_modules = [] && pending_doc = None && is_source_root
            then
              loop after_offset (Some text) pending_doc acc_items acc_modules rest
            else
              let pending_doc = combine_docstrings pending_doc (Some text) in
              loop after_offset overview pending_doc acc_items acc_modules rest
        | Syn.Cst.SignatureItem.Comment _ ->
            loop after_offset overview pending_doc acc_items acc_modules rest
        | Syn.Cst.SignatureItem.ModuleDeclaration decl ->
            let syntax_node = Syn.Cst.ModuleSignature.syntax_node decl in
            let leading_docs = docstrings_before_node ~after_offset syntax_node in
            let acc_items, leading_docs = split_leading_docs_for_previous_variant acc_items leading_docs in
            let initial_overview, attached_doc = split_initial_docstrings ~is_source_root leading_docs in
            let child_name = Syn.Cst.ModuleSignature.name decl in
            let child_snippet = snippet_of_node source syntax_node in
            let next_overview =
              match overview, initial_overview with
              | None, Some doc -> Some doc
              | _ -> overview
            in
            let child_docstring = combine_docstrings pending_doc attached_doc in
            let child_items =
              match Syn.Cst.ModuleSignature.module_type decl with
              | Some module_type -> signature_items_of_module_type module_type
              | None -> []
            in
            let child_path = path @ [ child_name ] in
            let* child_module =
              if child_items != [] then
                module_of_signature_items
                  ~lookup
                  ~source
                  ~source_path
                  ~path:child_path
                  ?docstring:child_docstring
                  ~is_source_root:false
                  ~snippet:child_snippet
                  child_items
              else
                match Syn.Cst.ModuleSignature.module_expression decl with
                | Some module_expression -> (
                    match module_expression_path_segments module_expression with
                    | Some target_path -> (
                        match Source.resolve_module_path lookup ~current_path:path ~target_path with
                        | Some child_source -> of_interface_source
                          ~lookup
                          ~path:child_path
                          ?docstring:child_docstring
                          child_source
                        | None -> module_of_signature_items
                          ~lookup
                          ~source
                          ~source_path
                          ~path:child_path
                          ?docstring:child_docstring
                          ~is_source_root:false
                          ~snippet:child_snippet
                          []
                      )
                    | None -> module_of_signature_items
                      ~lookup
                      ~source
                      ~source_path
                      ~path:child_path
                      ?docstring:child_docstring
                      ~is_source_root:false
                      ~snippet:child_snippet
                      []
                  )
                | None -> module_of_signature_items
                  ~lookup
                  ~source
                  ~source_path
                  ~path:child_path
                  ?docstring:child_docstring
                  ~is_source_root:false
                  ~snippet:child_snippet
                  []
            in
            loop
              (Syn.Cst.token_body_span syntax_node).end_
              next_overview
              None
              acc_items
              (child_module :: acc_modules)
              rest
        | Syn.Cst.SignatureItem.TypeDeclaration decl ->
            let syntax_node = Syn.Cst.TypeDeclaration.syntax_node decl in
            let leading_docs = docstrings_before_node ~after_offset syntax_node in
            let acc_items, leading_docs = split_leading_docs_for_previous_variant acc_items leading_docs in
            let initial_overview, attached_doc = split_initial_docstrings ~is_source_root leading_docs in
            let next_overview =
              match overview, initial_overview with
              | None, Some doc -> Some doc
              | _ -> overview
            in
            let new_items = items_of_type_declaration
              ?docstring:(combine_docstrings pending_doc attached_doc)
              source
              decl in
            loop
              (Syn.Cst.token_body_span syntax_node).end_
              next_overview
              None
              (List.reverse_append new_items acc_items)
              acc_modules
              rest
        | Syn.Cst.SignatureItem.ValueDeclaration decl ->
            let syntax_node = Syn.Cst.ValueDeclaration.syntax_node decl in
            let leading_docs = docstrings_before_node ~after_offset syntax_node in
            let acc_items, leading_docs = split_leading_docs_for_previous_variant acc_items leading_docs in
            let initial_overview, attached_doc = split_initial_docstrings ~is_source_root leading_docs in
            let next_overview =
              match overview, initial_overview with
              | None, Some doc -> Some doc
              | _ -> overview
            in
            let new_items = value_item_of_declaration
              ?docstring:(combine_docstrings pending_doc attached_doc)
              source
              decl in
            loop
              (Syn.Cst.token_body_span syntax_node).end_
              next_overview
              None
              (List.reverse_append new_items acc_items)
              acc_modules
              rest
        | Syn.Cst.SignatureItem.ExternalDeclaration decl ->
            let syntax_node = decl.syntax_node in
            let leading_docs = docstrings_before_node ~after_offset syntax_node in
            let acc_items, leading_docs = split_leading_docs_for_previous_variant acc_items leading_docs in
            let initial_overview, attached_doc = split_initial_docstrings ~is_source_root leading_docs in
            let next_overview =
              match overview, initial_overview with
              | None, Some doc -> Some doc
              | _ -> overview
            in
            let new_items = external_item_of_declaration
              ?docstring:(combine_docstrings pending_doc attached_doc)
              source
              decl in
            loop
              (Syn.Cst.token_body_span syntax_node).end_
              next_overview
              None
              (List.reverse_append new_items acc_items)
              acc_modules
              rest
        | _ ->
            let syntax_node = Syn.Cst.SignatureItem.syntax_node item in
            loop (Syn.Cst.token_body_span syntax_node).end_ overview None acc_items acc_modules rest
      )
  in
  loop 0 None None [] [] items

and of_interface_source = fun ~lookup ?path ?docstring (source_file: Source.interface_source) ->
  let parsed = Syn.parse_interface source_file.content in
  let* cst = Syn.build_cst parsed
  |> Result.map_err ~fn:(fun _ -> "failed to build CST for " ^ Path.to_string source_file.relative_path) in
  match Syn.Cst.SourceFile.signature_items cst with
  | Some items ->
      let path =
        match path with
        | Some path -> path
        | None -> source_file.module_path
      in
      module_of_signature_items
        ~lookup
        ~source:source_file.content
        ~source_path:source_file.relative_path
        ~path
        ?docstring
        ~is_source_root:true
        ~snippet:(String.trim source_file.content)
        items
  | None -> Error ("expected interface CST for " ^ Path.to_string source_file.relative_path)
