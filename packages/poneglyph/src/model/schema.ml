open Std

type def = Uri.t * Fact.t list

let namespace name = Uri.ns name

let kind ~ns name =
  let uri = Uri.make [ ns; Uri.kind name ] in
  (uri, [])

let field ~ns name =
  let uri = Uri.make [ ns; Uri.field name ] in
  (uri, [])

let add_fact (uri, facts) attribute value stated_at =
  let source = Uri.of_string "poneglyph:schema:bootstrap" in
  let fact = Fact.make ~source ~entity:uri ~attribute ~value ~stated_at ~tx_id:0 in
  (uri, fact :: facts)

let doc doc_str (uri, facts) =
  add_fact (uri, facts)
    (Uri.of_string "@field:doc")
    (Fact.String doc_str) (Datetime.now ())

let used_on (target_uri, _) (uri, facts) =
  add_fact (uri, facts)
    (Uri.of_string "@field:used_on")
    (Fact.Uri target_uri) (Datetime.now ())

let value_type type_uri (uri, facts) =
  add_fact (uri, facts)
    (Uri.of_string "@field:value_type")
    (Fact.Uri type_uri) (Datetime.now ())

let cardinality card (uri, facts) =
  add_fact (uri, facts)
    (Uri.of_string "@field:cardinality")
    (Fact.String card) (Datetime.now ())

let required req (uri, facts) =
  add_fact (uri, facts)
    (Uri.of_string "@field:required")
    (Fact.Bool req) (Datetime.now ())

(* Type URIs *)
module Type = struct
  let string = Uri.of_string "@type:string"
  let int = Uri.of_string "@type:int"
  let bool = Uri.of_string "@type:bool"
  let float = Uri.of_string "@type:float"
  let uri = Uri.of_string "@type:uri"
  let datetime = Uri.of_string "@type:datetime"
end

(* Fact value builders *)
let string_value ~field ~value entity =
  let attr = fst field in
  let source = Uri.of_string "poneglyph:schema:user" in
  Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String value)
    ~stated_at:(Datetime.now ()) ~tx_id:0

let int_value ~field ~value entity =
  let attr = fst field in
  let source = Uri.of_string "poneglyph:schema:user" in
  Fact.make ~source ~entity ~attribute:attr ~value:(Fact.Int value)
    ~stated_at:(Datetime.now ()) ~tx_id:0

let bool_value ~field ~value entity =
  let attr = fst field in
  let source = Uri.of_string "poneglyph:schema:user" in
  Fact.make ~source ~entity ~attribute:attr ~value:(Fact.Bool value)
    ~stated_at:(Datetime.now ()) ~tx_id:0

let float_value ~field ~value entity =
  let attr = fst field in
  let source = Uri.of_string "poneglyph:schema:user" in
  Fact.make ~source ~entity ~attribute:attr ~value:(Fact.Float value)
    ~stated_at:(Datetime.now ()) ~tx_id:0

let uri_value ~field ~value entity =
  let attr = fst field in
  let source = Uri.of_string "poneglyph:schema:user" in
  Fact.make ~source ~entity ~attribute:attr ~value:(Fact.Uri value)
    ~stated_at:(Datetime.now ()) ~tx_id:0

let datetime_value ~field ~value entity =
  let attr = fst field in
  let source = Uri.of_string "poneglyph:schema:user" in
  Fact.make ~source ~entity ~attribute:attr ~value:(Fact.DateTime value)
    ~stated_at:(Datetime.now ()) ~tx_id:0

let bootstrap ~stated_at =
  let ns = namespace "poneglyph" in

  let source = Uri.of_string "poneglyph:schema:bootstrap" in
  let make_fact ~entity ~attribute ~value =
    Fact.make ~source ~entity ~attribute ~value ~stated_at ~tx_id:0
  in

  let schema_uri = Uri.of_string "@schema" in
  let kind_kind = Uri.of_string "@kind:kind" in
  let kind_schema = Uri.of_string "@kind:schema" in
  let kind_field = Uri.of_string "@kind:field" in
  let kind_type = Uri.of_string "@kind:type" in

  let field_instance_of = Uri.of_string "@field:instance_of" in
  let field_type = Uri.of_string "@field:type" in
  let field_doc = Uri.of_string "@field:doc" in
  let field_name = Uri.of_string "@field:name" in
  let field_namespace = Uri.of_string "@field:namespace" in
  let field_used_on = Uri.of_string "@field:used_on" in
  let field_value_type = Uri.of_string "@field:value_type" in
  let field_cardinality = Uri.of_string "@field:cardinality" in
  let field_required = Uri.of_string "@field:required" in

  let type_string = Uri.of_string "@type:string" in
  let type_int = Uri.of_string "@type:int" in
  let type_bool = Uri.of_string "@type:bool" in
  let type_float = Uri.of_string "@type:float" in
  let type_uri = Uri.of_string "@type:uri" in
  let type_datetime = Uri.of_string "@type:datetime" in

  [
    (* @schema itself *)
    make_fact ~entity:schema_uri ~attribute:field_instance_of
      ~value:(Uri kind_schema);
    make_fact ~entity:schema_uri ~attribute:field_type ~value:(String "schema");
    make_fact ~entity:schema_uri ~attribute:field_doc
      ~value:(String "Poneglyph core schema - defines how to define schemas");
    (* @kind:schema *)
    make_fact ~entity:kind_schema ~attribute:field_instance_of
      ~value:(Uri kind_kind);
    make_fact ~entity:kind_schema ~attribute:field_type ~value:(String "kind");
    make_fact ~entity:kind_schema ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:kind_schema ~attribute:field_doc
      ~value:(String "A schema namespace definition");
    (* @kind:kind *)
    make_fact ~entity:kind_kind ~attribute:field_instance_of
      ~value:(Uri kind_kind);
    make_fact ~entity:kind_kind ~attribute:field_type ~value:(String "kind");
    make_fact ~entity:kind_kind ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:kind_kind ~attribute:field_doc
      ~value:(String "An entity type/kind definition");
    (* @kind:field *)
    make_fact ~entity:kind_field ~attribute:field_instance_of
      ~value:(Uri kind_kind);
    make_fact ~entity:kind_field ~attribute:field_type ~value:(String "kind");
    make_fact ~entity:kind_field ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:kind_field ~attribute:field_doc
      ~value:(String "An attribute/field definition");
    (* @kind:type *)
    make_fact ~entity:kind_type ~attribute:field_instance_of
      ~value:(Uri kind_kind);
    make_fact ~entity:kind_type ~attribute:field_type ~value:(String "kind");
    make_fact ~entity:kind_type ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:kind_type ~attribute:field_doc
      ~value:(String "A value type definition");
    (* @field:instance_of *)
    make_fact ~entity:field_instance_of ~attribute:field_instance_of
      ~value:(Uri kind_field);
    make_fact ~entity:field_instance_of ~attribute:field_type
      ~value:(String "field");
    make_fact ~entity:field_instance_of ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:field_instance_of ~attribute:field_value_type
      ~value:(Uri type_uri);
    make_fact ~entity:field_instance_of ~attribute:field_cardinality
      ~value:(String "one");
    make_fact ~entity:field_instance_of ~attribute:field_required
      ~value:(Bool true);
    make_fact ~entity:field_instance_of ~attribute:field_doc
      ~value:(String "Links an entity to its kind");
    (* @field:type *)
    make_fact ~entity:field_type ~attribute:field_instance_of
      ~value:(Uri kind_field);
    make_fact ~entity:field_type ~attribute:field_type ~value:(String "field");
    make_fact ~entity:field_type ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:field_type ~attribute:field_value_type
      ~value:(Uri type_string);
    make_fact ~entity:field_type ~attribute:field_cardinality
      ~value:(String "one");
    make_fact ~entity:field_type ~attribute:field_required ~value:(Bool false);
    make_fact ~entity:field_type ~attribute:field_doc
      ~value:(String "The type of schema element (schema/kind/field/type)");
    (* @field:doc *)
    make_fact ~entity:field_doc ~attribute:field_instance_of
      ~value:(Uri kind_field);
    make_fact ~entity:field_doc ~attribute:field_type ~value:(String "field");
    make_fact ~entity:field_doc ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:field_doc ~attribute:field_value_type
      ~value:(Uri type_string);
    make_fact ~entity:field_doc ~attribute:field_cardinality
      ~value:(String "one");
    make_fact ~entity:field_doc ~attribute:field_required ~value:(Bool false);
    make_fact ~entity:field_doc ~attribute:field_doc
      ~value:(String "Documentation string for this entity");
    (* @field:name *)
    make_fact ~entity:field_name ~attribute:field_instance_of
      ~value:(Uri kind_field);
    make_fact ~entity:field_name ~attribute:field_type ~value:(String "field");
    make_fact ~entity:field_name ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:field_name ~attribute:field_value_type
      ~value:(Uri type_string);
    make_fact ~entity:field_name ~attribute:field_cardinality
      ~value:(String "one");
    make_fact ~entity:field_name ~attribute:field_required ~value:(Bool false);
    make_fact ~entity:field_name ~attribute:field_doc
      ~value:(String "Human-readable name");
    (* @field:namespace *)
    make_fact ~entity:field_namespace ~attribute:field_instance_of
      ~value:(Uri kind_field);
    make_fact ~entity:field_namespace ~attribute:field_type
      ~value:(String "field");
    make_fact ~entity:field_namespace ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:field_namespace ~attribute:field_value_type
      ~value:(Uri type_uri);
    make_fact ~entity:field_namespace ~attribute:field_cardinality
      ~value:(String "one");
    make_fact ~entity:field_namespace ~attribute:field_required
      ~value:(Bool false);
    make_fact ~entity:field_namespace ~attribute:field_doc
      ~value:(String "Which schema namespace this belongs to");
    (* @field:used_on *)
    make_fact ~entity:field_used_on ~attribute:field_instance_of
      ~value:(Uri kind_field);
    make_fact ~entity:field_used_on ~attribute:field_type
      ~value:(String "field");
    make_fact ~entity:field_used_on ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:field_used_on ~attribute:field_value_type
      ~value:(Uri type_uri);
    make_fact ~entity:field_used_on ~attribute:field_cardinality
      ~value:(String "many");
    make_fact ~entity:field_used_on ~attribute:field_required
      ~value:(Bool false);
    make_fact ~entity:field_used_on ~attribute:field_doc
      ~value:(String "Which kind(s) this field can be used on");
    (* @field:value_type *)
    make_fact ~entity:field_value_type ~attribute:field_instance_of
      ~value:(Uri kind_field);
    make_fact ~entity:field_value_type ~attribute:field_type
      ~value:(String "field");
    make_fact ~entity:field_value_type ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:field_value_type ~attribute:field_value_type
      ~value:(Uri type_uri);
    make_fact ~entity:field_value_type ~attribute:field_cardinality
      ~value:(String "one");
    make_fact ~entity:field_value_type ~attribute:field_required
      ~value:(Bool false);
    make_fact ~entity:field_value_type ~attribute:field_doc
      ~value:(String "The type of value this field holds");
    (* @field:cardinality *)
    make_fact ~entity:field_cardinality ~attribute:field_instance_of
      ~value:(Uri kind_field);
    make_fact ~entity:field_cardinality ~attribute:field_type
      ~value:(String "field");
    make_fact ~entity:field_cardinality ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:field_cardinality ~attribute:field_value_type
      ~value:(Uri type_string);
    make_fact ~entity:field_cardinality ~attribute:field_cardinality
      ~value:(String "one");
    make_fact ~entity:field_cardinality ~attribute:field_required
      ~value:(Bool false);
    make_fact ~entity:field_cardinality ~attribute:field_doc
      ~value:(String "How many values allowed: 'one' or 'many'");
    (* @field:required *)
    make_fact ~entity:field_required ~attribute:field_instance_of
      ~value:(Uri kind_field);
    make_fact ~entity:field_required ~attribute:field_type
      ~value:(String "field");
    make_fact ~entity:field_required ~attribute:field_namespace
      ~value:(Uri schema_uri);
    make_fact ~entity:field_required ~attribute:field_value_type
      ~value:(Uri type_bool);
    make_fact ~entity:field_required ~attribute:field_cardinality
      ~value:(String "one");
    make_fact ~entity:field_required ~attribute:field_required
      ~value:(Bool false);
    make_fact ~entity:field_required ~attribute:field_doc
      ~value:(String "Is this field required on entities of this kind?");
    (* Types *)
    make_fact ~entity:type_string ~attribute:field_instance_of
      ~value:(Uri kind_type);
    make_fact ~entity:type_string ~attribute:field_type ~value:(String "type");
    make_fact ~entity:type_string ~attribute:field_doc
      ~value:(String "UTF-8 string value");
    make_fact ~entity:type_int ~attribute:field_instance_of
      ~value:(Uri kind_type);
    make_fact ~entity:type_int ~attribute:field_type ~value:(String "type");
    make_fact ~entity:type_int ~attribute:field_doc
      ~value:(String "Integer value");
    make_fact ~entity:type_bool ~attribute:field_instance_of
      ~value:(Uri kind_type);
    make_fact ~entity:type_bool ~attribute:field_type ~value:(String "type");
    make_fact ~entity:type_bool ~attribute:field_doc
      ~value:(String "Boolean value (true/false)");
    make_fact ~entity:type_float ~attribute:field_instance_of
      ~value:(Uri kind_type);
    make_fact ~entity:type_float ~attribute:field_type ~value:(String "type");
    make_fact ~entity:type_float ~attribute:field_doc
      ~value:(String "Floating-point value");
    make_fact ~entity:type_uri ~attribute:field_instance_of
      ~value:(Uri kind_type);
    make_fact ~entity:type_uri ~attribute:field_type ~value:(String "type");
    make_fact ~entity:type_uri ~attribute:field_doc
      ~value:(String "URI reference to another entity");
    make_fact ~entity:type_datetime ~attribute:field_instance_of
      ~value:(Uri kind_type);
    make_fact ~entity:type_datetime ~attribute:field_type ~value:(String "type");
    make_fact ~entity:type_datetime ~attribute:field_doc
      ~value:(String "ISO 8601 datetime value");
  ]
