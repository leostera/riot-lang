open Std

type constructor = {
  constructor_id: ConstructorId.t;
  name: string;
  scheme: TypeScheme.t;
}

type label = {
  label_id: LabelId.t;
  name: string;
  field_type: TypeRepr.t;
  mutable_: bool;
}

type variance =
  | Covariant
  | Contravariant
  | Invariant

let flip_variance = function
  | Covariant -> Contravariant
  | Contravariant -> Covariant
  | Invariant -> Invariant

let join_variance = fun left right ->
  match (left, right) with
  | (Invariant, _)
  | (_, Invariant) -> Invariant
  | (Covariant, Covariant) -> Covariant
  | (Contravariant, Contravariant) -> Contravariant
  | (Covariant, Contravariant)
  | (Contravariant, Covariant) -> Invariant

let compose_variance = fun outer inner ->
  match (outer, inner) with
  | (Invariant, _)
  | (_, Invariant) -> Invariant
  | (Covariant, variance) -> variance
  | (Contravariant, Covariant) -> Contravariant
  | (Contravariant, Contravariant) -> Covariant

let variance_to_string = function
  | Covariant -> "covariant"
  | Contravariant -> "contravariant"
  | Invariant -> "invariant"

type poly_variant_bound =
  | Exact
  | UpperBound
  | LowerBound

type poly_variant_tag = {
  name: string;
  payload_type: TypeRepr.t option;
}

type manifest =
  | Alias of TypeRepr.t
  | PolyVariant of {
      bound: poly_variant_bound;
      tags: poly_variant_tag list;
      inherited: TypeRepr.t list
    }

type t = {
  type_constructor_id: TypeConstructorId.t;
  type_name: string;
  param_ids: int list;
  param_variances: variance list;
  constructors: constructor list;
  labels: label list;
  manifest: manifest option;
}

let constructor_entries = fun decl ->
  decl.constructors
  |> List.map (fun (constructor: constructor) -> (constructor.name, constructor.scheme))

let constructor_to_json = fun (constructor: constructor) ->
  Data.Json.Object [
    ("constructor_id", Data.Json.Int (ConstructorId.to_int constructor.constructor_id));
    ("name", Data.Json.String constructor.name);
    ("scheme", Data.Json.String (TypePrinter.scheme_to_string constructor.scheme));
  ]

let label_to_json = fun (label: label) ->
  Data.Json.Object [
    ("label_id", Data.Json.Int (LabelId.to_int label.label_id));
    ("name", Data.Json.String label.name);
    ("field_type", Data.Json.String (TypePrinter.type_to_string label.field_type));
    ("mutable", Data.Json.Bool label.mutable_);
  ]

let poly_variant_bound_to_string = function
  | Exact -> "exact"
  | UpperBound -> "upper"
  | LowerBound -> "lower"

let poly_variant_tag_to_json = fun (tag: poly_variant_tag) ->
  let fields = [ ("name", Data.Json.String tag.name) ] in
  let fields =
    match tag.payload_type with
    | Some payload_type -> fields
    @ [ ("payload_type", Data.Json.String (TypePrinter.type_to_string payload_type)) ]
    | None -> fields
  in
  Data.Json.Object fields

let manifest_to_json = function
  | Alias manifest_type -> Data.Json.Object [
    ("tag", Data.Json.String "alias");
    ("type", Data.Json.String (TypePrinter.type_to_string manifest_type));
  ]
  | PolyVariant { bound; tags; inherited } -> Data.Json.Object [
    ("tag", Data.Json.String "poly_variant");
    ("bound", Data.Json.String (poly_variant_bound_to_string bound));
    ("tags", Data.Json.Array (List.map poly_variant_tag_to_json tags));
    (
      "inherited",
      Data.Json.Array (List.map
        (fun inherited -> Data.Json.String (TypePrinter.type_to_string inherited))
        inherited)
    );
  ]

let to_json = fun decl ->
  let fields = [
    ("type_constructor_id", TypeConstructorId.to_json decl.type_constructor_id);
    ("type_name", Data.Json.String decl.type_name);
    (
      "param_variances",
      Data.Json.Array (List.map
        (fun variance -> Data.Json.String (variance_to_string variance))
        decl.param_variances)
    );
    ("constructors", Data.Json.Array (List.map constructor_to_json decl.constructors));
  ] in
  let fields =
    match decl.labels with
    | [] -> fields
    | labels -> fields @ [ ("labels", Data.Json.Array (List.map label_to_json labels)); ]
  in
  let fields =
    match decl.manifest with
    | Some manifest -> fields @ [ ("manifest", manifest_to_json manifest) ]
    | None -> fields
  in
  Data.Json.Object fields

let poly_variant_tag_to_string = fun (tag: poly_variant_tag) ->
  match tag.payload_type with
  | Some payload_type -> "`" ^ tag.name ^ " of " ^ TypePrinter.type_to_string payload_type
  | None -> "`" ^ tag.name

let manifest_to_string = function
  | Alias manifest_type -> "= " ^ TypePrinter.type_to_string manifest_type
  | PolyVariant { bound; tags; inherited } ->
      let prefix =
        match bound with
        | Exact -> ""
        | UpperBound -> ">"
        | LowerBound -> "<"
      in
      let members = (List.map poly_variant_tag_to_string tags)
      @ (List.map TypePrinter.type_to_string inherited) in
      "= [" ^ prefix ^ " " ^ String.concat " | " members ^ " ]"

let to_string = fun decl ->
  let param_variances =
    match decl.param_variances with
    | [] -> "none"
    | param_variances -> param_variances |> List.map variance_to_string |> String.concat ", "
  in
  let constructors =
    match decl.constructors with
    | [] -> "none"
    | constructors -> constructors
    |> List.map
      (fun (constructor: constructor) ->
        ConstructorId.to_string constructor.constructor_id
        ^ " "
        ^ constructor.name
        ^ " : "
        ^ TypePrinter.scheme_to_string constructor.scheme)
    |> String.concat ", "
  in
  let labels =
    match decl.labels with
    | [] -> "none"
    | labels ->
        labels |> List.map
          (fun (label: label) ->
            let mutability =
              if label.mutable_ then
                "mutable "
              else
                ""
            in
            LabelId.to_string label.label_id
            ^ " "
            ^ label.name
            ^ " : "
            ^ mutability
            ^ TypePrinter.type_to_string label.field_type) |> String.concat ", "
  in
  let manifest =
    match decl.manifest with
    | Some manifest -> "; manifest = " ^ manifest_to_string manifest
    | None -> ""
  in
  TypeConstructorId.to_string decl.type_constructor_id
  ^ " "
  ^ decl.type_name
  ^ " { param_variances = "
  ^ param_variances
  ^ "; constructors = "
  ^ constructors
  ^ "; labels = "
  ^ labels
  ^ manifest
  ^ " }"
