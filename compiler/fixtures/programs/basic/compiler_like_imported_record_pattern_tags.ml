type typ = TInt | TVariant(String) | TRecord(String)
type field = { name: String, type_: typ }
type imported_record = { module_name: String, record_name: String, fields: List<field> }

fn qualify_type(module_name: String, type_: typ) -> typ {
  match type_ {
    TInt -> TInt,
    TVariant(name) -> TVariant(string_concat(module_name, string_concat(".", name))),
    TRecord(name) -> TRecord(string_concat(module_name, string_concat(".", name)))
  }
}

fn qualify_fields(module_name: String, fields: List<field>) -> List<field> {
  match fields {
    [] -> [],
    [field { name: name, type_: type_ }, ..rest] -> [field { name: name, type_: qualify_type(module_name, type_) }, ..qualify_fields(module_name, rest)]
  }
}

fn runtime_record_tag(record: imported_record) -> String {
  record.record_name
}

fn render_type(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TVariant(name) -> name,
    TRecord(name) -> name
  }
}

fn render_fields(fields: List<field>) -> String {
  match fields {
    [] -> "",
    [field { name: name, type_: type_ }] -> string_concat(name, string_concat(":", render_type(type_))),
    [field { name: name, type_: type_ }, ..rest] -> string_concat(name, string_concat(":", string_concat(render_type(type_), string_concat(",", render_fields(rest)))))
  }
}

fn main() {
  let provider = imported_record { module_name: "Syntax", record_name: "entry", fields: [field { name: "head", type_: TVariant("token") }, field { name: "tail", type_: TVariant("token") }] };
  let qualified = qualify_fields(provider.module_name, provider.fields);
  dbg(string_concat(render_fields(qualified), string_concat(";", runtime_record_tag(provider))))
}
