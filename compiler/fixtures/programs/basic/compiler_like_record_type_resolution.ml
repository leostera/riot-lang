type path = { module_name: String, name: String }
type record_info = { module_name: String, name: String, fields: List<String> }
type diagnostic = { message: String }
type lookup = Found(record_info) | Missing

fn same_path(path: path, record: record_info) -> bool {
  path.module_name == record.module_name && path.name == record.name
}

fn find_record(path: path, records: List<record_info>) -> lookup {
  match records {
    [] -> Missing,
    [candidate, ..rest] -> if same_path(path, candidate) { Found(candidate) } else { find_record(path, rest) }
  }
}

fn has_field(name: String, fields: List<String>) -> bool {
  match fields {
    [] -> false,
    [field, ..rest] -> if field == name { true } else { has_field(name, rest) }
  }
}

fn record_name(path: path) -> String {
  string_concat(path.module_name, string_concat(".", path.name))
}

fn check_record_pattern(path: path, field: String, records: List<record_info>) -> diagnostic {
  match find_record(path, records) {
    Found(record) -> if has_field(field, record.fields) { diagnostic { message: "ok" } } else { diagnostic { message: string_concat(record_name(path), string_concat(" missing field ", field)) } },
    Missing -> diagnostic { message: string_concat("unknown record ", record_name(path)) }
  }
}

fn main() {
  let records = [record_info { module_name: "Boxes", name: "box", fields: ["value"] }];
  let missing_record = check_record_pattern(path { module_name: "Boxes", name: "missing" }, "value", records);
  let missing_field = check_record_pattern(path { module_name: "Boxes", name: "box" }, "label", records);
  dbg(string_concat(missing_record.message, string_concat(";", missing_field.message)))
}
