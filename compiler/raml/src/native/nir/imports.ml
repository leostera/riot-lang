type linkage = Types.Import_requirement.linkage =
  | Runtime
  | External

type requirement = Types.Import_requirement.t = {
  symbol: string;
  linkage: linkage;
}

let linkage_to_json = Types.Import_requirement.linkage_to_json

let to_json = Types.Import_requirement.to_json

let make = fun ?(linkage = External) ~symbol () -> Types.Import_requirement.{ symbol; linkage }
