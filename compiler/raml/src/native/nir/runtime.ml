type t = Types.Runtime_helper.t = {
  name: string;
  symbol: string;
}

let to_json = Types.Runtime_helper.to_json

let make = fun ~name ~symbol -> Types.Runtime_helper.{ name; symbol }
