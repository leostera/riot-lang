type from_base = Base.token

let keep = Base.keep

module Imported = Base.Inner

let value : from_base = keep Imported.value
