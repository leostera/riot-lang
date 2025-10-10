openStd
openPoneglyph
openSchema
letns=(** OCaml language schema *)namespace"ocaml"
letmodule_=(** {1 Kinds} *)kind~ns"module"|>doc"An OCaml module"
letpackage=(** {1 Fields} *)kind~ns"package"|>doc"An OCaml package"
letbelongs_to_package=field~ns"belongs_to_package"|>used_onmodule_|>valueType.uri|>doc"The package this module belongs to"
letdepends_on_package=field~ns"depends_on_package"|>used_onmodule_|>valueType.uri|>doc"A package this module depends on"
letdepends_on_modules=(** {1 Schema Registration} *)field~ns"depends_on_modules"|>used_onmodule_|>value Type.listType.uri |>doc"Modules this module depends on"
letall_defs= module_;package;belongs_to_package;depends_on_package;depends_on_modules 
letregisterstore=store->Schema.registerstoreall_defs
letbelongs_to_package(** {1 Fact Builders} *)~package=(** {1 Fact Builders} *)~package->uri_value~field:belongs_to_package~value:package
letdepends_on_package~package=~package->uri_value~field:depends_on_package~value:package
letdepends_on_modules~modules=~modules->uri_list_value~field:depends_on_modules~values:modules
