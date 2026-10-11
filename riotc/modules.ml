// Load a deterministic, dependency-first graph of RiotML source modules.
use Prelude.{List, Nil, Cons, Option, Some, None, Ok, Err}
use Syntax.Ast.{
  SourceModule, ModulePath, ModuleName, Import, ImportedName, MemberName,
  ValueDeclaration, ValueDeclarationData, TypeDeclaration,
  TypeDeclarationData, TypeAlias, TypeRecord, TypeVariants,
  RecordTypeField, TupleVariant, TupleVariantData,
  RecordVariant, RecordVariantData, ValueName, TypeName, ConstructorName
}
use Syntax.Parser.SourceModule.{parse_source_module}
use Host.{read_file}
use String.{length, get, slice, equal}

type ResolvedSource = ResolvedSource {
  source_path: string,
  source: string
}

type LoadedModule = LoadedModule {
  module_path: Option<ModulePath>,
  source_path: string,
  source_module: SourceModule
}

type ModuleGraphState = ModuleGraphState {
  completed_paths: List<string>,
  active_paths: List<string>,
  reversed_modules: List<LoadedModule>
}

type VisitModuleRequest = VisitModuleRequest {
  state: ModuleGraphState,
  module_path: Option<ModulePath>,
  source_path: string,
  source: string
}

let lowercase_ascii = fn codepoint ->
  match codepoint {
    | 65 -> "a" | 66 -> "b" | 67 -> "c" | 68 -> "d" | 69 -> "e"
    | 70 -> "f" | 71 -> "g" | 72 -> "h" | 73 -> "i" | 74 -> "j"
    | 75 -> "k" | 76 -> "l" | 77 -> "m" | 78 -> "n" | 79 -> "o"
    | 80 -> "p" | 81 -> "q" | 82 -> "r" | 83 -> "s" | 84 -> "t"
    | 85 -> "u" | 86 -> "v" | 87 -> "w" | 88 -> "x" | 89 -> "y"
    | 90 -> "z"
    | _ -> ""
  }

let snake_component = fn name -> fn offset ->
  match (get name offset) {
    | None -> ""
    | Some(codepoint) -> match (codepoint >= 65) {
      | true -> match (codepoint <= 90) {
        | true -> (match (offset == 0) {
          | true -> lowercase_ascii codepoint
          | false -> "_" + lowercase_ascii codepoint
        }) + snake_component name (offset + 1)
        | false -> slice name offset (offset + 1) + snake_component name (offset + 1)
      }
      | false -> slice name offset (offset + 1) + snake_component name (offset + 1)
    }
  }

let module_path_relative = fn (ModulePath(components)) ->
  module_path_segments components ""

let module_path_segments = fn components -> fn relative_path ->
  match components {
    | Nil -> relative_path + ".ml"
    | Cons(ModuleName(name), Nil) -> relative_path + snake_component name 0 + ".ml"
    | Cons(ModuleName(name), rest) ->
      module_path_segments rest (relative_path + snake_component name 0 + "/")
  }

let module_path_display = fn (ModulePath(components)) ->
  module_path_display_segments components ""

let module_path_display_segments = fn components -> fn display ->
  match components {
    | Nil -> display
    | Cons(ModuleName(name), Nil) -> display + name
    | Cons(ModuleName(name), rest) ->
      module_path_display_segments rest (display + name + ".")
  }

let path_directory = fn path -> fn offset -> fn last_separator ->
  match (offset == length path) {
    | true -> match last_separator {
      | None -> "."
      | Some(separator) -> slice path 0 separator
    }
    | false -> match (get path offset) {
      | Some(47) -> path_directory path (offset + 1) (Some(offset))
      | _ -> path_directory path (offset + 1) last_separator
    }
  }

let directory_of = fn path -> path_directory path 0 None

let join_path = fn directory -> fn relative_path ->
  match (equal directory ".") {
    | true -> relative_path
    | false -> directory + "/" + relative_path
  }

let contains_path = fn source_path -> fn paths ->
  match paths {
    | Nil -> false
    | Cons(path, remaining) -> match (equal source_path path) {
      | true -> true
      | false -> contains_path source_path remaining
    }
  }

let append_paths = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(path, remaining) -> Cons(path, append_paths remaining second)
  }

let try_module_roots = fn roots -> fn relative_path -> fn module_name ->
  match roots {
    | Nil -> Err("module `" + module_name + "` was not found")
    | Cons(root, remaining_roots) -> {
      let candidate = join_path root relative_path;
      match (read_file candidate) {
        | Ok(source) -> Ok(ResolvedSource {
          source_path: candidate,
          source: source
        })
        | Err(_) -> try_module_roots remaining_roots relative_path module_name
      }
    }
  }

let resolve_import = fn importer_path -> fn include_roots -> fn (Import {
  module_path,
  selected_names: _
}) -> {
  let relative_path = module_path_relative module_path;
  let module_name = module_path_display module_path;
  let default_roots = Cons(directory_of importer_path,
    append_paths include_roots (Cons("tinyml/lib", Cons("tinyml/lib/std", Nil))));
  try_module_roots default_roots relative_path module_name
}

let visit_imports = fn state -> fn importer_path -> fn include_roots -> fn imports ->
  match imports {
    | Nil -> Ok(state)
    | Cons(imported_module, remaining_imports) ->
      match (resolve_import importer_path include_roots imported_module) {
        | Err(message) -> Err(message)
        | Ok(ResolvedSource { source_path, source }) ->
          match (visit_module (VisitModuleRequest {
            state: state,
            module_path: Some(import_module_path imported_module),
            source_path: source_path,
            source: source
          }) include_roots) {
            | Err(message) -> Err(message)
            | Ok(next_state) -> visit_imports next_state importer_path include_roots remaining_imports
          }
      }
  }

let import_module_path = fn (Import { module_path, selected_names: _ }) -> module_path

let visit_module = fn (VisitModuleRequest {
  state: ModuleGraphState {
    completed_paths,
    active_paths,
    reversed_modules
  },
  module_path,
  source_path,
  source
}) -> fn include_roots ->
  match (contains_path source_path completed_paths) {
    | true -> Ok(ModuleGraphState {
      completed_paths: completed_paths,
      active_paths: active_paths,
      reversed_modules: reversed_modules
    })
    | false -> match (contains_path source_path active_paths) {
      | true -> Err("module dependency cycle at `" + source_path + "`")
      | false -> match (parse_source_module source) {
        | Err(message) -> Err("cannot parse module `" + source_path + "`: " + message)
        | Ok(source_module) ->
          visit_module_imports source_module source_path include_roots
            (ModuleGraphState {
              completed_paths: completed_paths,
              active_paths: Cons(source_path, active_paths),
              reversed_modules: reversed_modules
            }) module_path
      }
    }
  }

let source_module_imports = fn (SourceModule {
  imports,
  declarations: _
}) -> imports

let append_names = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(name, remaining) -> Cons(name, append_names remaining second)
  }

let record_type_field_names = fn fields ->
  match fields {
    | Nil -> Nil
    | Cons(RecordTypeField { name: ValueName(name), field_type: _ }, remaining) ->
      Cons(name, record_type_field_names remaining)
  }

let variant_export_names = fn variants ->
  match variants {
    | Nil -> Nil
    | Cons(TupleVariant(TupleVariantData {
        constructor_name: ConstructorName(name),
        fields: _
      }), remaining) -> Cons(name, variant_export_names remaining)
    | Cons(RecordVariant(RecordVariantData {
        constructor_name: ConstructorName(name),
        fields
      }), remaining) -> append_names (Cons(name, record_type_field_names fields))
        (variant_export_names remaining)
  }

let declaration_export_names = fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(ValueDeclaration(ValueDeclarationData { name: ValueName(name), body: _ }), remaining) ->
      Cons(name, declaration_export_names remaining)
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: TypeName(name),
        parameters: _,
        definition: TypeAlias(_)
      }), remaining) -> Cons(name, declaration_export_names remaining)
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: TypeName(name),
        parameters: _,
        definition: TypeRecord(fields)
      }), remaining) -> append_names (Cons(name, record_type_field_names fields))
        (Cons(name, declaration_export_names remaining))
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: TypeName(name),
        parameters: _,
        definition: TypeVariants(variants)
      }), remaining) -> append_names (Cons(name, variant_export_names variants))
        (Cons(name, declaration_export_names remaining))
  }

let contains_export_name = fn wanted -> fn names ->
  match names {
    | Nil -> false
    | Cons(name, remaining) -> match (equal wanted name) {
      | true -> true
      | false -> contains_export_name wanted remaining
    }
  }

let find_imported_module = fn wanted_path -> fn modules ->
  match modules {
    | Nil -> None
    | Cons(LoadedModule {
        module_path: Some(module_path),
        source_path,
        source_module
      }, remaining) -> match (equal
        (module_path_display module_path) (module_path_display wanted_path)) {
      | true -> Some(LoadedModule {
        module_path: Some(module_path),
        source_path: source_path,
        source_module: source_module
      })
      | false -> find_imported_module wanted_path remaining
    }
    | Cons(LoadedModule { module_path: None, source_path: _, source_module: _ }, remaining) ->
      find_imported_module wanted_path remaining
  }

let validate_selected_members = fn module_name -> fn exported_names -> fn selected_names ->
  match selected_names {
    | Nil -> Ok(())
    | Cons(ImportedName(MemberName(member_name)), remaining) ->
      match (contains_export_name member_name exported_names) {
        | true -> validate_selected_members module_name exported_names remaining
        | false -> Err("module `" + module_name + "` does not export `" + member_name + "`")
      }
  }

let validate_one_import = fn modules -> fn (Import {
  module_path,
  selected_names
}) -> {
  let module_name = module_path_display module_path;
  match (find_imported_module module_path modules) {
    | None -> Err("module `" + module_name + "` is not in the loaded source graph")
    | Some(LoadedModule { module_path: _, source_path: _, source_module }) ->
      validate_selected_members module_name
        (declaration_export_names (source_module_declarations source_module)) selected_names
  }
}

let source_module_declarations = fn (SourceModule { imports: _, declarations }) -> declarations

let validate_import_list = fn modules -> fn imports ->
  match imports {
    | Nil -> Ok(())
    | Cons(import, remaining) -> match (validate_one_import modules import) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_import_list modules remaining
    }
  }

let validate_module_imports = fn modules -> fn loaded_modules ->
  match loaded_modules {
    | Nil -> Ok(())
    | Cons(LoadedModule { module_path: _, source_path: _, source_module }, remaining) ->
      match (validate_import_list modules (source_module_imports source_module)) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_module_imports modules remaining
      }
  }

let validate_import_members = fn modules -> validate_module_imports modules modules

let visit_module_imports = fn source_module -> fn source_path -> fn include_roots -> fn (ModuleGraphState {
  completed_paths: completed_paths_before,
  active_paths,
  reversed_modules: reversed_modules_before
}) -> fn module_path ->
  match (visit_imports (ModuleGraphState {
    completed_paths: completed_paths_before,
    active_paths: active_paths,
    reversed_modules: reversed_modules_before
  }) source_path include_roots (source_module_imports source_module)) {
    | Err(message) -> Err(message)
    | Ok(ModuleGraphState {
        completed_paths,
        active_paths: active_after_imports,
        reversed_modules
      }) -> Ok(ModuleGraphState {
      completed_paths: Cons(source_path, completed_paths),
      active_paths: remove_path source_path active_after_imports,
      reversed_modules: Cons(LoadedModule {
        module_path: module_path,
        source_path: source_path,
        source_module: source_module
      }, reversed_modules)
    })
  }

let remove_path = fn removed_path -> fn paths ->
  match paths {
    | Nil -> Nil
    | Cons(path, remaining) -> match (equal removed_path path) {
      | true -> remaining
      | false -> Cons(path, remove_path removed_path remaining)
    }
  }

let load_source_graph = fn source_path -> fn include_roots -> {
  let root_source = match (read_file source_path) {
    | Err(message) -> Err("cannot read source `" + source_path + "`: " + message)
    | Ok(source) -> Ok(source)
  };
  match root_source {
    | Err(message) -> Err(message)
    | Ok(source) -> match (visit_module (VisitModuleRequest {
      state: ModuleGraphState {
        completed_paths: Nil,
        active_paths: Nil,
        reversed_modules: Nil
      },
      module_path: None,
      source_path: source_path,
      source: source
    }) include_roots) {
      | Err(message) -> Err(message)
      | Ok(ModuleGraphState {
        completed_paths: _,
        active_paths: _,
        reversed_modules
      }) -> {
        let modules = reverse_modules reversed_modules Nil;
        match (validate_import_members modules) {
          | Err(message) -> Err(message)
          | Ok(_) -> Ok(modules)
        }
      }
    }
  }
}

let reverse_modules = fn modules -> fn reversed ->
  match modules {
    | Nil -> reversed
    | Cons(module, remaining) -> reverse_modules remaining (Cons(module, reversed))
  }

let module_source_pairs = fn modules ->
  match modules {
    | Nil -> Nil
    | Cons(LoadedModule {
        module_path: _,
        source_path,
        source_module
      }, remaining) -> Cons((source_path, source_module), module_source_pairs remaining)
  }

let load_source_units = fn source_path -> fn include_roots ->
  match (load_source_graph source_path include_roots) {
    | Err(message) -> Err(message)
    | Ok(modules) -> Ok(module_source_pairs modules)
  }
