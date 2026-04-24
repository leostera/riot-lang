open Std

let dependency_spec_error_message = function
  | Riot_deps.RegistryDependencySpecError error -> Riot_deps.Registry_package_spec.error_message error
  | Riot_deps.SourceDependencySpecError error -> Riot_deps.Git_dependency.message error

let path_dependency_load_error_message = function
  | Riot_deps.PathDependencyManifestReadFailed error -> IO.error_message error
  | Riot_deps.PathDependencyTomlParseFailed error -> Data.Toml.error_to_string error
  | Riot_deps.PathDependencyManifestDecodeFailed error -> Riot_model.Package.manifest_error_message error

let source_dependency_load_error_message = function
  | Riot_deps.SourceDependencyMaterializationFailed error -> Riot_deps.Git_dependency.message error
  | Riot_deps.SourceDependencyManifestReadFailed error -> IO.error_message error
  | Riot_deps.SourceDependencyTomlParseFailed error -> Data.Toml.error_to_string error
  | Riot_deps.SourceDependencyManifestDecodeFailed error -> Riot_model.Package.manifest_error_message
    error

let registry_initialization_error_message = function
  | Riot_deps.RegistryFilesystemInitializationFailed error -> error

let registry_lookup_error_message = function
  | Riot_deps.RegistryPackageDocumentReadFailed error -> error
  | Riot_deps.RegistryPackageNameDecodeFailed error -> Riot_model.Package_name.error_message error

let registry_search_error_message = function
  | Riot_deps.RegistrySearchRequestFailed error -> error

let registry_materialization_error_message = function
  | Riot_deps.RegistryPackageMaterializationFailed error -> Riot_deps.Error.message error
  | Riot_deps.RegistryPackageManifestReadFailed error -> IO.error_message error
  | Riot_deps.RegistryPackageTomlParseFailed error -> Data.Toml.error_to_string error
  | Riot_deps.RegistryPackageManifestDecodeFailed error -> Riot_model.Package.manifest_error_message
    error

let message = function
  | Riot_deps.CurrentPackageNotFound { cwd } ->
      "could not determine current package from '" ^ Path.to_string cwd ^ "'"
  | Riot_deps.PackageNotFound { package } ->
      "workspace package '" ^ Riot_model.Package_name.to_string package ^ "' was not found"
  | Riot_deps.DependencySpecInvalid { dependency; error } ->
      "invalid dependency '" ^ dependency ^ "': " ^ dependency_spec_error_message error
  | Riot_deps.PathDependencyMustBeRelative { dependency } ->
      "path dependency '" ^ dependency ^ "' must be a relative path"
  | Riot_deps.PathDependencyLoadFailed { dependency; path; error } ->
      "failed to load path dependency '"
      ^ dependency
      ^ "' from '"
      ^ Path.to_string path
      ^ "': "
      ^ path_dependency_load_error_message error
  | Riot_deps.SourceDependencyLoadFailed { dependency; source_locator; ref_; error } ->
      let suffix =
        match ref_ with
        | Some ref_ -> "#" ^ ref_
        | None -> ""
      in
      "failed to load source dependency '"
      ^ dependency
      ^ "' from '"
      ^ source_locator
      ^ suffix
      ^ "': "
      ^ source_dependency_load_error_message error
  | Riot_deps.RegistryInitializationFailed { registry; error } ->
      "failed to initialize registry '" ^ registry ^ "': " ^ registry_initialization_error_message error
  | Riot_deps.RegistryLookupFailed { package; registry; error } ->
      "failed to look up package '"
      ^ package
      ^ "' in registry '"
      ^ registry
      ^ "': "
      ^ registry_lookup_error_message error
  | Riot_deps.RegistryMaterializationFailed { package; version; registry; error } ->
      "failed to materialize package '"
      ^ package
      ^ "@"
      ^ version
      ^ "' from registry '"
      ^ registry
      ^ "': "
      ^ registry_materialization_error_message error
  | Riot_deps.RegistrySearchFailed { query; registry; error } ->
      "failed to search registry '"
      ^ registry
      ^ "' for '"
      ^ query
      ^ "': "
      ^ registry_search_error_message error
  | Riot_deps.RegistryPackageNotFound { package; registry; suggestions } ->
      let base = "package '" ^ package ^ "' was not found in registry '" ^ registry ^ "'" in
      (
        match suggestions with
        | [] -> base
        | suggestions ->
            let lines =
              List.map suggestions
                ~fn:(fun { Riot_deps.package; latest_version; description } ->
                  match description with
                  | Some description -> "  - " ^ package ^ "@" ^ latest_version ^ " - " ^ description
                  | None -> "  - " ^ package ^ "@" ^ latest_version)
            in
            base ^ "\nDid you mean:\n" ^ String.concat "\n" lines
      )
  | Riot_deps.RegistryReleaseYanked { package; version; registry } ->
      "package '" ^ package ^ "@" ^ version ^ "' was yanked from registry '" ^ registry ^ "'"
  | Riot_deps.RegistryVersionNotFound { package; requirement; registry } ->
      "package '"
      ^ package
      ^ "' has no release matching '"
      ^ requirement
      ^ "' in registry '"
      ^ registry
      ^ "'"
  | Riot_deps.ManifestUpdateFailed error ->
      Riot_deps.Manifest_edit.error_message error
  | Riot_deps.DependencyNotFoundInSection { path; section; dependency } ->
      "dependency '"
      ^ dependency
      ^ "' was not found in ["
      ^ section
      ^ "] of '"
      ^ Path.to_string path
      ^ "'"
  | Riot_deps.WorkspaceReloadFailed { workspace_root; error } ->
      "failed to reload workspace '"
      ^ Path.to_string workspace_root
      ^ "': "
      ^ Riot_model.Workspace_manager.scan_error_message error
  | Riot_deps.WorkspaceReloadHadErrors { workspace_root; errors } ->
      "workspace '"
      ^ Path.to_string workspace_root
      ^ "' has load errors:\n"
      ^ String.concat "\n" (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string)
  | Riot_deps.MaterializedPackageNotFound { package_root; workspace_root } ->
      "materialized package root '"
      ^ Path.to_string package_root
      ^ "' does not correspond to a package in workspace '"
      ^ Path.to_string workspace_root
      ^ "'"
  | Riot_deps.LockRefreshFailed error ->
      Riot_deps.Error.message error
