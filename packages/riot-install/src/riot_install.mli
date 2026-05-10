open Std

type destination =
  | Local
  | Global
type external_spec =
  | Source of {
      spec: Riot_deps.Git_dependency.spec;
      update: bool;
    }
  | Registry of Riot_deps.Registry_package_spec.t
type request =
  | Workspace of {
      workspace: Riot_model.Workspace.t;
      package_name: Riot_model.Package_name.t option;
      binary_name: string;
      destination: destination;
    }
  | External of {
      spec: external_spec;
      binary_name: string;
    }
type install_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of {
      package_name: Riot_model.Package_name.t;
      binary_name: string;
    }
  | BuildFailed of Riot_build.error
  | ArtifactNotFound of {
      package_name: Riot_model.Package_name.t;
      binary_name: string;
      reason: string;
    }
  | PromotionFailed of {
      binary_name: string;
      destination: Path.t;
      mode: destination;
      reason: string;
    }
  | ExternalTargetLoadFailed of {
      target: string;
      error: Riot_deps.package_error;
    }

val install_error_message: install_error -> string

val install: ?on_event:(Riot_model.Event.t -> unit) -> request -> (unit, install_error) result
