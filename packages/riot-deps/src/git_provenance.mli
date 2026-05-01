open Std

type t = {
  locator: string;
  selector: string;
  repository_root: Path.t;
  origin_url: string;
  package_subdir: Path.t option;
}
type error =
  | NotGitRepository of {
      path: Path.t;
    }
  | MissingOriginRemote of {
      path: Path.t;
    }
  | InvalidRepositoryRoot of {
      path: string;
      error: Path.error;
    }
  | PackageOutsideRepository of {
      package_root: Path.t;
      repository_root: Path.t;
    }
  | UnsupportedRemoteUrl of { url: string }
  | GitCommandFailed of { command: string; status: int; stdout: string; stderr: string }
  | GitCommandSpawnFailed of {
      command: string;
      error: Command.error;
    }

val message: error -> string

val discover: package_root:Path.t -> (t, error) result
