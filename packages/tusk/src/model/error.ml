type t =
  | ScanWorkspaceError
  | WorkspaceTomlParseError of string
  | WorkspaceTomlWriteError
