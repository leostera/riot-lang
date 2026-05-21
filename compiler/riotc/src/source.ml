/// A source path supplied to riotc.
type SourcePath = SourcePath(String)

/// Source contents loaded or embedded for a compiler pipeline run.
type SourceContent = SourceContent(String)

/// One Riot ML source file known to riotc.
type Source = Source(SourcePath, SourceContent)

fn source(path: String, content: String) -> Source {
  Source(SourcePath(path), SourceContent(content))
}

fn path_text(source: Source) -> String {
  match source {
    Source(path, _) ->
      match path {
        SourcePath(text) -> text
      }
  }
}

fn content_text(source: Source) -> String {
  match source {
    Source(_, content) ->
      match content {
        SourceContent(text) -> text
      }
  }
}
