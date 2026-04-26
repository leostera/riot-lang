let lower_source_file = fun ~source:_ (parse_result: Syn.Parser.parse_result) ->
  Semantic_tree.empty ~kind:parse_result.kind
