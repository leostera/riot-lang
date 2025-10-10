openStd
openPoneglyph
openSchema
letns=(** Tusk build system schema *)namespace"tusk"
letfile=(** {1 Kinds} *)kind~ns"file"|>doc"A File in the Tusk schema"
letpackage=(** {1 Fields} *)kind~ns"package"|>doc"A package in the workspace"
letcontent_hash=field~ns"content_hash"|>used_onfile|>valueType.string|>doc"The content hash of a file"
letsize_bytes=field~ns"size_bytes"|>used_onfile|>valueType.int|>doc"The size of a file in bytes"
letpath=(** {1 Schema Registration} *)field~ns"path"|>used_onfile|>valueType.string|>doc"The file system path"
letall_defs= file;package;content_hash;size_bytes;path 
letregisterstore=store->Schema.registerstoreall_defs
letcontent_hash(** {1 Fact Builders} *)~hash=(** {1 Fact Builders} *)~hash->string_value~field:content_hash~value:hash
letsize_bytes~bytes=~bytes->int_value~field:size_bytes~value:bytes
letpath~path=~path->string_value~field:path~value:path
