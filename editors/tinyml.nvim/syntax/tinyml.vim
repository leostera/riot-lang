" TinyML syntax highlighting
" Language: TinyML

if exists("b:current_syntax")
  finish
endif

syn keyword tinymlKeyword let rec fn type use match
syn keyword tinymlBoolean true false

syn match tinymlTypeVar "'[A-Za-z_][A-Za-z0-9_']*"
syn match tinymlConstructor "\<[A-Z][A-Za-z0-9_']*\>"
syn match tinymlType "\<\(unit\|bool\|u8\|u16\|u32\|u64\|i8\|i16\|i32\|i64\|f8\|f16\|f32\|f64\|string\)\>"

syn match tinymlNumber "\<0x[0-9A-Fa-f]\+\>"
syn match tinymlNumber "\<0b[01]\+\>"
syn match tinymlNumber "\<0o[0-7]\+\>"
syn match tinymlNumber "\<[0-9]\+\(\.[0-9]\+\)\?\([eE][+-]\?[0-9]\+\)\?\>"

syn region tinymlString start='"' skip='\\.' end='"'
syn match tinymlComment "//.*$"

syn match tinymlOperator "->\|=>\|[=:+\-*/%|.,;<>]"
syn match tinymlDelimiter "[(){}]"

hi def link tinymlKeyword Keyword
hi def link tinymlBoolean Boolean
hi def link tinymlTypeVar Type
hi def link tinymlConstructor Structure
hi def link tinymlType Type
hi def link tinymlNumber Number
hi def link tinymlString String
hi def link tinymlComment Comment
hi def link tinymlOperator Operator
hi def link tinymlDelimiter Delimiter

let b:current_syntax = "tinyml"
