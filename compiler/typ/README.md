# typ

Incremental type checker engine for Riot.

Primary public typing entrypoint:

```ocaml
Typ.check ~config ~source
```

where `source` is an already prepared `Typ.Model.Source.t` carrying the parse
result and CST.
