# pkgs-ml

Client-side helpers for talking to the `pkgs.ml` registry.

`pkgs-ml` is not a general application package. It is the reusable library
behind Riot's package-management tooling: sparse-index reads, registry cache
layout, artifact download/materialization, and artifact publishing.

If you are building tooling that needs to consume or publish packages without
reimplementing the registry protocol, start here.

## Install

```sh
riot add pkgs-ml
```

## What it gives you

- `Pkgs_ml.Registry_cache` to compute the on-disk cache layout under
  `~/.riot/registry/<registry>/...`;
- `Pkgs_ml.Sparse_index` to read sparse-index config and package documents;
- `Pkgs_ml.Registry` to fetch, cache, materialize, search, and publish
  releases.

## Example

```ocaml
open Std

let cache =
  Pkgs_ml.Registry_cache.create
    ~registry_name:"pkgs.ml"
    ~riot_home:(Path.v "/tmp/demo-riot-home")
    ()
  |> Result.expect ~msg:"cache should be creatable"

let index_dir = Pkgs_ml.Registry_cache.index_dir cache
let archive = Pkgs_ml.Registry_cache.archive_path cache ~package_name:"std" ~version:"0.1.0"
```

A runnable example is included:

```sh
riot run -p pkgs-ml cache_layout
```

That example prints the cache layout and the derived sparse-index URL for a
package document.

## When to use it

Use `pkgs-ml` when you need:

- local cache layout helpers;
- sparse index parsing and URL derivation;
- release materialization into `src/<pkg>/<version>`;
- publishing via `POST /v1/publish`.

If you are building Riot's dependency solver or lock refresh machinery, this is
the right level. If you are writing a general application, it is probably not.

## Related packages

- `riot-deps` builds dependency solving and lock refresh on top of this package.
- `riot-publish` uses the same registry surface when uploading artifacts.
