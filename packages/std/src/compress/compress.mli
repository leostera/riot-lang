(** # Compress

    Streaming compression and decompression APIs.

    The compression namespace currently exposes:

    - [`Gzip`] for reading gzip-compressed data and decompressing it into
      readers, writers, files, or strings

    These APIs are designed to compose with [`IO.Reader`] and [`IO.Writer`]
    instead of forcing a file-only workflow.

    ## Example

    ```ocaml
    open Std

    let () =
      match Compress.Gzip.decompress_file ~src:(Path.v "package.tar.gz") ~dst:(Path.v "package.tar") with
      | Ok () -> Log.info "gzip payload decompressed"
      | Error _ -> Log.error "failed to decompress gzip payload"
    ```
*)
module Gzip = Gzip
