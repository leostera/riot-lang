(**
   # Archive

   Streaming archive APIs built on top of Riot's [`IO.Reader`] and
   filesystem abstractions.

   The archive namespace currently exposes:

   - [`Tar`] for reading and extracting tar archives

   ## Example

   ```ocaml
   open Std

   let () =
     match Archive.Tar.extract_file ~archive:(Path.v "package.tar") ~into:(Path.v "./out") with
     | Ok () -> Log.info "archive extracted"
     | Error _ -> Log.error "failed to extract archive"
   ```
*)
module Tar = Tar
