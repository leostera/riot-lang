module Iovec = Iovec
module Buffer = Buffer
module StringView = String_view
module Stdin = Stdio.Stdin
module Stdout = Stdio.Stdout
module Stderr = Stdio.Stderr

let print = Stdout.print

let println = Stdout.println

let eprint = Stderr.print

let eprintln = Stderr.println
