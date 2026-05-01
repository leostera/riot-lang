#!/bin/bash

for ml in *.ml *.mli; do
  echo "Regenerating $ml"
  ~/.riot/toolchains/5.5.0-riot.4/aarch64-apple-darwin/bin/ocamlc.opt -i $ml 2> $ml.infer.expected
done
