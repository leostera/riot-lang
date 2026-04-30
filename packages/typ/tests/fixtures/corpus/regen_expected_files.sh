#!/bin/bash

for ml in *.ml; do
  ~/.riot/toolchains/5.5.0-riot.4/aarch64-apple-darwin/bin/ocamlc.opt -i $ml > $ml.infer.expected
done
