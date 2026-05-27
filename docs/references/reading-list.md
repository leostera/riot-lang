# Riot / TinyML references

Curated papers/books/docs for building TinyML into Riot ML.

## Already in repo

- Joe Armstrong — *Making Reliable Distributed Systems in the Presence of Software Errors* — `docs/papers/armstrong_thesis_2003.pdf`
- Victor Taelin et al. — *HVM2* — `docs/papers/HVM2.pdf`

## Core ML type inference

- Damas & Milner — *Principal Type-Schemes for Functional Programs*  
  Local: `docs/papers/damas_milner_principal_type_schemes.pdf`  
  Source: https://people.eecs.berkeley.edu/~necula/Papers/DamasMilnerAlgoW.pdf  
  The root paper for HM / Algorithm W.

- François Pottier & Didier Rémy — *The Essence of ML Type Inference*  
  Local: `docs/papers/pottier_remy_essence_ml_type_inference.pdf`  
  Source: https://pauillac.inria.fr/~fpottier/publis/emlti-final.pdf  
  Best “how to actually understand/implement inference” reference.

- François Pottier — *Hindley-Milner Elaboration in Applicative Style*  
  Local: `docs/papers/pottier_hm_elaboration_applicative_style.pdf`  
  Source: https://pauillac.inria.fr/~fpottier/publis/fpottier-elaboration.pdf  
  Useful when moving from inferred surface syntax to typed core IR.

- Mark P. Jones — *Typing Haskell in Haskell*  
  Local: `docs/papers/jones_typing_haskell_in_haskell.pdf`  
  Source: https://web.cecs.pdx.edu/~mpj/thih/thih.pdf  
  Executable typechecker spec; good implementation companion.

## Pattern matching / ML compiler architecture

- Andrew W. Appel — *A SML Compiler* / SML/NJ compiler overview  
  Local: `docs/papers/appel_sml_compiler.pdf`  
  Source: https://www.cs.princeton.edu/~appel/papers/97.pdf  
  Good overview of a real ML compiler pipeline, including match compilation.

## IRs, closures, native compilation

- Simon Peyton Jones — *The Implementation of Functional Programming Languages*  
  Local: `docs/papers/peyton_jones_implementation_functional_programming_languages.pdf`  
  Source: https://simon.peytonjones.org/slpj-book-1987/  
  Classic for graph reduction, lambda lifting, runtime representation.

- Minamide, Morrisett, Harper — *Typed Closure Conversion*  
  http://www.cs.cmu.edu/~rwh/papers/closures/popl96.pdf  
  Important if we keep typed IRs through closure conversion.

- Flanagan et al. — *The Essence of Compiling with Continuations*  
  https://dl.acm.org/doi/10.1145/989393.989443  
  Classic CPS compiler paper.

- Kelsey — *A Correspondence between Continuation Passing Style and Static Single Assignment Form*  
  Source listed, but PDF currently 404: https://cs.purdue.edu/homes/suresh/502-Fall2008/papers/kelsey-ssa-cps.pdf  
  Helps relate functional IRs to LLVM/SSA thinking.

- Fluet et al. — *Compiling with Continuations and LLVM*  
  http://manticore.cs.uchicago.edu/papers/eptcs285-cwc-llvm.pdf  
  Directly relevant for functional languages targeting LLVM.

- Cong et al. — *Compiling with Continuations, or without? Whatever.*  
  https://cs.purdue.edu/homes/rompf/papers/cong-icfp19.pdf  
  Modern view of choosing direct style vs CPS selectively.

## Modules and separate compilation

- Xavier Leroy — *Manifest Types, Modules, and Separate Compilation*  
  https://caml.inria.fr/pub/papers/xleroy-manifest_types-popl94.pdf  
  Key paper for ML-ish modules and compilation boundaries.

- Dreyer et al. — *F-ing Modules*  
  https://people.mpi-sws.org/~dreyer/papers/f-ing/journal.pdf  
  Deeper theory; useful later, not v0 reading.

- Rossberg — *1ML: Core and Modules United*  
  https://people.mpi-sws.org/~rossberg/1ml/1ml-extended.pdf  
  Inspiring later design reference.

## Actors, runtime, selective receive

- Erik Stenman — *The Beam Book*  
  Local: `docs/papers/beam_book_a4.pdf`  
  Source: https://github.com/happi/theBeamBook/releases/latest/download/beam-book-a4.pdf  
  Practical BEAM internals, including receive loops.

- Trinder et al. — *Scaling Reliably: Improving the Scalability of the Erlang Distributed Actor Platform*  
  https://eprints.gla.ac.uk/143232/7/143232.pdf  
  Runtime/scheduler/distribution lessons for Riot actors.

- Erlang/OTP source — `beam_ssa_recv.erl`  
  https://github.com/erlang/otp/blob/367f4a3fabb12cda3f2547e9908acbf28cb34e3a/lib/compiler/src/beam_ssa_recv.erl  
  Concrete selective-receive optimization reference.

- Erlang/OTP source — `msg_instrs.tab`  
  https://github.com/erlang/otp/blob/4d0c23bd19f138e4fcfedd11283636e96d6bbc4f/erts/emulator/beam/msg_instrs.tab  
  Concrete receive-loop VM implementation reference.

## Suggested first reading order

1. Damas & Milner
2. Pottier/Rémy — Essence of ML Type Inference
3. Appel SML compiler overview
4. Typed Closure Conversion
5. Kelsey CPS/SSA
6. Compiling with Continuations and LLVM
7. Leroy Manifest Types
8. The Beam Book receive/runtime chapters
