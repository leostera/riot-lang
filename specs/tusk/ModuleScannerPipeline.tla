---------------------- MODULE ModuleScannerPipeline ----------------------
EXTENDS Naturals, Sequences, TLC

\* Readable PlusCal slice for the scanner + filter path in:
\*
\* - packages/tusk-planner/src/module_scanner.ml
\* - packages/tusk-planner/src/module_graph.ml
\*
\* This model focuses on the current single-directory normalization step:
\*
\* - raw filesystem entries are tagged into scanner entry kinds
\* - entry paths are stored relative to the source directory base
\* - planner filtering keeps allowed typed source files and prunes empty dirs
\* - the final entry sequence is sorted canonically by kind priority, then name
\*
\* The bug config checks one stronger law implied by the current type surface:
\* if `.c` and `.h` are first-class scanner entry kinds, allowed native source
\* files should survive the scan + filter pipeline as dedicated `C` / `H`
\* entries.  The current extracted tagging logic does not do that.

CONSTANTS
  Entries,
  Components,
  Names,
  NameRank,
  EntryKind,
  Exts,
  BasePath,
  AllowedPaths,
  DirVisibleChildren

ASSUME Entries # {}
ASSUME Components # {}
ASSUME Names \in [Entries -> Components]
ASSUME NameRank \in [Components -> Nat]
ASSUME EntryKind \in [Entries -> {"File", "Dir"}]
ASSUME Exts \in [Entries -> {"ml", "mli", "c", "h", "txt", ""}]
ASSUME BasePath \in Seq(Components)
ASSUME AllowedPaths \subseteq Seq(Components)
ASSUME DirVisibleChildren \in [Entries -> BOOLEAN]
ASSUME \A e1, e2 \in Entries :
  (NameRank[Names[e1]] = NameRank[Names[e2]]) => (Names[e1] = Names[e2])

\* Passing smoke model: OCaml files and a visible subdir survive filtering and
\* are emitted in canonical order.  An unrelated text file is dropped.
SmokeEntries ==
  {"Iface", "Impl", "Notes", "UtilDir"}

SmokeComponents ==
  {"src", "a.mli", "b.ml", "notes.txt", "util"}

SmokeNames ==
  [ e \in SmokeEntries |->
      CASE e = "Iface" -> "a.mli"
        [] e = "Impl" -> "b.ml"
        [] e = "Notes" -> "notes.txt"
        [] OTHER -> "util" ]

SmokeNameRank ==
  [ c \in SmokeComponents |->
      CASE c = "a.mli" -> 1
        [] c = "b.ml" -> 2
        [] c = "notes.txt" -> 3
        [] c = "src" -> 4
        [] OTHER -> 5 ]

SmokeEntryKind ==
  [ e \in SmokeEntries |->
      CASE e = "UtilDir" -> "Dir"
        [] OTHER -> "File" ]

SmokeExts ==
  [ e \in SmokeEntries |->
      CASE e = "Iface" -> "mli"
        [] e = "Impl" -> "ml"
        [] e = "Notes" -> "txt"
        [] OTHER -> "" ]

SmokeBasePath ==
  <<"src">>

SmokeAllowedPaths ==
  {<<"src", "a.mli">>, <<"src", "b.ml">>}

SmokeDirVisibleChildren ==
  [ e \in SmokeEntries |->
      CASE e = "UtilDir" -> TRUE
        [] OTHER -> FALSE ]

\* Bug model: `.c` and `.h` are allowed source files, but current extracted
\* tagging still classifies them as `Other`, so filtering drops them.
NativeBugEntries ==
  {"StubC", "HeaderH"}

NativeBugComponents ==
  {"src", "stubs.c", "api.h"}

NativeBugNames ==
  [ e \in NativeBugEntries |->
      CASE e = "StubC" -> "stubs.c"
        [] OTHER -> "api.h" ]

NativeBugNameRank ==
  [ c \in NativeBugComponents |->
      CASE c = "api.h" -> 1
        [] c = "src" -> 2
        [] OTHER -> 3 ]

NativeBugEntryKind ==
  [ e \in NativeBugEntries |-> "File" ]

NativeBugExts ==
  [ e \in NativeBugEntries |->
      CASE e = "StubC" -> "c"
        [] OTHER -> "h" ]

NativeBugBasePath ==
  <<"src">>

NativeBugAllowedPaths ==
  {<<"src", "stubs.c">>, <<"src", "api.h">>}

NativeBugDirVisibleChildren ==
  [ e \in NativeBugEntries |-> FALSE ]

EntryRelPath(e) ==
  Append(BasePath, Names[e])

\* This mirrors the current `scan_directory` implementation exactly:
\* `.ml` and `.mli` get dedicated tags, everything else becomes `Other`.
CurrentScannedTag(e) ==
  IF EntryKind[e] = "Dir"
    THEN "Dir"
    ELSE
      CASE Exts[e] = "mli" -> "MLI"
        [] Exts[e] = "ml" -> "ML"
        [] OTHER -> "Other"

ShouldKeep(e, tag, relPath) ==
  IF EntryKind[e] = "Dir"
    THEN DirVisibleChildren[e]
    ELSE IF tag \in {"ML", "MLI", "C", "H"}
      THEN relPath \in AllowedPaths
      ELSE FALSE

TagPriority(tag) ==
  CASE tag = "MLI" -> 0
    [] tag = "ML" -> 1
    [] tag = "C" -> 2
    [] tag = "H" -> 3
    [] tag = "Other" -> 4
    [] OTHER -> 5

RECURSIVE SortEntries(_, _)

EntryLeq(e1, e2, tagMap) ==
  \/ TagPriority(tagMap[e1]) < TagPriority(tagMap[e2])
  \/ /\ TagPriority(tagMap[e1]) = TagPriority(tagMap[e2])
     /\ NameRank[Names[e1]] <= NameRank[Names[e2]]

MinEntry(es, tagMap) ==
  CHOOSE e \in es : \A other \in es : EntryLeq(e, other, tagMap)

SortEntries(es, tagMap) ==
  IF es = {}
    THEN <<>>
    ELSE
      LET first == MinEntry(es, tagMap) IN
      <<first>> \o SortEntries(es \ {first}, tagMap)

SeqToSet(seq) ==
  { seq[i] : i \in 1..Len(seq) }

IsPrefix(prefix, seq) ==
  /\ Len(prefix) <= Len(seq)
  /\ \A i \in 1..Len(prefix) : prefix[i] = seq[i]

(* --algorithm ModuleScannerPipeline
variables
  pending = Entries,
  scannedTag = [e \in Entries |-> "Pending"],
  relPath = [e \in Entries |-> <<>>],
  kept = {},
  finalOrder = <<>>;

begin
  NextEntry:
    while pending # {} do
      with e \in pending do
        pending := pending \ {e};
        relPath[e] := EntryRelPath(e);
        scannedTag[e] := CurrentScannedTag(e);
        if ShouldKeep(e, CurrentScannedTag(e), EntryRelPath(e)) then
          kept := kept \cup {e};
        end if;
      end with;
    end while;

  Finalize:
    finalOrder := SortEntries(kept, scannedTag);

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "635c9a82" /\ chksum(tla) = "5ee554b3")
VARIABLES pending, scannedTag, relPath, kept, finalOrder, pc

vars == << pending, scannedTag, relPath, kept, finalOrder, pc >>

Init == (* Global variables *)
        /\ pending = Entries
        /\ scannedTag = [e \in Entries |-> "Pending"]
        /\ relPath = [e \in Entries |-> <<>>]
        /\ kept = {}
        /\ finalOrder = <<>>
        /\ pc = "NextEntry"

NextEntry == /\ pc = "NextEntry"
             /\ IF pending # {}
                   THEN /\ \E e \in pending:
                             /\ pending' = pending \ {e}
                             /\ relPath' = [relPath EXCEPT ![e] = EntryRelPath(e)]
                             /\ scannedTag' = [scannedTag EXCEPT ![e] = CurrentScannedTag(e)]
                             /\ IF ShouldKeep(e, CurrentScannedTag(e), EntryRelPath(e))
                                   THEN /\ kept' = (kept \cup {e})
                                   ELSE /\ TRUE
                                        /\ kept' = kept
                        /\ pc' = "NextEntry"
                   ELSE /\ pc' = "Finalize"
                        /\ UNCHANGED << pending, scannedTag, relPath, kept >>
             /\ UNCHANGED finalOrder

Finalize == /\ pc = "Finalize"
            /\ finalOrder' = SortEntries(kept, scannedTag)
            /\ pc' = "Finished"
            /\ UNCHANGED << pending, scannedTag, relPath, kept >>

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << pending, scannedTag, relPath, kept, finalOrder >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == NextEntry \/ Finalize \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 

TypeOK ==
  /\ pending \subseteq Entries
  /\ scannedTag \in [Entries -> {"Pending", "MLI", "ML", "C", "H", "Other", "Dir"}]
  /\ relPath \in [Entries -> Seq(Components)]
  /\ kept \subseteq Entries
  /\ finalOrder \in Seq(Entries)

ScanSettled ==
  pending = {}

ResultsFinalized ==
  pc \in {"Finished", "Done"}

RelativePathsStayUnderBase ==
  \A e \in Entries :
    relPath[e] = <<>> \/ IsPrefix(BasePath, relPath[e])

AllowedOcamlFilesSurviveFiltering ==
  ScanSettled
  =>
  \A e \in Entries :
    EntryKind[e] = "File" /\ Exts[e] \in {"ml", "mli"} /\ relPath[e] \in AllowedPaths
    =>
    e \in kept

OtherFilesAreDropped ==
  ScanSettled
  =>
  \A e \in Entries :
    scannedTag[e] = "Other"
    =>
    e \notin kept

VisibleDirectoriesSurviveFiltering ==
  ScanSettled
  =>
  \A e \in Entries :
    EntryKind[e] = "Dir" /\ DirVisibleChildren[e]
    =>
    e \in kept

FinalOrderMatchesCanonicalSort ==
  ResultsFinalized
  =>
  finalOrder = SortEntries(kept, scannedTag)

FinalOrderContainsExactlyKeptEntries ==
  ResultsFinalized
  =>
  SeqToSet(finalOrder) = kept

NativeExtensionsReceiveDedicatedTags ==
  ScanSettled
  =>
  \A e \in Entries :
    EntryKind[e] = "File" /\ Exts[e] \in {"c", "h"}
    =>
    ((Exts[e] = "c" /\ scannedTag[e] = "C")
      \/ (Exts[e] = "h" /\ scannedTag[e] = "H"))

AllowedNativeFilesSurviveFiltering ==
  ScanSettled
  =>
  \A e \in Entries :
    EntryKind[e] = "File" /\ Exts[e] \in {"c", "h"} /\ relPath[e] \in AllowedPaths
    =>
    e \in kept

=============================================================================
