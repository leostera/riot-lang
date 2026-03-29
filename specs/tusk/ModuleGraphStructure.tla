------------------------ MODULE ModuleGraphStructure ------------------------
EXTENDS FiniteSets, Naturals, Sequences, TLC

\* Readable PlusCal slice for the structural module-graph builder path in:
\*
\* - packages/tusk-planner/src/module_scanner.ml
\* - packages/tusk-planner/src/library_definition.ml
\* - packages/tusk-planner/src/module_graph.ml
\*
\* This model intentionally stops before the `ocamldep` wiring phase.  It only
\* captures the deterministic structure-building rules around:
\*
\* - filtering library child files
\* - file-over-directory precedence
\* - when alias/interface/implementation nodes are created
\* - how concrete vs generated library interfaces choose child dependencies
\*
\* The abstraction is name-based.  We do not model full OCaml filenames or
\* concrete graph node ids here; we model the semantic sets those nodes stand
\* for.

CONSTANTS
  Libraries,
  ModuleNames,
  RawMLChildren,
  RawMLIChildren,
  RawDirChildren,
  BinaryChildren

ASSUME Libraries # {}
ASSUME ModuleNames # {}
ASSUME Libraries \subseteq ModuleNames
ASSUME RawMLChildren \in [Libraries -> SUBSET ModuleNames]
ASSUME RawMLIChildren \in [Libraries -> SUBSET ModuleNames]
ASSUME RawDirChildren \in [Libraries -> SUBSET ModuleNames]
ASSUME BinaryChildren \in [Libraries -> SUBSET ModuleNames]
ASSUME \A lib \in Libraries :
  BinaryChildren[lib] \subseteq (RawMLChildren[lib] \cup RawMLIChildren[lib])

\* Small named model presets so the `.cfg` file can stay simple.
SmokeLibraries ==
  {"Root", "Util", "Shared", "Empty", "Partial", "Nested"}

SmokeModuleNames ==
  {"Root", "Util", "Shared", "Empty", "Partial", "Nested", "Helper", "Leaf",
   "OnlyBin"}

SmokeRawMLChildren ==
  [lib \in SmokeLibraries |->
    CASE lib = "Root" -> {"Root", "Helper", "OnlyBin"}
      [] lib = "Util" -> {"Leaf"}
      [] lib = "Partial" -> {"Partial"}
      [] OTHER -> {}]

SmokeRawMLIChildren ==
  [lib \in SmokeLibraries |->
    CASE lib = "Root" -> {"Root"}
      [] OTHER -> {}]

SmokeRawDirChildren ==
  [lib \in SmokeLibraries |->
    CASE lib = "Root" -> {"Util", "Helper"}
      [] lib = "Util" -> {"Shared"}
      [] lib = "Partial" -> {"Nested"}
      [] OTHER -> {}]

SmokeBinaryChildren ==
  [lib \in SmokeLibraries |->
    CASE lib = "Root" -> {"OnlyBin"}
      [] OTHER -> {}]

FileChildren(lib) ==
  RawMLChildren[lib] \cup RawMLIChildren[lib]

ConcreteML(lib) ==
  lib \in RawMLChildren[lib]

ConcreteMLI(lib) ==
  lib \in RawMLIChildren[lib]

HasConcretePair(lib) ==
  ConcreteML(lib) /\ ConcreteMLI(lib)

\* Matches `Library_definition.from_entries`:
\* - exclude the library interface file itself
\* - exclude declared binary sources
ChildFiles(lib) ==
  (FileChildren(lib) \ {lib}) \ BinaryChildren[lib]

\* Matches `Library_definition.from_entries`:
\* if a same-named file exists, it wins over the directory child.
ChildDirs(lib) ==
  { dir \in RawDirChildren[lib] : dir \notin ChildFiles(lib) }

ChildModules(lib) ==
  ChildFiles(lib) \cup ChildDirs(lib)

HasOcamlContent(lib) ==
  ChildModules(lib) # {} \/ ConcreteML(lib) \/ ConcreteMLI(lib)

\* Matches `Library_definition.deps_for_library_interface`.
ExpectedLibraryDeps(lib) ==
  IF HasConcretePair(lib)
    THEN ChildFiles(lib)
    ELSE ChildModules(lib)

(* --algorithm ModuleGraphStructure
variables
  pending = Libraries,
  built = {},
  buildOrder = <<>>,
  aliasCreated = [lib \in Libraries |-> FALSE],
  intfCreated = [lib \in Libraries |-> FALSE],
  implCreated = [lib \in Libraries |-> FALSE],
  intfChildDeps = [lib \in Libraries |-> {}],
  implChildDeps = [lib \in Libraries |-> {}],
  implDependsOnIntf = [lib \in Libraries |-> FALSE],
  bothDependOnAlias = [lib \in Libraries |-> FALSE];

begin
  Loop:
    while pending # {} do
    ChooseLibrary:
      with lib \in pending do
        pending := pending \ {lib};

        if HasOcamlContent(lib) then
          aliasCreated[lib] := TRUE;
          intfCreated[lib] := TRUE;
          implCreated[lib] := TRUE;
          bothDependOnAlias[lib] := TRUE;
          implDependsOnIntf[lib] := TRUE;
          intfChildDeps[lib] := ExpectedLibraryDeps(lib);
          implChildDeps[lib] := ExpectedLibraryDeps(lib);
        end if;

        built := built \cup {lib};
        buildOrder := Append(buildOrder, lib);
      end with;
  end while;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "ee9e7f68" /\ chksum(tla) = "1db3e457")
VARIABLES pending, built, buildOrder, aliasCreated, intfCreated, implCreated, 
          intfChildDeps, implChildDeps, implDependsOnIntf, bothDependOnAlias, 
          pc

vars == << pending, built, buildOrder, aliasCreated, intfCreated, implCreated, 
           intfChildDeps, implChildDeps, implDependsOnIntf, bothDependOnAlias, 
           pc >>

Init == (* Global variables *)
        /\ pending = Libraries
        /\ built = {}
        /\ buildOrder = <<>>
        /\ aliasCreated = [lib \in Libraries |-> FALSE]
        /\ intfCreated = [lib \in Libraries |-> FALSE]
        /\ implCreated = [lib \in Libraries |-> FALSE]
        /\ intfChildDeps = [lib \in Libraries |-> {}]
        /\ implChildDeps = [lib \in Libraries |-> {}]
        /\ implDependsOnIntf = [lib \in Libraries |-> FALSE]
        /\ bothDependOnAlias = [lib \in Libraries |-> FALSE]
        /\ pc = "Loop"

Loop == /\ pc = "Loop"
        /\ IF pending # {}
              THEN /\ pc' = "ChooseLibrary"
              ELSE /\ pc' = "Finished"
        /\ UNCHANGED << pending, built, buildOrder, aliasCreated, intfCreated, 
                        implCreated, intfChildDeps, implChildDeps, 
                        implDependsOnIntf, bothDependOnAlias >>

ChooseLibrary == /\ pc = "ChooseLibrary"
                 /\ \E lib \in pending:
                      /\ pending' = pending \ {lib}
                      /\ IF HasOcamlContent(lib)
                            THEN /\ aliasCreated' = [aliasCreated EXCEPT ![lib] = TRUE]
                                 /\ intfCreated' = [intfCreated EXCEPT ![lib] = TRUE]
                                 /\ implCreated' = [implCreated EXCEPT ![lib] = TRUE]
                                 /\ bothDependOnAlias' = [bothDependOnAlias EXCEPT ![lib] = TRUE]
                                 /\ implDependsOnIntf' = [implDependsOnIntf EXCEPT ![lib] = TRUE]
                                 /\ intfChildDeps' = [intfChildDeps EXCEPT ![lib] = ExpectedLibraryDeps(lib)]
                                 /\ implChildDeps' = [implChildDeps EXCEPT ![lib] = ExpectedLibraryDeps(lib)]
                            ELSE /\ TRUE
                                 /\ UNCHANGED << aliasCreated, intfCreated, 
                                                 implCreated, intfChildDeps, 
                                                 implChildDeps, 
                                                 implDependsOnIntf, 
                                                 bothDependOnAlias >>
                      /\ built' = (built \cup {lib})
                      /\ buildOrder' = Append(buildOrder, lib)
                 /\ pc' = "Loop"

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << pending, built, buildOrder, aliasCreated, 
                            intfCreated, implCreated, intfChildDeps, 
                            implChildDeps, implDependsOnIntf, 
                            bothDependOnAlias >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == Loop \/ ChooseLibrary \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 

ProcessedLibraries ==
  built

TypeOK ==
  /\ pending \subseteq Libraries
  /\ built \subseteq Libraries
  /\ buildOrder \in Seq(Libraries)
  /\ aliasCreated \in [Libraries -> BOOLEAN]
  /\ intfCreated \in [Libraries -> BOOLEAN]
  /\ implCreated \in [Libraries -> BOOLEAN]
  /\ intfChildDeps \in [Libraries -> SUBSET ModuleNames]
  /\ implChildDeps \in [Libraries -> SUBSET ModuleNames]
  /\ implDependsOnIntf \in [Libraries -> BOOLEAN]
  /\ bothDependOnAlias \in [Libraries -> BOOLEAN]

ProcessedLibrariesRespectCreationRules ==
  \A lib \in ProcessedLibraries :
    IF HasOcamlContent(lib)
      THEN
        /\ aliasCreated[lib]
        /\ intfCreated[lib]
        /\ implCreated[lib]
        /\ implDependsOnIntf[lib]
        /\ bothDependOnAlias[lib]
      ELSE
        /\ ~aliasCreated[lib]
        /\ ~intfCreated[lib]
        /\ ~implCreated[lib]
        /\ ~implDependsOnIntf[lib]
        /\ ~bothDependOnAlias[lib]
        /\ intfChildDeps[lib] = {}
        /\ implChildDeps[lib] = {}

SelfFilesExcludedFromFilteredChildren ==
  \A lib \in Libraries :
    /\ lib \notin ChildFiles(lib)
    /\ lib \notin intfChildDeps[lib]
    /\ lib \notin implChildDeps[lib]

BinaryFilesExcludedFromFilteredChildren ==
  \A lib \in Libraries :
    /\ ChildFiles(lib) \cap BinaryChildren[lib] = {}
    /\ intfChildDeps[lib] \cap BinaryChildren[lib] = {}
    /\ implChildDeps[lib] \cap BinaryChildren[lib] = {}

ShadowedDirectoriesAreFilteredFromChildDirs ==
  \A lib \in Libraries :
    \A dir \in RawDirChildren[lib] \cap ChildFiles(lib) :
      dir \notin ChildDirs(lib)

ConcreteLibrariesOnlyDependOnChildFiles ==
  \A lib \in ProcessedLibraries :
    HasConcretePair(lib) /\ HasOcamlContent(lib)
    =>
    /\ intfChildDeps[lib] = ChildFiles(lib)
    /\ implChildDeps[lib] = ChildFiles(lib)

GeneratedOrPartialLibrariesDependOnAllChildModules ==
  \A lib \in ProcessedLibraries :
    ~HasConcretePair(lib) /\ HasOcamlContent(lib)
    =>
    /\ intfChildDeps[lib] = ChildModules(lib)
    /\ implChildDeps[lib] = ChildModules(lib)

=============================================================================
