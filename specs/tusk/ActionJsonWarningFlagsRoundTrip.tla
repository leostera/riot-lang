---------------- MODULE ActionJsonWarningFlagsRoundTrip ----------------
EXTENDS Naturals, Sequences, TLC

\* Readable PlusCal slice for the warning-flag part of:
\*
\* - packages/tusk-toolchain/src/ocamlc.ml (`flags_to_string`)
\* - packages/tusk-planner/src/action.ml (`Action.to_json` / `Action.from_json`)
\*
\* This slice models only `Ocamlc.Warning [...]` flags inside an action JSON
\* round-trip. Other action fields are intentionally omitted because the bug
\* here is entirely in how the warning payload is encoded and then parsed back.
\*
\* Abstraction boundary:
\* - The concrete JSON stores `["-w", "-a-49"]`
\* - The model stores the `-w` payload as a sequence of warning codes
\*   `<<"a", "49">>` instead of a concatenated string
\* - That keeps the spec readable while preserving the exact lossy distinction:
\*   the serializer can emit multiple warning codes, but the current parser only
\*   recognizes the one-code payloads `<<"a">>` and `<<"49">>`

CONSTANT OriginalWarnings

WarningUniverse ==
  {"All", "NoCmiFile"}

WarningCodeUniverse ==
  {"a", "49"}

ASSUME OriginalWarnings \in Seq(WarningUniverse)

\* Passing smoke model: one warning survives the round-trip.
SmokeOriginalWarnings ==
  <<"All">>

\* Bug model: multiple warnings serialize fine, but the current parser does not
\* reconstruct the combined payload.
WarningCombinationBugOriginalWarnings ==
  <<"All", "NoCmiFile">>

WarningCode(w) ==
  CASE w = "All" -> "a"
    [] OTHER -> "49"

EncodeWarningPayload(ws) ==
  [i \in 1..Len(ws) |-> WarningCode(ws[i])]

(* --algorithm ActionJsonWarningFlagsRoundTrip
variables
  serialized_flag_name = "",
  serialized_warning_payload = <<>>,
  restored_warnings = <<>>;

begin
  SerializeWarningFlag:
    \* Mirrors `flags_to_string`: all warning codes are packed into one `-w`
    \* payload.
    serialized_flag_name := "-w";
    serialized_warning_payload := EncodeWarningPayload(OriginalWarnings);

  ParseWarningFlag:
    \* Mirrors the warning subset of `Action.from_json`:
    \* - `<<"a">>` becomes `Warning [All]`
    \* - `<<"49">>` becomes `Warning [NoCmiFile]`
    \* - every other warning payload becomes `Warning []`
    if serialized_flag_name = "-w" then
      if serialized_warning_payload = <<"a">> then
        restored_warnings := <<"All">>;
      else
        if serialized_warning_payload = <<"49">> then
          restored_warnings := <<"NoCmiFile">>;
        else
          restored_warnings := <<>>;
        end if;
      end if;
    else
      restored_warnings := <<>>;
    end if;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "d47e873e" /\ chksum(tla) = "5397126b")
VARIABLES serialized_flag_name, serialized_warning_payload, restored_warnings, 
          pc

vars == << serialized_flag_name, serialized_warning_payload, 
           restored_warnings, pc >>

Init == (* Global variables *)
        /\ serialized_flag_name = ""
        /\ serialized_warning_payload = <<>>
        /\ restored_warnings = <<>>
        /\ pc = "SerializeWarningFlag"

SerializeWarningFlag == /\ pc = "SerializeWarningFlag"
                        /\ serialized_flag_name' = "-w"
                        /\ serialized_warning_payload' = EncodeWarningPayload(OriginalWarnings)
                        /\ pc' = "ParseWarningFlag"
                        /\ UNCHANGED restored_warnings

ParseWarningFlag == /\ pc = "ParseWarningFlag"
                    /\ IF serialized_flag_name = "-w"
                          THEN /\ IF serialized_warning_payload = <<"a">>
                                     THEN /\ restored_warnings' = <<"All">>
                                     ELSE /\ IF serialized_warning_payload = <<"49">>
                                                THEN /\ restored_warnings' = <<"NoCmiFile">>
                                                ELSE /\ restored_warnings' = <<>>
                          ELSE /\ restored_warnings' = <<>>
                    /\ pc' = "Finished"
                    /\ UNCHANGED << serialized_flag_name, 
                                    serialized_warning_payload >>

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << serialized_flag_name, serialized_warning_payload, 
                            restored_warnings >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == SerializeWarningFlag \/ ParseWarningFlag \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION

TypeOK ==
  /\ serialized_flag_name \in {"", "-w"}
  /\ serialized_warning_payload \in Seq(WarningCodeUniverse)
  /\ restored_warnings \in Seq(WarningUniverse)

Settled ==
  pc = "Done"

SingleWarningRoundTripPreserved ==
  Settled
  /\ Len(OriginalWarnings) <= 1
  =>
  restored_warnings = OriginalWarnings

WarningRoundTripPreserved ==
  Settled
  =>
  restored_warnings = OriginalWarnings

=============================================================================
