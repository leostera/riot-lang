------------------------- MODULE ActorsCommon -------------------------
EXTENDS Integers, FiniteSets, Sequences, TLC, QueueUtils

\* Shared vocabulary and pure helper operators used by both the integration
\* model and the smaller slice specs. This module intentionally stays tiny:
\* shared state-machine names, message shapes, and collection helpers only.

ProcessStates == {
  "Absent",
  "Uninitialized",
  "Runnable",
  "Running",
  "WaitingMessage",
  "WaitingIO",
  "Exited",
  "Finalized"
}

AliveStates == {
  "Uninitialized",
  "Runnable",
  "Running",
  "WaitingMessage",
  "WaitingIO"
}

BlockedOps == { "None", "Receive", "Syscall" }

TimerModes == { "OneShot", "Interval" }

TimerKinds == { "None", "WakeProcess", "SendMessage" }

ExitSignal(from, reason) == << "EXIT", from, reason >>

DownSignal(target, ref, reason) == << "DOWN", target, ref, reason >>

QueueMembershipCountInQueues(pid, queues, workers) ==
  Cardinality({ w \in workers : Contains(queues[w], pid) })

\* Selective receive scans the save queue before the main mailbox.  A miss
\* moves the scanned mailbox prefix into the new save queue.
SelectiveReceiveScan(saved, inbox, accepted, noMessage) ==
  IF FirstMatchingIndex(saved, accepted) # 0
  THEN
    LET i == FirstMatchingIndex(saved, accepted) IN
    [ matched    |-> TRUE,
      message    |-> saved[i],
      newSave    |-> RemoveIndex(saved, i),
      newMailbox |-> inbox ]
  ELSE
    LET j == FirstMatchingIndex(inbox, accepted) IN
    IF j = 0
    THEN
      [ matched    |-> FALSE,
        message    |-> noMessage,
        newSave    |-> saved \o inbox,
        newMailbox |-> <<>> ]
    ELSE
      [ matched    |-> TRUE,
        message    |-> inbox[j],
        newSave    |-> saved \o Prefix(inbox, j - 1),
        newMailbox |-> Suffix(inbox, j + 1) ]

EnqueueOnOwnerQueue(pid, owners, queues, queuedFlags, workers) ==
  IF queuedFlags[pid]
  THEN
    [ queues      |-> queues,
      queuedFlags |-> queuedFlags ]
  ELSE
    [ queues |->
        [ w \in workers |->
            IF w = owners[pid] THEN Enqueue(queues[w], pid) ELSE queues[w] ],
      queuedFlags |->
        [ queuedFlags EXCEPT ![pid] = TRUE ] ]

RemoveFromAllQueues(pid, queues, workers) ==
  [ w \in workers |->
      IF Contains(queues[w], pid)
      THEN RemoveIndex(queues[w], FirstIndexOf(queues[w], pid))
      ELSE queues[w] ]

RemoveSymmetricLink(linkMap, p, q) ==
  [ x \in DOMAIN linkMap |->
      IF x = p THEN linkMap[x] \ { q }
      ELSE IF x = q THEN linkMap[x] \ { p }
      ELSE linkMap[x] ]

RemoveOutgoingMonitor(monitorMap, observer, ref, target) ==
  [ x \in DOMAIN monitorMap |->
      IF x = observer
      THEN monitorMap[x] \ { << ref, target >> }
      ELSE monitorMap[x] ]

RemoveIncomingMonitor(monitoredByMap, target, observer, ref) ==
  [ x \in DOMAIN monitoredByMap |->
      IF x = target
      THEN monitoredByMap[x] \ { << observer, ref >> }
      ELSE monitoredByMap[x] ]

ClearKey(map, key, emptyValue) == [map EXCEPT ![key] = emptyValue]

=============================================================================
