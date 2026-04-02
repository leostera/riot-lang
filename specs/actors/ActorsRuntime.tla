------------------------- MODULE ActorsRuntime -------------------------
EXTENDS Integers, FiniteSets, Sequences, TLC, QueueUtils, ActorsCommon

\* This is a bounded, readable design model for the current `packages/actors`
\* runtime.  It is intentionally more semantic than the OCaml implementation:
\*
\* - it models process states, worker queues, links, monitors, selective
\*   receive, blocking syscalls, and timers directly;
\* - it does not model OCaml continuations, backtraces, or the low-level async
\*   poller;
\* - it keeps queue and mailbox sizes bounded so TLC can explore the model.
\*
\* The point is to state the runtime contract clearly enough that we can ask:
\* "what must always be true?" and "where does the implementation diverge?".

CONSTANTS
  Processes,
  Workers,
  UserMessages,
  TimerIds,
  MonitorRefs,
  MainProcess,
  MainWorker,
  SelectorUniverse,
  ExitReasons,
  NoProcess,
  NoMessage,
  NoTimer,
  NoSelector,
  NoReason,
  ExitOk,
  MaxMailboxLen,
  MaxRunQueueLen

ExitValues == ExitReasons \cup { ExitOk }

SystemExitMessages ==
  { ExitSignal(from, reason) : from \in Processes, reason \in ExitValues }

SystemDownMessages ==
  { DownSignal(target, ref, reason) :
      target \in Processes,
      ref \in MonitorRefs,
      reason \in ExitValues }

AllMessages == UserMessages \cup SystemExitMessages \cup SystemDownMessages

ASSUME MainProcess \in Processes
ASSUME MainWorker \in Workers
ASSUME Workers # {}
ASSUME Processes # {}
ASSUME SelectorUniverse # {}
ASSUME \A selector \in SelectorUniverse : selector \subseteq AllMessages
ASSUME NoProcess \notin Processes
ASSUME NoMessage \notin UserMessages
ASSUME NoTimer \notin TimerIds
ASSUME NoSelector \notin SelectorUniverse
ASSUME NoReason \notin ExitReasons
ASSUME ExitOk \notin ExitReasons
ASSUME MaxMailboxLen \in Nat
ASSUME MaxRunQueueLen \in Nat

VARIABLES
  procState,
  exitReason,
  blockedOp,
  waitingFilter,
  ownerWorker,
  workerQueue,
  slotQueued,
  slotExecuting,
  slotPending,
  mailbox,
  saveQueue,
  trapExit,
  links,
  monitors,
  monitoredBy,
  ioRegistered,
  ioReady,
  receiveTimerId,
  receiveTimeoutFired,
  syscallTimerId,
  syscallTimeoutFired,
  timerActive,
  timerMode,
  timerKind,
  timerWakeTarget,
  timerSendTarget,
  timerMessage

vars ==
  << procState,
     exitReason,
     blockedOp,
     waitingFilter,
     ownerWorker,
     workerQueue,
     slotQueued,
     slotExecuting,
     slotPending,
     mailbox,
     saveQueue,
     trapExit,
     links,
     monitors,
     monitoredBy,
     ioRegistered,
     ioReady,
     receiveTimerId,
     receiveTimeoutFired,
     syscallTimerId,
     syscallTimeoutFired,
     timerActive,
     timerMode,
     timerKind,
     timerWakeTarget,
     timerSendTarget,
     timerMessage >>

IsAlive(p) == procState[p] \in AliveStates

HasMessages(p) == Len(mailbox[p]) + Len(saveQueue[p]) > 0

CanAppendMailbox(p) == Len(mailbox[p]) < MaxMailboxLen

CanAppendRunQueue(w) == Len(workerQueue[w]) < MaxRunQueueLen

QueueMembershipCount(p) ==
  QueueMembershipCountInQueues(p, workerQueue, Workers)

ReceiveScan(saved, inbox, accepted) ==
  SelectiveReceiveScan(saved, inbox, accepted, NoMessage)

EnqueueOnOwner(pid, owners, queues, queuedFlags) ==
  EnqueueOnOwnerQueue(pid, owners, queues, queuedFlags, Workers)

RemoveFromAllWorkerQueues(pid, queues) ==
  RemoveFromAllQueues(pid, queues, Workers)

ClearReceiveTimerIds(map, pid) == ClearKey(map, pid, NoTimer)

ClearSyscallTimerIds(map, pid) == ClearKey(map, pid, NoTimer)

TypeOK ==
  /\ procState \in [Processes -> ProcessStates]
  /\ exitReason \in [Processes -> (ExitValues \cup { NoReason })]
  /\ blockedOp \in [Processes -> BlockedOps]
  /\ waitingFilter \in [Processes -> (SelectorUniverse \cup { NoSelector })]
  /\ ownerWorker \in [Processes -> Workers]
  /\ workerQueue \in [Workers -> Seq(Processes)]
  /\ slotQueued \in [Processes -> BOOLEAN]
  /\ slotExecuting \in [Processes -> BOOLEAN]
  /\ slotPending \in [Processes -> BOOLEAN]
  /\ mailbox \in [Processes -> Seq(AllMessages)]
  /\ saveQueue \in [Processes -> Seq(AllMessages)]
  /\ trapExit \in [Processes -> BOOLEAN]
  /\ links \in [Processes -> SUBSET Processes]
  /\ monitors \in [Processes -> SUBSET (MonitorRefs \X Processes)]
  /\ monitoredBy \in [Processes -> SUBSET (Processes \X MonitorRefs)]
  /\ ioRegistered \in [Processes -> BOOLEAN]
  /\ ioReady \in [Processes -> BOOLEAN]
  /\ receiveTimerId \in [Processes -> (TimerIds \cup { NoTimer })]
  /\ receiveTimeoutFired \in [Processes -> BOOLEAN]
  /\ syscallTimerId \in [Processes -> (TimerIds \cup { NoTimer })]
  /\ syscallTimeoutFired \in [Processes -> BOOLEAN]
  /\ timerActive \in [TimerIds -> BOOLEAN]
  /\ timerMode \in [TimerIds -> TimerModes]
  /\ timerKind \in [TimerIds -> TimerKinds]
  /\ timerWakeTarget \in [TimerIds -> (Processes \cup { NoProcess })]
  /\ timerSendTarget \in [TimerIds -> (Processes \cup { NoProcess })]
  /\ timerMessage \in [TimerIds -> (AllMessages \cup { NoMessage })]

QueueFlagsAgree ==
  \A p \in Processes : slotQueued[p] <=> QueueMembershipCount(p) = 1

\* Bounded smoke-run cutoff used only by the tiny integration config.
SmokeDepthBound == TLCGet("level") < 5

RunningSlotsAreExclusive ==
  \A p \in Processes : slotExecuting[p] => procState[p] = "Running"

WorkerQueuesContainNoDuplicates ==
  \A w \in Workers : UniqueElements(workerQueue[w])

LinksAreSymmetric ==
  \A p \in Processes :
    \A q \in links[p] : p \in links[q]

MonitorsAreDual ==
  \A observer \in Processes :
    \A ref \in MonitorRefs :
      \A target \in Processes :
        (<< ref, target >> \in monitors[observer])
        <=>
        (<< observer, ref >> \in monitoredBy[target])

DeadProcessesAreDetached ==
  \A p \in Processes :
    procState[p] \in { "Absent", "Finalized" }
    =>
    /\ links[p] = {}
    /\ monitors[p] = {}
    /\ monitoredBy[p] = {}
    /\ receiveTimerId[p] = NoTimer
    /\ syscallTimerId[p] = NoTimer
    /\ ~slotQueued[p]
    /\ ~slotExecuting[p]
    /\ ~slotPending[p]
    /\ ~ioRegistered[p]

ConsistencyOK ==
  /\ QueueFlagsAgree
  /\ RunningSlotsAreExclusive
  /\ WorkerQueuesContainNoDuplicates
  /\ LinksAreSymmetric
  /\ MonitorsAreDual
  /\ DeadProcessesAreDetached

FreshTimer(tid) ==
  /\ ~timerActive[tid]
  /\ timerKind[tid] = "None"

FreshMonitorRef(ref) ==
  \A p \in Processes : \A q \in Processes : << ref, q >> \notin monitors[p]

Init ==
  /\ procState =
      [ p \in Processes |->
          IF p = MainProcess THEN "Uninitialized" ELSE "Absent" ]
  /\ exitReason = [ p \in Processes |-> NoReason ]
  /\ blockedOp = [ p \in Processes |-> "None" ]
  /\ waitingFilter = [ p \in Processes |-> NoSelector ]
  /\ ownerWorker = [ p \in Processes |-> MainWorker ]
  /\ workerQueue =
      [ w \in Workers |->
          IF w = MainWorker THEN << MainProcess >> ELSE <<>> ]
  /\ slotQueued = [ p \in Processes |-> p = MainProcess ]
  /\ slotExecuting = [ p \in Processes |-> FALSE ]
  /\ slotPending = [ p \in Processes |-> FALSE ]
  /\ mailbox = [ p \in Processes |-> <<>> ]
  /\ saveQueue = [ p \in Processes |-> <<>> ]
  /\ trapExit = [ p \in Processes |-> FALSE ]
  /\ links = [ p \in Processes |-> {} ]
  /\ monitors = [ p \in Processes |-> {} ]
  /\ monitoredBy = [ p \in Processes |-> {} ]
  /\ ioRegistered = [ p \in Processes |-> FALSE ]
  /\ ioReady = [ p \in Processes |-> FALSE ]
  /\ receiveTimerId = [ p \in Processes |-> NoTimer ]
  /\ receiveTimeoutFired = [ p \in Processes |-> FALSE ]
  /\ syscallTimerId = [ p \in Processes |-> NoTimer ]
  /\ syscallTimeoutFired = [ p \in Processes |-> FALSE ]
  /\ timerActive = [ tid \in TimerIds |-> FALSE ]
  /\ timerMode = [ tid \in TimerIds |-> "OneShot" ]
  /\ timerKind = [ tid \in TimerIds |-> "None" ]
  /\ timerWakeTarget = [ tid \in TimerIds |-> NoProcess ]
  /\ timerSendTarget = [ tid \in TimerIds |-> NoProcess ]
  /\ timerMessage = [ tid \in TimerIds |-> NoMessage ]

Spawn(newPid, worker) ==
  /\ newPid \in Processes
  /\ worker \in Workers
  /\ procState[newPid] = "Absent"
  /\ CanAppendRunQueue(worker)
  /\ procState' = [procState EXCEPT ![newPid] = "Uninitialized"]
  /\ exitReason' = [exitReason EXCEPT ![newPid] = NoReason]
  /\ blockedOp' = [blockedOp EXCEPT ![newPid] = "None"]
  /\ waitingFilter' = [waitingFilter EXCEPT ![newPid] = NoSelector]
  /\ ownerWorker' = [ownerWorker EXCEPT ![newPid] = worker]
  /\ workerQueue' = [workerQueue EXCEPT ![worker] = Enqueue(@, newPid)]
  /\ slotQueued' = [slotQueued EXCEPT ![newPid] = TRUE]
  /\ slotExecuting' = [slotExecuting EXCEPT ![newPid] = FALSE]
  /\ slotPending' = [slotPending EXCEPT ![newPid] = FALSE]
  /\ mailbox' = [mailbox EXCEPT ![newPid] = <<>>]
  /\ saveQueue' = [saveQueue EXCEPT ![newPid] = <<>>]
  /\ trapExit' = [trapExit EXCEPT ![newPid] = FALSE]
  /\ links' = [links EXCEPT ![newPid] = {}]
  /\ monitors' = [monitors EXCEPT ![newPid] = {}]
  /\ monitoredBy' = [monitoredBy EXCEPT ![newPid] = {}]
  /\ ioRegistered' = [ioRegistered EXCEPT ![newPid] = FALSE]
  /\ ioReady' = [ioReady EXCEPT ![newPid] = FALSE]
  /\ receiveTimerId' = [receiveTimerId EXCEPT ![newPid] = NoTimer]
  /\ receiveTimeoutFired' = [receiveTimeoutFired EXCEPT ![newPid] = FALSE]
  /\ syscallTimerId' = [syscallTimerId EXCEPT ![newPid] = NoTimer]
  /\ syscallTimeoutFired' = [syscallTimeoutFired EXCEPT ![newPid] = FALSE]
  /\ UNCHANGED << timerActive, timerMode, timerKind,
                  timerWakeTarget, timerSendTarget, timerMessage >>

LinkProcesses(observer, target) ==
  /\ observer \in Processes
  /\ target \in Processes
  /\ observer # target
  /\ procState[observer] = "Running"
  /\ IsAlive(target)
  /\ links' = [ x \in Processes |->
      IF x = observer THEN links[x] \cup { target }
      ELSE IF x = target THEN links[x] \cup { observer }
      ELSE links[x] ]
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter, ownerWorker,
                  workerQueue, slotQueued, slotExecuting, slotPending,
                  mailbox, saveQueue, trapExit, monitors, monitoredBy,
                  ioRegistered, ioReady, receiveTimerId, receiveTimeoutFired,
                  syscallTimerId, syscallTimeoutFired, timerActive, timerMode,
                  timerKind, timerWakeTarget, timerSendTarget, timerMessage >>

UnlinkProcesses(observer, target) ==
  /\ observer \in Processes
  /\ target \in Processes
  /\ procState[observer] = "Running"
  /\ links' = RemoveSymmetricLink(links, observer, target)
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter, ownerWorker,
                  workerQueue, slotQueued, slotExecuting, slotPending,
                  mailbox, saveQueue, trapExit, monitors, monitoredBy,
                  ioRegistered, ioReady, receiveTimerId, receiveTimeoutFired,
                  syscallTimerId, syscallTimeoutFired, timerActive, timerMode,
                  timerKind, timerWakeTarget, timerSendTarget, timerMessage >>

MonitorProcess(observer, target, ref) ==
  /\ observer \in Processes
  /\ target \in Processes
  /\ ref \in MonitorRefs
  /\ observer # target
  /\ procState[observer] = "Running"
  /\ IsAlive(target)
  /\ FreshMonitorRef(ref)
  /\ monitors' =
      [monitors EXCEPT ![observer] = @ \cup { << ref, target >> }]
  /\ monitoredBy' =
      [monitoredBy EXCEPT ![target] = @ \cup { << observer, ref >> }]
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter, ownerWorker,
                  workerQueue, slotQueued, slotExecuting, slotPending,
                  mailbox, saveQueue, trapExit, links, ioRegistered, ioReady,
                  receiveTimerId, receiveTimeoutFired, syscallTimerId,
                  syscallTimeoutFired, timerActive, timerMode, timerKind,
                  timerWakeTarget, timerSendTarget, timerMessage >>

DemonitorProcess(observer, ref, target) ==
  /\ observer \in Processes
  /\ target \in Processes
  /\ ref \in MonitorRefs
  /\ procState[observer] = "Running"
  /\ << ref, target >> \in monitors[observer]
  /\ monitors' = RemoveOutgoingMonitor(monitors, observer, ref, target)
  /\ monitoredBy' = RemoveIncomingMonitor(monitoredBy, target, observer, ref)
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter, ownerWorker,
                  workerQueue, slotQueued, slotExecuting, slotPending,
                  mailbox, saveQueue, trapExit, links, ioRegistered, ioReady,
                  receiveTimerId, receiveTimeoutFired, syscallTimerId,
                  syscallTimeoutFired, timerActive, timerMode, timerKind,
                  timerWakeTarget, timerSendTarget, timerMessage >>

DeliverMessage(target, msg) ==
  /\ target \in Processes
  /\ msg \in AllMessages
  /\ IsAlive(target)
  /\ CanAppendMailbox(target)
  /\ LET wake == EnqueueOnOwner(target, ownerWorker, workerQueue, slotQueued) IN
     /\ mailbox' = [mailbox EXCEPT ![target] = Enqueue(@, msg)]
     /\ IF procState[target] = "WaitingMessage"
        THEN
          /\ procState' = [procState EXCEPT ![target] = "Runnable"]
          /\ workerQueue' = wake.queues
          /\ slotQueued' = wake.queuedFlags
        ELSE IF procState[target] = "Runnable"
        THEN
          /\ procState' = procState
          /\ workerQueue' = wake.queues
          /\ slotQueued' = wake.queuedFlags
        ELSE
          /\ procState' = procState
          /\ workerQueue' = workerQueue
          /\ slotQueued' = slotQueued
  /\ UNCHANGED << exitReason, blockedOp, waitingFilter, ownerWorker,
                  slotExecuting, slotPending, saveQueue, trapExit, links,
                  monitors, monitoredBy, ioRegistered, ioReady,
                  receiveTimerId, receiveTimeoutFired, syscallTimerId,
                  syscallTimeoutFired, timerActive, timerMode, timerKind,
                  timerWakeTarget, timerSendTarget, timerMessage >>

StealQueuedProcess(thief, victim, pid) ==
  /\ thief \in Workers
  /\ victim \in Workers
  /\ thief # victim
  /\ workerQueue[victim] # <<>>
  /\ pid \in Processes
  /\ Contains(workerQueue[victim], pid)
  /\ ~Contains(workerQueue[thief], pid)
  /\ CanAppendRunQueue(thief)
  /\ LET i == FirstIndexOf(workerQueue[victim], pid) IN
     /\ i # 0
     /\ workerQueue' =
          [ workerQueue EXCEPT
              ![victim] = RemoveIndex(@, i),
              ![thief] = Enqueue(@, pid) ]
  /\ ownerWorker' = [ownerWorker EXCEPT ![pid] = thief]
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter,
                  slotQueued, slotExecuting, slotPending, mailbox, saveQueue,
                  trapExit, links, monitors, monitoredBy, ioRegistered,
                  ioReady, receiveTimerId, receiveTimeoutFired,
                  syscallTimerId, syscallTimeoutFired, timerActive, timerMode,
                  timerKind, timerWakeTarget, timerSendTarget, timerMessage >>

PopStaleEntry(worker) ==
  /\ worker \in Workers
  /\ workerQueue[worker] # <<>>
  /\ LET pid == Head(workerQueue[worker]) IN
     /\ (slotExecuting[pid]
         \/ procState[pid] \in {
              "WaitingMessage", "WaitingIO", "Exited", "Finalized", "Absent"
            })
     /\ workerQueue' = [workerQueue EXCEPT ![worker] = Tail(@)]
     /\ slotQueued' = [slotQueued EXCEPT ![pid] = FALSE]
     /\ slotPending' =
          [ x \in Processes |->
              IF x = pid THEN slotPending[x] \/ slotExecuting[pid]
              ELSE slotPending[x] ]
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter, ownerWorker,
                  slotExecuting, mailbox, saveQueue, trapExit, links,
                  monitors, monitoredBy, ioRegistered, ioReady,
                  receiveTimerId, receiveTimeoutFired, syscallTimerId,
                  syscallTimeoutFired, timerActive, timerMode, timerKind,
                  timerWakeTarget, timerSendTarget, timerMessage >>

DispatchRunnable(worker) ==
  /\ worker \in Workers
  /\ workerQueue[worker] # <<>>
  /\ LET pid == Head(workerQueue[worker]) IN
     /\ procState[pid] \in { "Uninitialized", "Runnable" }
     /\ ~slotExecuting[pid]
     /\ workerQueue' = [workerQueue EXCEPT ![worker] = Tail(@)]
     /\ slotQueued' = [slotQueued EXCEPT ![pid] = FALSE]
     /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = TRUE]
     /\ procState' = [procState EXCEPT ![pid] = "Running"]
  /\ UNCHANGED << exitReason, blockedOp, waitingFilter, ownerWorker,
                  slotPending, mailbox, saveQueue, trapExit, links, monitors,
                  monitoredBy, ioRegistered, ioReady, receiveTimerId,
                  receiveTimeoutFired, syscallTimerId, syscallTimeoutFired,
                  timerActive, timerMode, timerKind, timerWakeTarget,
                  timerSendTarget, timerMessage >>

RunningYield(pid) ==
  /\ pid \in Processes
  /\ procState[pid] = "Running"
  /\ slotExecuting[pid]
  /\ blockedOp[pid] = "None"
  /\ LET wake == EnqueueOnOwner(pid, ownerWorker, workerQueue, slotQueued) IN
     /\ procState' = [procState EXCEPT ![pid] = "Runnable"]
     /\ workerQueue' = wake.queues
     /\ slotQueued' = wake.queuedFlags
  /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = FALSE]
  /\ slotPending' = [slotPending EXCEPT ![pid] = FALSE]
  /\ waitingFilter' = [waitingFilter EXCEPT ![pid] = NoSelector]
  /\ UNCHANGED << exitReason, blockedOp, ownerWorker, mailbox, saveQueue,
                  trapExit, links, monitors, monitoredBy, ioRegistered,
                  ioReady, receiveTimerId, receiveTimeoutFired,
                  syscallTimerId, syscallTimeoutFired, timerActive, timerMode,
                  timerKind, timerWakeTarget, timerSendTarget, timerMessage >>

RunningBeginReceiveNoWait(pid, selector) ==
  /\ pid \in Processes
  /\ selector \in SelectorUniverse
  /\ procState[pid] = "Running"
  /\ slotExecuting[pid]
  /\ blockedOp[pid] = "None"
  /\ LET scan == ReceiveScan(saveQueue[pid], mailbox[pid], selector) IN
     /\ scan.matched
     /\ mailbox' = [mailbox EXCEPT ![pid] = scan.newMailbox]
     /\ saveQueue' = [saveQueue EXCEPT ![pid] = scan.newSave]
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter,
                  ownerWorker, workerQueue, slotQueued, slotExecuting,
                  slotPending, trapExit, links, monitors, monitoredBy,
                  ioRegistered, ioReady, receiveTimerId, receiveTimeoutFired,
                  syscallTimerId, syscallTimeoutFired, timerActive, timerMode,
                  timerKind, timerWakeTarget, timerSendTarget, timerMessage >>

RunningBeginReceiveBlock(pid, selector, tid) ==
  /\ pid \in Processes
  /\ selector \in SelectorUniverse
  /\ procState[pid] = "Running"
  /\ slotExecuting[pid]
  /\ blockedOp[pid] = "None"
  /\ LET scan == ReceiveScan(saveQueue[pid], mailbox[pid], selector) IN
     /\ ~scan.matched
     /\ mailbox' = [mailbox EXCEPT ![pid] = scan.newMailbox]
     /\ saveQueue' = [saveQueue EXCEPT ![pid] = scan.newSave]
     /\ blockedOp' = [blockedOp EXCEPT ![pid] = "Receive"]
     /\ waitingFilter' = [waitingFilter EXCEPT ![pid] = selector]
     /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = FALSE]
     /\ slotPending' = [slotPending EXCEPT ![pid] = FALSE]
     /\ IF tid = NoTimer
        THEN
          /\ receiveTimerId' = receiveTimerId
          /\ receiveTimeoutFired' = [receiveTimeoutFired EXCEPT ![pid] = FALSE]
          /\ timerActive' = timerActive
          /\ timerMode' = timerMode
          /\ timerKind' = timerKind
          /\ timerWakeTarget' = timerWakeTarget
          /\ timerSendTarget' = timerSendTarget
          /\ timerMessage' = timerMessage
        ELSE
          /\ tid \in TimerIds
          /\ FreshTimer(tid)
          /\ receiveTimerId' = [receiveTimerId EXCEPT ![pid] = tid]
          /\ receiveTimeoutFired' = [receiveTimeoutFired EXCEPT ![pid] = FALSE]
          /\ timerActive' = [timerActive EXCEPT ![tid] = TRUE]
          /\ timerMode' = [timerMode EXCEPT ![tid] = "OneShot"]
          /\ timerKind' = [timerKind EXCEPT ![tid] = "WakeProcess"]
          /\ timerWakeTarget' = [timerWakeTarget EXCEPT ![tid] = pid]
          /\ timerSendTarget' = timerSendTarget
          /\ timerMessage' = timerMessage
     /\ IF slotPending[pid]
        THEN
          LET wake == EnqueueOnOwner(pid, ownerWorker, workerQueue, slotQueued) IN
          /\ procState' = [procState EXCEPT ![pid] = "Runnable"]
          /\ workerQueue' = wake.queues
          /\ slotQueued' = wake.queuedFlags
        ELSE
          /\ procState' = [procState EXCEPT ![pid] = "WaitingMessage"]
          /\ workerQueue' = workerQueue
          /\ slotQueued' = slotQueued
  /\ UNCHANGED << exitReason, ownerWorker, trapExit, links, monitors,
                  monitoredBy, ioRegistered, ioReady, syscallTimerId,
                  syscallTimeoutFired >>

RunningResumeBlockedReceive(pid) ==
  /\ pid \in Processes
  /\ procState[pid] = "Running"
  /\ slotExecuting[pid]
  /\ blockedOp[pid] = "Receive"
  /\ LET selector == waitingFilter[pid] IN
     LET scan == ReceiveScan(saveQueue[pid], mailbox[pid], selector) IN
     /\ mailbox' = [mailbox EXCEPT ![pid] = scan.newMailbox]
     /\ saveQueue' = [saveQueue EXCEPT ![pid] = scan.newSave]
     /\ IF scan.matched
        THEN
          /\ blockedOp' = [blockedOp EXCEPT ![pid] = "None"]
          /\ waitingFilter' = [waitingFilter EXCEPT ![pid] = NoSelector]
          /\ receiveTimeoutFired' = [receiveTimeoutFired EXCEPT ![pid] = FALSE]
          /\ IF receiveTimerId[pid] = NoTimer
             THEN
               /\ receiveTimerId' = receiveTimerId
               /\ timerActive' = timerActive
               /\ timerMode' = timerMode
               /\ timerKind' = timerKind
               /\ timerWakeTarget' = timerWakeTarget
               /\ timerSendTarget' = timerSendTarget
               /\ timerMessage' = timerMessage
             ELSE
               /\ receiveTimerId' = ClearReceiveTimerIds(receiveTimerId, pid)
               /\ timerActive' = [timerActive EXCEPT ![receiveTimerId[pid]] = FALSE]
               /\ timerMode' = timerMode
               /\ timerKind' = timerKind
               /\ timerWakeTarget' = timerWakeTarget
               /\ timerSendTarget' = timerSendTarget
               /\ timerMessage' = timerMessage
          /\ procState' = procState
          /\ workerQueue' = workerQueue
          /\ slotQueued' = slotQueued
          /\ slotExecuting' = slotExecuting
          /\ slotPending' = slotPending
        ELSE IF receiveTimeoutFired[pid]
        THEN
          /\ blockedOp' = [blockedOp EXCEPT ![pid] = "None"]
          /\ waitingFilter' = [waitingFilter EXCEPT ![pid] = NoSelector]
          /\ receiveTimeoutFired' = [receiveTimeoutFired EXCEPT ![pid] = FALSE]
          /\ IF receiveTimerId[pid] = NoTimer
             THEN
               /\ receiveTimerId' = receiveTimerId
               /\ timerActive' = timerActive
               /\ timerMode' = timerMode
               /\ timerKind' = timerKind
               /\ timerWakeTarget' = timerWakeTarget
               /\ timerSendTarget' = timerSendTarget
               /\ timerMessage' = timerMessage
             ELSE
               /\ receiveTimerId' = ClearReceiveTimerIds(receiveTimerId, pid)
               /\ timerActive' = [timerActive EXCEPT ![receiveTimerId[pid]] = FALSE]
               /\ timerMode' = timerMode
               /\ timerKind' = timerKind
               /\ timerWakeTarget' = timerWakeTarget
               /\ timerSendTarget' = timerSendTarget
               /\ timerMessage' = timerMessage
          /\ procState' = procState
          /\ workerQueue' = workerQueue
          /\ slotQueued' = slotQueued
          /\ slotExecuting' = slotExecuting
          /\ slotPending' = slotPending
        ELSE IF slotPending[pid]
        THEN
          LET wake == EnqueueOnOwner(pid, ownerWorker, workerQueue, slotQueued) IN
          /\ procState' = [procState EXCEPT ![pid] = "Runnable"]
          /\ workerQueue' = wake.queues
          /\ slotQueued' = wake.queuedFlags
          /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = FALSE]
          /\ slotPending' = [slotPending EXCEPT ![pid] = FALSE]
          /\ blockedOp' = blockedOp
          /\ waitingFilter' = waitingFilter
          /\ receiveTimeoutFired' = receiveTimeoutFired
          /\ receiveTimerId' = receiveTimerId
          /\ timerActive' = timerActive
          /\ timerMode' = timerMode
          /\ timerKind' = timerKind
          /\ timerWakeTarget' = timerWakeTarget
          /\ timerSendTarget' = timerSendTarget
          /\ timerMessage' = timerMessage
        ELSE
          /\ procState' = [procState EXCEPT ![pid] = "WaitingMessage"]
          /\ workerQueue' = workerQueue
          /\ slotQueued' = slotQueued
          /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = FALSE]
          /\ slotPending' = [slotPending EXCEPT ![pid] = FALSE]
          /\ blockedOp' = blockedOp
          /\ waitingFilter' = waitingFilter
          /\ receiveTimeoutFired' = receiveTimeoutFired
          /\ receiveTimerId' = receiveTimerId
          /\ timerActive' = timerActive
          /\ timerMode' = timerMode
          /\ timerKind' = timerKind
          /\ timerWakeTarget' = timerWakeTarget
          /\ timerSendTarget' = timerSendTarget
          /\ timerMessage' = timerMessage
  /\ UNCHANGED << exitReason, ownerWorker, trapExit, links, monitors,
                  monitoredBy, ioRegistered, ioReady, syscallTimerId,
                  syscallTimeoutFired >>

RunningBeginSyscall(pid, tid) ==
  /\ pid \in Processes
  /\ procState[pid] = "Running"
  /\ slotExecuting[pid]
  /\ blockedOp[pid] = "None"
  /\ IF ioReady[pid]
     THEN
       /\ ioReady' = [ioReady EXCEPT ![pid] = FALSE]
       /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter,
                       ownerWorker, workerQueue, slotQueued, slotExecuting,
                       slotPending, mailbox, saveQueue, trapExit, links,
                       monitors, monitoredBy, ioRegistered, receiveTimerId,
                       receiveTimeoutFired, syscallTimerId,
                       syscallTimeoutFired, timerActive, timerMode, timerKind,
                       timerWakeTarget, timerSendTarget, timerMessage >>
     ELSE
       /\ blockedOp' = [blockedOp EXCEPT ![pid] = "Syscall"]
       /\ ioRegistered' = [ioRegistered EXCEPT ![pid] = TRUE]
       /\ ioReady' = [ioReady EXCEPT ![pid] = FALSE]
       /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = FALSE]
       /\ slotPending' = [slotPending EXCEPT ![pid] = FALSE]
       /\ IF tid = NoTimer
          THEN
            /\ syscallTimerId' = syscallTimerId
            /\ syscallTimeoutFired' = [syscallTimeoutFired EXCEPT ![pid] = FALSE]
            /\ timerActive' = timerActive
            /\ timerMode' = timerMode
            /\ timerKind' = timerKind
            /\ timerWakeTarget' = timerWakeTarget
            /\ timerSendTarget' = timerSendTarget
            /\ timerMessage' = timerMessage
          ELSE
            /\ tid \in TimerIds
            /\ FreshTimer(tid)
            /\ syscallTimerId' = [syscallTimerId EXCEPT ![pid] = tid]
            /\ syscallTimeoutFired' = [syscallTimeoutFired EXCEPT ![pid] = FALSE]
            /\ timerActive' = [timerActive EXCEPT ![tid] = TRUE]
            /\ timerMode' = [timerMode EXCEPT ![tid] = "OneShot"]
            /\ timerKind' = [timerKind EXCEPT ![tid] = "WakeProcess"]
            /\ timerWakeTarget' = [timerWakeTarget EXCEPT ![tid] = pid]
            /\ timerSendTarget' = timerSendTarget
            /\ timerMessage' = timerMessage
       /\ IF slotPending[pid]
          THEN
            LET wake == EnqueueOnOwner(pid, ownerWorker, workerQueue, slotQueued) IN
            /\ procState' = [procState EXCEPT ![pid] = "Runnable"]
            /\ workerQueue' = wake.queues
            /\ slotQueued' = wake.queuedFlags
          ELSE
            /\ procState' = [procState EXCEPT ![pid] = "WaitingIO"]
            /\ workerQueue' = workerQueue
            /\ slotQueued' = slotQueued
  /\ UNCHANGED << exitReason, waitingFilter, ownerWorker, mailbox, saveQueue,
                  trapExit, links, monitors, monitoredBy, receiveTimerId,
                  receiveTimeoutFired >>

RunningResumeBlockedSyscall(pid) ==
  /\ pid \in Processes
  /\ procState[pid] = "Running"
  /\ slotExecuting[pid]
  /\ blockedOp[pid] = "Syscall"
  /\ IF ioReady[pid]
     THEN
       /\ blockedOp' = [blockedOp EXCEPT ![pid] = "None"]
       /\ ioReady' = [ioReady EXCEPT ![pid] = FALSE]
       /\ ioRegistered' = [ioRegistered EXCEPT ![pid] = FALSE]
       /\ syscallTimeoutFired' = [syscallTimeoutFired EXCEPT ![pid] = FALSE]
       /\ IF syscallTimerId[pid] = NoTimer
          THEN
            /\ syscallTimerId' = syscallTimerId
            /\ timerActive' = timerActive
            /\ timerMode' = timerMode
            /\ timerKind' = timerKind
            /\ timerWakeTarget' = timerWakeTarget
            /\ timerSendTarget' = timerSendTarget
            /\ timerMessage' = timerMessage
          ELSE
            /\ syscallTimerId' = ClearSyscallTimerIds(syscallTimerId, pid)
            /\ timerActive' = [timerActive EXCEPT ![syscallTimerId[pid]] = FALSE]
            /\ timerMode' = timerMode
            /\ timerKind' = timerKind
            /\ timerWakeTarget' = timerWakeTarget
            /\ timerSendTarget' = timerSendTarget
            /\ timerMessage' = timerMessage
       /\ UNCHANGED << procState, exitReason, waitingFilter, ownerWorker,
                       workerQueue, slotQueued, slotExecuting, slotPending,
                       mailbox, saveQueue, trapExit, links, monitors,
                       monitoredBy, receiveTimerId, receiveTimeoutFired >>
     ELSE IF syscallTimeoutFired[pid]
     THEN
       /\ blockedOp' = [blockedOp EXCEPT ![pid] = "None"]
       /\ ioReady' = [ioReady EXCEPT ![pid] = FALSE]
       /\ ioRegistered' = [ioRegistered EXCEPT ![pid] = FALSE]
       /\ syscallTimeoutFired' = [syscallTimeoutFired EXCEPT ![pid] = FALSE]
       /\ IF syscallTimerId[pid] = NoTimer
          THEN
            /\ syscallTimerId' = syscallTimerId
            /\ timerActive' = timerActive
            /\ timerMode' = timerMode
            /\ timerKind' = timerKind
            /\ timerWakeTarget' = timerWakeTarget
            /\ timerSendTarget' = timerSendTarget
            /\ timerMessage' = timerMessage
          ELSE
            /\ syscallTimerId' = ClearSyscallTimerIds(syscallTimerId, pid)
            /\ timerActive' = [timerActive EXCEPT ![syscallTimerId[pid]] = FALSE]
            /\ timerMode' = timerMode
            /\ timerKind' = timerKind
            /\ timerWakeTarget' = timerWakeTarget
            /\ timerSendTarget' = timerSendTarget
            /\ timerMessage' = timerMessage
       /\ UNCHANGED << procState, exitReason, waitingFilter, ownerWorker,
                       workerQueue, slotQueued, slotExecuting, slotPending,
                       mailbox, saveQueue, trapExit, links, monitors,
                       monitoredBy, receiveTimerId, receiveTimeoutFired >>
     ELSE IF slotPending[pid]
     THEN
       LET wake == EnqueueOnOwner(pid, ownerWorker, workerQueue, slotQueued) IN
       /\ procState' = [procState EXCEPT ![pid] = "Runnable"]
       /\ workerQueue' = wake.queues
       /\ slotQueued' = wake.queuedFlags
       /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = FALSE]
       /\ slotPending' = [slotPending EXCEPT ![pid] = FALSE]
       /\ UNCHANGED << exitReason, blockedOp, waitingFilter, ownerWorker,
                       mailbox, saveQueue, trapExit, links, monitors,
                       monitoredBy, ioRegistered, ioReady, receiveTimerId,
                       receiveTimeoutFired, syscallTimerId,
                       syscallTimeoutFired, timerActive, timerMode, timerKind,
                       timerWakeTarget, timerSendTarget, timerMessage >>
     ELSE
       /\ procState' = [procState EXCEPT ![pid] = "WaitingIO"]
       /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = FALSE]
       /\ slotPending' = [slotPending EXCEPT ![pid] = FALSE]
       /\ UNCHANGED << exitReason, blockedOp, waitingFilter, ownerWorker,
                       workerQueue, slotQueued, mailbox, saveQueue, trapExit,
                       links, monitors, monitoredBy, ioRegistered, ioReady,
                       receiveTimerId, receiveTimeoutFired, syscallTimerId,
                       syscallTimeoutFired, timerActive, timerMode, timerKind,
                       timerWakeTarget, timerSendTarget, timerMessage >>

IOBecomesReady(pid) ==
  /\ pid \in Processes
  /\ ioRegistered[pid]
  /\ IsAlive(pid)
  /\ LET wake == EnqueueOnOwner(pid, ownerWorker, workerQueue, slotQueued) IN
     /\ ioRegistered' = [ioRegistered EXCEPT ![pid] = FALSE]
     /\ ioReady' = [ioReady EXCEPT ![pid] = TRUE]
     /\ IF procState[pid] = "WaitingIO"
        THEN
          /\ procState' = [procState EXCEPT ![pid] = "Runnable"]
          /\ workerQueue' = wake.queues
          /\ slotQueued' = wake.queuedFlags
        ELSE
          /\ procState' = procState
          /\ workerQueue' = workerQueue
          /\ slotQueued' = slotQueued
  /\ UNCHANGED << exitReason, blockedOp, waitingFilter, ownerWorker,
                  slotExecuting, slotPending, mailbox, saveQueue, trapExit,
                  links, monitors, monitoredBy, receiveTimerId,
                  receiveTimeoutFired, syscallTimerId, syscallTimeoutFired,
                  timerActive, timerMode, timerKind, timerWakeTarget,
                  timerSendTarget, timerMessage >>

AddUserOneShotTimer(tid, target, msg) ==
  /\ tid \in TimerIds
  /\ target \in Processes
  /\ msg \in UserMessages
  /\ FreshTimer(tid)
  /\ timerActive' = [timerActive EXCEPT ![tid] = TRUE]
  /\ timerMode' = [timerMode EXCEPT ![tid] = "OneShot"]
  /\ timerKind' = [timerKind EXCEPT ![tid] = "SendMessage"]
  /\ timerWakeTarget' = timerWakeTarget
  /\ timerSendTarget' = [timerSendTarget EXCEPT ![tid] = target]
  /\ timerMessage' = [timerMessage EXCEPT ![tid] = msg]
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter, ownerWorker,
                  workerQueue, slotQueued, slotExecuting, slotPending,
                  mailbox, saveQueue, trapExit, links, monitors, monitoredBy,
                  ioRegistered, ioReady, receiveTimerId, receiveTimeoutFired,
                  syscallTimerId, syscallTimeoutFired >>

AddUserIntervalTimer(tid, target, msg) ==
  /\ tid \in TimerIds
  /\ target \in Processes
  /\ msg \in UserMessages
  /\ FreshTimer(tid)
  /\ timerActive' = [timerActive EXCEPT ![tid] = TRUE]
  /\ timerMode' = [timerMode EXCEPT ![tid] = "Interval"]
  /\ timerKind' = [timerKind EXCEPT ![tid] = "SendMessage"]
  /\ timerWakeTarget' = timerWakeTarget
  /\ timerSendTarget' = [timerSendTarget EXCEPT ![tid] = target]
  /\ timerMessage' = [timerMessage EXCEPT ![tid] = msg]
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter, ownerWorker,
                  workerQueue, slotQueued, slotExecuting, slotPending,
                  mailbox, saveQueue, trapExit, links, monitors, monitoredBy,
                  ioRegistered, ioReady, receiveTimerId, receiveTimeoutFired,
                  syscallTimerId, syscallTimeoutFired >>

CancelTimer(tid) ==
  /\ tid \in TimerIds
  /\ timerActive[tid]
  /\ timerActive' = [timerActive EXCEPT ![tid] = FALSE]
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter, ownerWorker,
                  workerQueue, slotQueued, slotExecuting, slotPending,
                  mailbox, saveQueue, trapExit, links, monitors, monitoredBy,
                  ioRegistered, ioReady, receiveTimerId, receiveTimeoutFired,
                  syscallTimerId, syscallTimeoutFired, timerMode, timerKind,
                  timerWakeTarget, timerSendTarget, timerMessage >>

FireWakeTimer(tid) ==
  /\ tid \in TimerIds
  /\ timerActive[tid]
  /\ timerKind[tid] = "WakeProcess"
  /\ LET pid == timerWakeTarget[tid] IN
     LET wake == EnqueueOnOwner(pid, ownerWorker, workerQueue, slotQueued) IN
     /\ IF pid \in Processes /\ IsAlive(pid)
        THEN
          /\ IF receiveTimerId[pid] = tid
             THEN receiveTimeoutFired' = [receiveTimeoutFired EXCEPT ![pid] = TRUE]
             ELSE receiveTimeoutFired' = receiveTimeoutFired
          /\ IF syscallTimerId[pid] = tid
             THEN
               /\ syscallTimeoutFired' = [syscallTimeoutFired EXCEPT ![pid] = TRUE]
               /\ ioRegistered' = [ioRegistered EXCEPT ![pid] = FALSE]
             ELSE
               /\ syscallTimeoutFired' = syscallTimeoutFired
               /\ ioRegistered' = ioRegistered
          /\ IF procState[pid] \in { "WaitingMessage", "WaitingIO" }
             THEN
               /\ procState' = [procState EXCEPT ![pid] = "Runnable"]
               /\ workerQueue' = wake.queues
               /\ slotQueued' = wake.queuedFlags
             ELSE
               /\ procState' = procState
               /\ workerQueue' = workerQueue
               /\ slotQueued' = slotQueued
        ELSE
          /\ procState' = procState
          /\ workerQueue' = workerQueue
          /\ slotQueued' = slotQueued
          /\ receiveTimeoutFired' = receiveTimeoutFired
          /\ syscallTimeoutFired' = syscallTimeoutFired
          /\ ioRegistered' = ioRegistered
     /\ timerActive' =
          [timerActive EXCEPT ![tid] =
              IF timerMode[tid] = "Interval" THEN TRUE ELSE FALSE ]
  /\ UNCHANGED << exitReason, blockedOp, waitingFilter, ownerWorker,
                  slotExecuting, slotPending, mailbox, saveQueue, trapExit,
                  links, monitors, monitoredBy, ioReady, receiveTimerId,
                  syscallTimerId, timerMode, timerKind, timerWakeTarget,
                  timerSendTarget, timerMessage >>

FireSendTimer(tid) ==
  /\ tid \in TimerIds
  /\ timerActive[tid]
  /\ timerKind[tid] = "SendMessage"
  /\ LET target == timerSendTarget[tid] IN
     /\ IF target \in Processes /\ IsAlive(target) /\ CanAppendMailbox(target)
        THEN
          LET wake == EnqueueOnOwner(target, ownerWorker, workerQueue, slotQueued) IN
          /\ mailbox' = [mailbox EXCEPT ![target] = Enqueue(@, timerMessage[tid])]
          /\ IF procState[target] = "WaitingMessage"
             THEN
               /\ procState' = [procState EXCEPT ![target] = "Runnable"]
               /\ workerQueue' = wake.queues
               /\ slotQueued' = wake.queuedFlags
             ELSE IF procState[target] = "Runnable"
             THEN
               /\ procState' = procState
               /\ workerQueue' = wake.queues
               /\ slotQueued' = wake.queuedFlags
             ELSE
               /\ procState' = procState
               /\ workerQueue' = workerQueue
               /\ slotQueued' = slotQueued
        ELSE
          /\ mailbox' = mailbox
          /\ procState' = procState
          /\ workerQueue' = workerQueue
          /\ slotQueued' = slotQueued
  /\ timerActive' =
       [timerActive EXCEPT ![tid] =
           IF timerMode[tid] = "Interval" THEN TRUE ELSE FALSE ]
  /\ UNCHANGED << exitReason, blockedOp, waitingFilter, ownerWorker,
                  slotExecuting, slotPending, saveQueue, trapExit, links,
                  monitors, monitoredBy, ioRegistered, ioReady,
                  receiveTimerId, receiveTimeoutFired, syscallTimerId,
                  syscallTimeoutFired, timerMode, timerKind, timerWakeTarget,
                  timerSendTarget, timerMessage >>

RunningCrash(pid, reason) ==
  LET receiveCleared ==
        IF receiveTimerId[pid] = NoTimer
        THEN timerActive
        ELSE [timerActive EXCEPT ![receiveTimerId[pid]] = FALSE]
      allCleared ==
        IF syscallTimerId[pid] = NoTimer
        THEN receiveCleared
        ELSE [receiveCleared EXCEPT ![syscallTimerId[pid]] = FALSE]
  IN
  /\ pid \in Processes
  /\ reason \in ExitValues
  /\ procState[pid] = "Running"
  /\ slotExecuting[pid]
  /\ procState' = [procState EXCEPT ![pid] = "Exited"]
  /\ exitReason' = [exitReason EXCEPT ![pid] = reason]
  /\ blockedOp' = [blockedOp EXCEPT ![pid] = "None"]
  /\ waitingFilter' = [waitingFilter EXCEPT ![pid] = NoSelector]
  /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = FALSE]
  /\ slotPending' = [slotPending EXCEPT ![pid] = FALSE]
  /\ ioRegistered' = [ioRegistered EXCEPT ![pid] = FALSE]
  /\ ioReady' = [ioReady EXCEPT ![pid] = FALSE]
  /\ receiveTimeoutFired' = [receiveTimeoutFired EXCEPT ![pid] = FALSE]
  /\ syscallTimeoutFired' = [syscallTimeoutFired EXCEPT ![pid] = FALSE]
  /\ receiveTimerId' = ClearReceiveTimerIds(receiveTimerId, pid)
  /\ syscallTimerId' = ClearSyscallTimerIds(syscallTimerId, pid)
  /\ timerActive' = allCleared
  /\ UNCHANGED << ownerWorker, workerQueue, slotQueued, mailbox, saveQueue,
                  trapExit, links, monitors, monitoredBy, timerMode,
                  timerKind, timerWakeTarget, timerSendTarget, timerMessage >>

ResolveMonitorAfterExit(exited, observer, ref) ==
  /\ exited \in Processes
  /\ observer \in Processes
  /\ ref \in MonitorRefs
  /\ procState[exited] = "Exited"
  /\ << observer, ref >> \in monitoredBy[exited]
  /\ monitors' = RemoveOutgoingMonitor(monitors, observer, ref, exited)
  /\ monitoredBy' = RemoveIncomingMonitor(monitoredBy, exited, observer, ref)
  /\ IF IsAlive(observer) /\ CanAppendMailbox(observer)
     THEN
       LET wake == EnqueueOnOwner(observer, ownerWorker, workerQueue, slotQueued) IN
       /\ mailbox' =
            [mailbox EXCEPT
                ![observer] = Enqueue(@, DownSignal(exited, ref, exitReason[exited]))]
       /\ IF procState[observer] \in { "WaitingMessage", "WaitingIO" }
          THEN
            /\ procState' = [procState EXCEPT ![observer] = "Runnable"]
            /\ workerQueue' = wake.queues
            /\ slotQueued' = wake.queuedFlags
          ELSE IF procState[observer] = "Runnable"
          THEN
            /\ procState' = procState
            /\ workerQueue' = wake.queues
            /\ slotQueued' = wake.queuedFlags
          ELSE
            /\ procState' = procState
            /\ workerQueue' = workerQueue
            /\ slotQueued' = slotQueued
     ELSE
       /\ mailbox' = mailbox
       /\ procState' = procState
       /\ workerQueue' = workerQueue
       /\ slotQueued' = slotQueued
  /\ UNCHANGED << exitReason, blockedOp, waitingFilter, ownerWorker,
                  slotExecuting, slotPending, saveQueue, trapExit, links,
                  ioRegistered, ioReady, receiveTimerId, receiveTimeoutFired,
                  syscallTimerId, syscallTimeoutFired, timerActive, timerMode,
                  timerKind, timerWakeTarget, timerSendTarget, timerMessage >>

ResolveOutgoingMonitorAfterExit(exited, target, ref) ==
  /\ exited \in Processes
  /\ target \in Processes
  /\ ref \in MonitorRefs
  /\ procState[exited] = "Exited"
  /\ << ref, target >> \in monitors[exited]
  /\ monitors' = RemoveOutgoingMonitor(monitors, exited, ref, target)
  /\ monitoredBy' = RemoveIncomingMonitor(monitoredBy, target, exited, ref)
  /\ UNCHANGED << procState, exitReason, blockedOp, waitingFilter, ownerWorker,
                  workerQueue, slotQueued, slotExecuting, slotPending,
                  mailbox, saveQueue, trapExit, links, ioRegistered, ioReady,
                  receiveTimerId, receiveTimeoutFired, syscallTimerId,
                  syscallTimeoutFired, timerActive, timerMode, timerKind,
                  timerWakeTarget, timerSendTarget, timerMessage >>

ResolveLinkAfterExit(exited, linked) ==
  LET linkedReceiveCleared ==
        IF receiveTimerId[linked] = NoTimer
        THEN timerActive
        ELSE [timerActive EXCEPT ![receiveTimerId[linked]] = FALSE]
      linkedAllCleared ==
        IF syscallTimerId[linked] = NoTimer
        THEN linkedReceiveCleared
        ELSE [linkedReceiveCleared EXCEPT ![syscallTimerId[linked]] = FALSE]
  IN
  /\ exited \in Processes
  /\ linked \in Processes
  /\ procState[exited] = "Exited"
  /\ linked \in links[exited]
  /\ links' = RemoveSymmetricLink(links, exited, linked)
  /\ IF IsAlive(linked) /\ trapExit[linked] /\ CanAppendMailbox(linked)
     THEN
       LET wake == EnqueueOnOwner(linked, ownerWorker, workerQueue, slotQueued) IN
       /\ mailbox' =
            [mailbox EXCEPT
                ![linked] = Enqueue(@, ExitSignal(exited, exitReason[exited]))]
       /\ IF procState[linked] \in { "WaitingMessage", "WaitingIO" }
          THEN
            /\ procState' = [procState EXCEPT ![linked] = "Runnable"]
            /\ workerQueue' = wake.queues
            /\ slotQueued' = wake.queuedFlags
          ELSE IF procState[linked] = "Runnable"
          THEN
            /\ procState' = procState
            /\ workerQueue' = wake.queues
            /\ slotQueued' = wake.queuedFlags
          ELSE
            /\ procState' = procState
            /\ workerQueue' = workerQueue
            /\ slotQueued' = slotQueued
       /\ exitReason' = exitReason
     ELSE IF IsAlive(linked) /\ ~trapExit[linked] /\ exitReason[exited] # ExitOk
     THEN
       /\ procState' = [procState EXCEPT ![linked] = "Exited"]
       /\ exitReason' = [exitReason EXCEPT ![linked] = exitReason[exited]]
       /\ blockedOp' = [blockedOp EXCEPT ![linked] = "None"]
       /\ waitingFilter' = [waitingFilter EXCEPT ![linked] = NoSelector]
       /\ slotExecuting' = [slotExecuting EXCEPT ![linked] = FALSE]
       /\ slotPending' = [slotPending EXCEPT ![linked] = FALSE]
       /\ ioRegistered' = [ioRegistered EXCEPT ![linked] = FALSE]
       /\ ioReady' = [ioReady EXCEPT ![linked] = FALSE]
       /\ receiveTimerId' = ClearReceiveTimerIds(receiveTimerId, linked)
       /\ receiveTimeoutFired' = [receiveTimeoutFired EXCEPT ![linked] = FALSE]
       /\ syscallTimerId' = ClearSyscallTimerIds(syscallTimerId, linked)
       /\ syscallTimeoutFired' = [syscallTimeoutFired EXCEPT ![linked] = FALSE]
       /\ timerActive' = linkedAllCleared
       /\ mailbox' = mailbox
       /\ workerQueue' = workerQueue
       /\ slotQueued' = slotQueued
     ELSE
       /\ procState' = procState
       /\ exitReason' = exitReason
       /\ blockedOp' = blockedOp
       /\ waitingFilter' = waitingFilter
       /\ slotExecuting' = slotExecuting
       /\ slotPending' = slotPending
       /\ mailbox' = mailbox
       /\ workerQueue' = workerQueue
       /\ slotQueued' = slotQueued
       /\ ioRegistered' = ioRegistered
       /\ ioReady' = ioReady
       /\ receiveTimerId' = receiveTimerId
       /\ receiveTimeoutFired' = receiveTimeoutFired
       /\ syscallTimerId' = syscallTimerId
       /\ syscallTimeoutFired' = syscallTimeoutFired
       /\ timerActive' = timerActive
  /\ UNCHANGED << ownerWorker, saveQueue, trapExit, monitors, monitoredBy,
                  timerMode, timerKind, timerWakeTarget, timerSendTarget,
                  timerMessage >>

FinalizeExited(pid) ==
  /\ pid \in Processes
  /\ procState[pid] = "Exited"
  /\ links[pid] = {}
  /\ monitors[pid] = {}
  /\ monitoredBy[pid] = {}
  /\ procState' = [procState EXCEPT ![pid] = "Finalized"]
  /\ blockedOp' = [blockedOp EXCEPT ![pid] = "None"]
  /\ waitingFilter' = [waitingFilter EXCEPT ![pid] = NoSelector]
  /\ slotQueued' = [slotQueued EXCEPT ![pid] = FALSE]
  /\ slotExecuting' = [slotExecuting EXCEPT ![pid] = FALSE]
  /\ slotPending' = [slotPending EXCEPT ![pid] = FALSE]
  /\ mailbox' = [mailbox EXCEPT ![pid] = <<>>]
  /\ saveQueue' = [saveQueue EXCEPT ![pid] = <<>>]
  /\ ioRegistered' = [ioRegistered EXCEPT ![pid] = FALSE]
  /\ ioReady' = [ioReady EXCEPT ![pid] = FALSE]
  /\ receiveTimerId' = [receiveTimerId EXCEPT ![pid] = NoTimer]
  /\ receiveTimeoutFired' = [receiveTimeoutFired EXCEPT ![pid] = FALSE]
  /\ syscallTimerId' = [syscallTimerId EXCEPT ![pid] = NoTimer]
  /\ syscallTimeoutFired' = [syscallTimeoutFired EXCEPT ![pid] = FALSE]
  /\ workerQueue' = RemoveFromAllWorkerQueues(pid, workerQueue)
  /\ UNCHANGED << exitReason, ownerWorker, trapExit, links,
                  monitors, monitoredBy, timerActive, timerMode, timerKind,
                  timerWakeTarget, timerSendTarget, timerMessage >>

Next ==
  \/ \E newPid \in Processes, worker \in Workers : Spawn(newPid, worker)
  \/ \E observer \in Processes, target \in Processes : LinkProcesses(observer, target)
  \/ \E observer \in Processes, target \in Processes : UnlinkProcesses(observer, target)
  \/ \E observer \in Processes, target \in Processes, ref \in MonitorRefs :
       MonitorProcess(observer, target, ref)
  \/ \E observer \in Processes, target \in Processes, ref \in MonitorRefs :
       DemonitorProcess(observer, ref, target)
  \/ \E target \in Processes, msg \in AllMessages : DeliverMessage(target, msg)
  \/ \E thief \in Workers, victim \in Workers, pid \in Processes :
       StealQueuedProcess(thief, victim, pid)
  \/ \E worker \in Workers : PopStaleEntry(worker)
  \/ \E worker \in Workers : DispatchRunnable(worker)
  \/ \E pid \in Processes : RunningYield(pid)
  \/ \E pid \in Processes, selector \in SelectorUniverse :
       RunningBeginReceiveNoWait(pid, selector)
  \/ \E pid \in Processes, selector \in SelectorUniverse, tid \in TimerIds \cup { NoTimer } :
       RunningBeginReceiveBlock(pid, selector, tid)
  \/ \E pid \in Processes : RunningResumeBlockedReceive(pid)
  \/ \E pid \in Processes, tid \in TimerIds \cup { NoTimer } :
       RunningBeginSyscall(pid, tid)
  \/ \E pid \in Processes : RunningResumeBlockedSyscall(pid)
  \/ \E pid \in Processes : IOBecomesReady(pid)
  \/ \E tid \in TimerIds, target \in Processes, msg \in UserMessages :
       AddUserOneShotTimer(tid, target, msg)
  \/ \E tid \in TimerIds, target \in Processes, msg \in UserMessages :
       AddUserIntervalTimer(tid, target, msg)
  \/ \E tid \in TimerIds : CancelTimer(tid)
  \/ \E tid \in TimerIds : FireWakeTimer(tid)
  \/ \E tid \in TimerIds : FireSendTimer(tid)
  \/ \E pid \in Processes, reason \in ExitValues : RunningCrash(pid, reason)
  \/ \E exited \in Processes, observer \in Processes, ref \in MonitorRefs :
       ResolveMonitorAfterExit(exited, observer, ref)
  \/ \E exited \in Processes, target \in Processes, ref \in MonitorRefs :
       ResolveOutgoingMonitorAfterExit(exited, target, ref)
  \/ \E exited \in Processes, linked \in Processes :
       ResolveLinkAfterExit(exited, linked)
  \/ \E pid \in Processes : FinalizeExited(pid)

Spec == Init /\ [][Next]_vars

=============================================================================
