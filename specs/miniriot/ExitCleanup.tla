--------------------------- MODULE ExitCleanup ---------------------------
EXTENDS Integers, FiniteSets, Sequences, TLC, QueueUtils, MiniriotCommon

\* A focused model of links, monitors, crash propagation, and final cleanup.
\* This slice intentionally preserves the historical buggy behavior that left
\* stale survivor-side relation metadata behind after an exit. That makes it a
\* bug-reproduction model first; the companion bug config asks TLC to check the
\* intended cleanup laws and produce a counterexample.

CONSTANTS
  Processes,
  MonitorRefs,
  ExitReasons,
  ExitOk,
  NoReason,
  MaxMailboxLen

ExitValues == ExitReasons \cup { ExitOk }

Notifications ==
  { ExitSignal(from, reason) : from \in Processes, reason \in ExitValues }
  \cup
  { DownSignal(target, ref, reason) :
      target \in Processes,
      ref \in MonitorRefs,
      reason \in ExitValues }

ASSUME Processes # {}
ASSUME MonitorRefs # {}
ASSUME ExitOk \notin ExitReasons
ASSUME NoReason \notin ExitReasons
ASSUME MaxMailboxLen \in Nat

VARIABLES
  procState,
  exitReason,
  trapExit,
  links,
  monitors,
  monitoredBy,
  mailbox

vars ==
  << procState,
     exitReason,
     trapExit,
     links,
     monitors,
     monitoredBy,
     mailbox >>

IsAlive(p) == procState[p] \in AliveStates

CanAppendMailbox(p) == Len(mailbox[p]) < MaxMailboxLen

FreshMonitorRef(ref) ==
  \A p \in Processes : \A q \in Processes : << ref, q >> \notin monitors[p]

TypeOK ==
  /\ procState \in [Processes -> ProcessStates]
  /\ exitReason \in [Processes -> ExitValues \cup { NoReason }]
  /\ trapExit \in [Processes -> BOOLEAN]
  /\ links \in [Processes -> SUBSET Processes]
  /\ monitors \in [Processes -> SUBSET (MonitorRefs \X Processes)]
  /\ monitoredBy \in [Processes -> SUBSET (Processes \X MonitorRefs)]
  /\ mailbox \in [Processes -> Seq(Notifications)]

MailboxBounded ==
  \A p \in Processes : Len(mailbox[p]) <= MaxMailboxLen

\* Bounded smoke-run cutoff used only by the small passing config.
SmokeDepthBound == TLCGet("level") < 6

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

FinalizedProcessesClearOwnState ==
  \A p \in Processes :
    procState[p] = "Finalized"
    =>
    /\ links[p] = {}
    /\ monitors[p] = {}
    /\ monitoredBy[p] = {}
    /\ mailbox[p] = <<>>

\* Intended cleanup law: once a process is no longer alive, surviving processes
\* should not keep direct link or monitor metadata pointing at it. The current
\* slice deliberately violates this law so TLC can reproduce the design bug.
DeadProcessesAreGloballyDetached ==
  \A dead \in Processes :
    ~IsAlive(dead)
    =>
    /\ \A other \in Processes : dead \notin links[other]
    /\ \A other \in Processes :
         \A ref \in MonitorRefs : << ref, dead >> \notin monitors[other]
    /\ \A other \in Processes :
         \A ref \in MonitorRefs : << dead, ref >> \notin monitoredBy[other]

Init ==
  /\ procState = [ p \in Processes |-> "Runnable" ]
  /\ exitReason = [ p \in Processes |-> NoReason ]
  /\ trapExit = [ p \in Processes |-> FALSE ]
  /\ links = [ p \in Processes |-> {} ]
  /\ monitors = [ p \in Processes |-> {} ]
  /\ monitoredBy = [ p \in Processes |-> {} ]
  /\ mailbox = [ p \in Processes |-> <<>> ]

LinkProcesses(observer, target) ==
  /\ observer \in Processes
  /\ target \in Processes
  /\ observer # target
  /\ IsAlive(observer)
  /\ IsAlive(target)
  /\ links' =
      [ x \in Processes |->
          IF x = observer THEN links[x] \cup { target }
          ELSE IF x = target THEN links[x] \cup { observer }
          ELSE links[x] ]
  /\ UNCHANGED << procState, exitReason, trapExit, monitors, monitoredBy, mailbox >>

UnlinkProcesses(observer, target) ==
  /\ observer \in Processes
  /\ target \in Processes
  /\ observer # target
  /\ target \in links[observer]
  /\ links' = RemoveSymmetricLink(links, observer, target)
  /\ UNCHANGED << procState, exitReason, trapExit, monitors, monitoredBy, mailbox >>

MonitorProcess(observer, target, ref) ==
  /\ observer \in Processes
  /\ target \in Processes
  /\ observer # target
  /\ ref \in MonitorRefs
  /\ IsAlive(observer)
  /\ IsAlive(target)
  /\ FreshMonitorRef(ref)
  /\ monitors' =
      [ monitors EXCEPT ![observer] = @ \cup { << ref, target >> } ]
  /\ monitoredBy' =
      [ monitoredBy EXCEPT ![target] = @ \cup { << observer, ref >> } ]
  /\ UNCHANGED << procState, exitReason, trapExit, links, mailbox >>

DemonitorProcess(observer, ref, target) ==
  /\ observer \in Processes
  /\ target \in Processes
  /\ ref \in MonitorRefs
  /\ << ref, target >> \in monitors[observer]
  /\ monitors' = RemoveOutgoingMonitor(monitors, observer, ref, target)
  /\ monitoredBy' = RemoveIncomingMonitor(monitoredBy, target, observer, ref)
  /\ UNCHANGED << procState, exitReason, trapExit, links, mailbox >>

CrashProcess(pid, reason) ==
  /\ pid \in Processes
  /\ reason \in ExitValues
  /\ IsAlive(pid)
  /\ procState' = [procState EXCEPT ![pid] = "Exited"]
  /\ exitReason' = [exitReason EXCEPT ![pid] = reason]
  /\ UNCHANGED << trapExit, links, monitors, monitoredBy, mailbox >>

ResolveMonitorAfterExit(exited, observer, ref) ==
  /\ exited \in Processes
  /\ observer \in Processes
  /\ ref \in MonitorRefs
  /\ procState[exited] = "Exited"
  /\ << observer, ref >> \in monitoredBy[exited]
\* Historical bug: the dead process clears its own incoming-monitor metadata,
\* but the surviving observer keeps the stale outgoing monitor entry.
  /\ monitors' = monitors
  /\ monitoredBy' = RemoveIncomingMonitor(monitoredBy, exited, observer, ref)
  /\ IF IsAlive(observer) /\ CanAppendMailbox(observer)
     THEN
       /\ mailbox' =
            [mailbox EXCEPT
               ![observer] = Enqueue(@, DownSignal(exited, ref, exitReason[exited]))]
     ELSE
       /\ mailbox' = mailbox
  /\ UNCHANGED << procState, exitReason, trapExit, links >>

ResolveOutgoingMonitorAfterExit(exited, target, ref) ==
  /\ exited \in Processes
  /\ target \in Processes
  /\ ref \in MonitorRefs
  /\ procState[exited] = "Exited"
  /\ << ref, target >> \in monitors[exited]
\* Historical bug: the exiting observer clears its own monitor entry, but the
\* surviving target keeps the stale reverse registration.
  /\ monitors' = RemoveOutgoingMonitor(monitors, exited, ref, target)
  /\ monitoredBy' = monitoredBy
  /\ UNCHANGED << procState, exitReason, trapExit, links, mailbox >>

ResolveLinkAfterExit(exited, linked) ==
  /\ exited \in Processes
  /\ linked \in Processes
  /\ procState[exited] = "Exited"
  /\ linked \in links[exited]
\* Historical bug: the exiting process clears its own link set, but the
\* surviving process keeps a stale link back to the dead pid.
  /\ links' = [links EXCEPT ![exited] = @ \ { linked }]
  /\ IF IsAlive(linked) /\ trapExit[linked] /\ CanAppendMailbox(linked)
     THEN
       /\ procState' = procState
       /\ exitReason' = exitReason
       /\ mailbox' =
            [mailbox EXCEPT
               ![linked] = Enqueue(@, ExitSignal(exited, exitReason[exited]))]
     ELSE IF IsAlive(linked) /\ ~trapExit[linked] /\ exitReason[exited] # ExitOk
     THEN
       /\ procState' = [procState EXCEPT ![linked] = "Exited"]
       /\ exitReason' = [exitReason EXCEPT ![linked] = exitReason[exited]]
       /\ mailbox' = mailbox
     ELSE
       /\ procState' = procState
       /\ exitReason' = exitReason
       /\ mailbox' = mailbox
  /\ UNCHANGED << trapExit, monitors, monitoredBy >>

FinalizeExited(pid) ==
  /\ pid \in Processes
  /\ procState[pid] = "Exited"
\* Historical behavior: the dead process only insists that its own local
\* relation tables are clear before finalization. Survivor-side references may
\* still exist, which is exactly the bug the separate property config exposes.
  /\ links[pid] = {}
  /\ monitors[pid] = {}
  /\ monitoredBy[pid] = {}
  /\ procState' = [procState EXCEPT ![pid] = "Finalized"]
  /\ mailbox' = [mailbox EXCEPT ![pid] = <<>>]
  /\ UNCHANGED << exitReason, trapExit, links, monitors, monitoredBy >>

ToggleTrapExit(pid) ==
  /\ pid \in Processes
  /\ IsAlive(pid)
  /\ trapExit' = [trapExit EXCEPT ![pid] = ~@]
  /\ UNCHANGED << procState, exitReason, links, monitors, monitoredBy, mailbox >>

Next ==
  \/ \E observer \in Processes, target \in Processes :
       LinkProcesses(observer, target)
  \/ \E observer \in Processes, target \in Processes :
       UnlinkProcesses(observer, target)
  \/ \E observer \in Processes, target \in Processes, ref \in MonitorRefs :
       MonitorProcess(observer, target, ref)
  \/ \E observer \in Processes, target \in Processes, ref \in MonitorRefs :
       DemonitorProcess(observer, ref, target)
  \/ \E pid \in Processes : ToggleTrapExit(pid)
  \/ \E pid \in Processes, reason \in ExitValues : CrashProcess(pid, reason)
  \/ \E exited \in Processes, observer \in Processes, ref \in MonitorRefs :
       ResolveMonitorAfterExit(exited, observer, ref)
  \/ \E exited \in Processes, target \in Processes, ref \in MonitorRefs :
       ResolveOutgoingMonitorAfterExit(exited, target, ref)
  \/ \E exited \in Processes, linked \in Processes :
       ResolveLinkAfterExit(exited, linked)
  \/ \E pid \in Processes : FinalizeExited(pid)

Spec == Init /\ [][Next]_vars

=============================================================================
