--------------------------- MODULE QueueUtils ---------------------------
EXTENDS Integers, Sequences, FiniteSets, TLC

\* Small sequence helpers used by the runtime spec.
\* The main module keeps these operators in a separate file so the core state
\* machine can read more like a design document and less like list plumbing.

Indices(q) == 1..Len(q)

Elements(q) == { q[i] : i \in Indices(q) }

Prefix(q, n) ==
  IF n <= 0 THEN <<>>
  ELSE IF n >= Len(q) THEN q
  ELSE SubSeq(q, 1, n)

Suffix(q, n) ==
  IF n > Len(q) THEN <<>>
  ELSE IF n <= 1 THEN q
  ELSE SubSeq(q, n, Len(q))

Enqueue(q, x) == Append(q, x)

Occurrences(q, x) == Cardinality({ i \in Indices(q) : q[i] = x })

Contains(q, x) == Occurrences(q, x) > 0

RemoveIndex(q, i) == Prefix(q, i - 1) \o Suffix(q, i + 1)

Least(S) == CHOOSE x \in S : \A y \in S : x <= y

FirstIndexOf(q, x) ==
  IF { i \in Indices(q) : q[i] = x } = {}
  THEN 0
  ELSE Least({ i \in Indices(q) : q[i] = x })

FirstMatchingIndex(q, accepted) ==
  IF { i \in Indices(q) : q[i] \in accepted } = {}
  THEN 0
  ELSE Least({ i \in Indices(q) : q[i] \in accepted })

UniqueElements(q) == \A x \in Elements(q) : Occurrences(q, x) = 1

=============================================================================
