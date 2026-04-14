# 1000 additional tests for the OCaml stdlib

Columns: priority, what the test is testing, expected behavior.

## Actor+Agent+Process

1. **[P1]** Agent.start then Agent.get — returns the initial state-derived value
2. **[P1]** Agent.update then Agent.get — reflects the updated state
3. **[P1]** Agent.get_and_update — returns the computed reply and stores the new state atomically
4. **[P1]** Agent.cast — eventually updates the state without blocking the caller
5. **[P1]** Agent.start_link — creates an agent linked to the caller and supports the same get/update operations
6. **[P1]** Actor.self inside a spawned actor — returns a PID different from the parent's PID
7. **[P1]** Actor.spawn — starts an unlinked actor and returns a live PID
8. **[P1]** Actor.spawn_link — starts a linked actor; an abnormal child exit is observed by the linked parent according to the runtime link semantics
9. **[P1]** Process.id () — returns a positive operating-system process identifier
10. **[P1]** Process.default_stdio — uses inherited stdio streams by default

## Bool

1. **[P2]** Bool.equal true true — returns true
2. **[P2]** Bool.equal true false — returns false
3. **[P2]** Bool.compare false true — is negative
4. **[P2]** Bool.compare true false — is positive
5. **[P2]** Bool.not true — returns false
6. **[P2]** Bool.to_string false — returns "false"

## Calendar

1. **[P1]** Calendar.is_leap_year ~year:2000 — returns true
2. **[P1]** Calendar.is_leap_year ~year:1900 — returns false
3. **[P1]** Calendar.is_leap_year ~year:2024 — returns true
4. **[P1]** Calendar.is_leap_year ~year:2023 — returns false
5. **[P1]** Calendar.last_day_of_month ~year:2024 ~month:2 — returns 29
6. **[P1]** Calendar.last_day_of_month ~year:2023 ~month:2 — returns 28
7. **[P1]** Calendar.last_day_of_month on a 30-day month — returns 30
8. **[P1]** Calendar.last_day_of_month on invalid month 0 — raises Invalid_argument
9. **[P1]** Calendar.last_day_of_month on invalid month 13 — raises Invalid_argument
10. **[P1]** Calendar.is_valid_date for 2024-02-29 — returns true
11. **[P1]** Calendar.is_valid_date for 2023-02-29 — returns false
12. **[P1]** Calendar.is_valid_date for 2024-04-31 — returns false
13. **[P1]** Calendar.date_to_gregorian_days {year=0;month=1;day=1} — returns 0
14. **[P1]** Calendar.date_to_gregorian_days {year=1970;month=1;day=1} — returns Calendar.days_from_0_to_1970
15. **[P1]** Calendar.gregorian_days_to_date 0 — returns {year=0;month=1;day=1}
16. **[P1]** Calendar.gregorian_days_to_date (Calendar.days_from_0_to_1970) — returns {year=1970;month=1;day=1}
17. **[P1]** Calendar date -> gregorian days -> date round-trip for representative leap and non-leap dates — returns the original date
18. **[P1]** Calendar.naive_to_gregorian_seconds epoch midnight — returns 0 for year 0 midnight
19. **[P1]** Calendar.gregorian_seconds_to_naive 0 — returns year 0 / Jan 1 / 00:00:00
20. **[P1]** Calendar day/time -> gregorian seconds -> day/time round-trip — returns the original date and time for representative inputs
21. **[P1]** Calendar.day_of_week for a known Monday date — returns Monday
22. **[P1]** Calendar.day_of_week for a known Sunday date — returns Sunday
23. **[P1]** Calendar.iso_week_number for a mid-year date — returns the expected ISO year/week
24. **[P1]** Calendar.iso_week_number around New Year boundary — returns the correct ISO year and week even when week belongs to adjacent year
25. **[P1]** Calendar.time_to_seconds {23,59,59} — returns 86399
26. **[P1]** Calendar.seconds_to_time 86399 — returns {hour=23; minute=59; second=59}

## Char

1. **[P2]** Char.from_int 65 — returns Some 'A'
2. **[P2]** Char.from_int 0 — returns Some '\x00'
3. **[P2]** Char.from_int 255 — returns Some byte 255
4. **[P2]** Char.from_int (-1) — returns None
5. **[P2]** Char.from_int 256 — returns None
6. **[P2]** Char.from_int_unchecked 97 — returns 'a'
7. **[P2]** Char.to_int 'A' — returns 65
8. **[P2]** Char.code '\n' — returns 10
9. **[P2]** Char.lowercase_ascii 'A' — returns 'a'
10. **[P2]** Char.lowercase_ascii '!' — returns '!' unchanged
11. **[P2]** Char.uppercase_ascii 'z' — returns 'Z'
12. **[P2]** Char.uppercase_ascii '5' — returns '5' unchanged

## Collections.Array

1. **[P2]** Array.make ~count:3 ~value:7 — creates [|7;7;7|]
2. **[P2]** Array.init ~count:4 (fun i -> i * i) — creates [|0;1;4;9|]
3. **[P2]** Array.length on a 3-element array — returns 3
4. **[P2]** Array.get valid index — returns Some element
5. **[P2]** Array.get negative index — returns None
6. **[P2]** Array.get past-the-end index — returns None
7. **[P2]** Array.get_unchecked valid index — returns the element directly
8. **[P2]** Array.set valid index — mutates that slot only
9. **[P2]** Array.clone then mutate original — does not mutate the clone
10. **[P2]** Array.blit non-overlapping full copy — copies source slice into destination
11. **[P1]** Array.blit overlapping right shift — preserves OCaml Array.blit overlap semantics
12. **[P1]** Array.blit overlapping left shift — preserves OCaml Array.blit overlap semantics
13. **[P2]** Array.sub interior slice — returns the expected subarray
14. **[P2]** Array.sub zero length — returns an empty array
15. **[P2]** Array.for_each visits items in index order — observed visitation order is 0..n-1
16. **[P2]** Array.map doubles values — returns a fresh mapped array
17. **[P2]** Array.fold_left over [|1;2;3|] — accumulates left-to-right
18. **[P2]** Array.fold_right over [|1;2;3|] — accumulates right-to-left
19. **[P2]** Array.from_list [1;2;3] — creates [|1;2;3|]
20. **[P1]** Array.iter / mut_iter over 3 elements — produce items in index order

## Collections.Deque

1. **[P2]** Deque.create — starts empty
2. **[P2]** Deque.with_capacity ~size:1 — starts empty with capacity at least 1
3. **[P2]** Deque.push_front on empty — front and back become that value
4. **[P2]** Deque.push_back on empty — front and back become that value
5. **[P2]** Deque.push_front then pop_front — roundtrips the inserted value
6. **[P2]** Deque.push_back then pop_back — roundtrips the inserted value
7. **[P2]** Deque.push_front/push_back mix — front/back reflect both ends correctly
8. **[P2]** Deque.pop_front on empty — returns None
9. **[P2]** Deque.pop_back on empty — returns None
10. **[P2]** Deque.insert at 0 — matches push_front semantics
11. **[P2]** Deque.insert at length — matches push_back semantics
12. **[P1]** Deque.insert in the middle — shifts later values to preserve order
13. **[P2]** Deque.remove out of bounds — returns None
14. **[P2]** Deque.remove middle element — returns removed value and preserves remaining order
15. **[P1]** Deque.get valid logical index — returns expected value even after wrap-around
16. **[P2]** Deque.length and is_empty after mixed operations — reflect the current element count
17. **[P2]** Deque.clear — empties the deque
18. **[P1]** Deque.capacity grows when buffer fills — capacity increases and contents stay ordered
19. **[P1]** Deque.for_each after wrap-around — visits values in logical front-to-back order
20. **[P2]** Deque.fold_left after wrap-around — folds in logical front-to-back order
21. **[P2]** Deque.to_list after mixed pushes — matches logical ordering
22. **[P2]** Deque.contains present value — returns true
23. **[P1]** Deque.append left right — appends right to left and empties right
24. **[P1]** Deque.split_off at midpoint — returns tail half and leaves original prefix intact

## Collections.HashMap

1. **[P2]** HashMap.create — starts empty
2. **[P2]** HashMap.with_capacity ~size:32 — starts empty with reserved buckets
3. **[P2]** HashMap.from_list with unique keys — contains all pairs
4. **[P1]** HashMap.from_list with duplicate key — keeps the last value for that key
5. **[P2]** HashMap.insert new key — returns None and increments length
6. **[P1]** HashMap.insert existing key — returns Some old_value and does not change length
7. **[P2]** HashMap.get existing key — returns Some value
8. **[P2]** HashMap.get missing key — returns None
9. **[P2]** HashMap.remove existing key — returns Some value and decrements length
10. **[P2]** HashMap.remove missing key — returns None
11. **[P2]** HashMap.has_key existing key — returns true
12. **[P2]** HashMap.length after overwrite — counts distinct keys, not insert calls
13. **[P2]** HashMap.is_empty after clear — returns true
14. **[P2]** HashMap.clear — removes all entries
15. **[P2]** HashMap.keys after inserts — contains every key exactly once
16. **[P2]** HashMap.values after inserts — contains every current value exactly once
17. **[P2]** HashMap.for_each over all entries — visits each entry exactly once
18. **[P2]** HashMap.fold_left over all entries — accumulates every entry exactly once
19. **[P1]** HashMap.to_list then rebuild via from_list — preserves the same key->value mapping
20. **[P1]** HashMap.entry on missing key then or_insert — inserts default and returns it
21. **[P1]** HashMap.or_insert on existing key — returns current value without overwriting it
22. **[P1]** HashMap.and_modify on existing key — updates value in place
23. **[P1]** HashMap.and_modify on missing key — leaves map unchanged
24. **[P2]** HashMap.iter — yields each entry exactly once
25. **[P2]** HashMap.mut_iter after removals — yields only live entries
26. **[P1]** HashMap collision-heavy keys — still returns the correct values for every key

## Collections.HashSet

1. **[P2]** HashSet.create — starts empty
2. **[P2]** HashSet.with_capacity ~size:16 — starts empty
3. **[P1]** HashSet.from_list with duplicates — contains unique values only
4. **[P2]** HashSet.insert new value — returns true and increments length
5. **[P2]** HashSet.insert duplicate value — returns false and does not change length
6. **[P2]** HashSet.remove existing value — returns true and removes it
7. **[P2]** HashSet.remove missing value — returns false
8. **[P2]** HashSet.contains existing value — returns true
9. **[P2]** HashSet.contains missing value — returns false
10. **[P2]** HashSet.length after duplicate inserts — counts unique members only
11. **[P2]** HashSet.is_empty after clear — returns true
12. **[P2]** HashSet.clear — removes all members
13. **[P2]** HashSet.for_each — visits each unique value exactly once
14. **[P2]** HashSet.fold_left — accumulates every unique value exactly once
15. **[P2]** HashSet.to_list — contains each member exactly once
16. **[P1]** HashSet.union overlapping sets — contains all distinct members from both inputs
17. **[P1]** HashSet.intersection overlapping sets — contains only shared members
18. **[P1]** HashSet.difference left right — contains members only in left
19. **[P1]** HashSet.symmetric_difference — contains members in exactly one side
20. **[P2]** HashSet.is_subset true case — returns true
21. **[P2]** HashSet.is_subset false case — returns false
22. **[P2]** HashSet.is_superset true case — returns true
23. **[P2]** HashSet.is_disjoint for non-overlapping sets — returns true
24. **[P1]** HashSet.iter / mut_iter — yield each member exactly once

## Collections.Heap

1. **[P2]** Heap.create — starts as an empty min-heap
2. **[P2]** Heap.create_max — starts as an empty max-heap
3. **[P1]** Heap.create_with custom compare — respects the provided ordering
4. **[P2]** Heap.from_list [3;1;2] — peek returns the minimum
5. **[P1]** Heap.from_list_with custom descending compare — peek returns the maximum under that compare
6. **[P2]** Heap.push into min-heap — updates peek to the smallest element
7. **[P2]** Heap.pop on empty — returns None
8. **[P1]** Heap.pop on non-empty min-heap — returns elements in ascending order over repeated pops
9. **[P2]** Heap.pop_unchecked after non-empty check — returns the root element
10. **[P2]** Heap.peek on empty — returns None
11. **[P2]** Heap.peek_unchecked after non-empty check — returns the root element without removal
12. **[P2]** Heap.length after pushes/pops — tracks live item count exactly
13. **[P2]** Heap.is_empty after removing all items — returns true
14. **[P2]** Heap.clear — empties the heap
15. **[P1]** Heap.to_list on min-heap — returns a sorted ascending list
16. **[P1]** Heap.to_list_unordered — contains exactly the same multiset as the heap
17. **[P2]** Heap.for_each — visits each element exactly once
18. **[P2]** Heap.fold_left — accumulates over all elements exactly once
19. **[P1]** Heap.iter — can be exhausted without mutating the heap
20. **[P1]** Heap.mut_iter — yields every live element exactly once

## Collections.List

1. **[P2]** List.length [] — returns 0
2. **[P2]** List.compare_lengths shorter vs longer — is negative
3. **[P2]** List.is_empty [] — returns true
4. **[P2]** List.append [1;2] [3;4] — returns [1;2;3;4]
5. **[P2]** List.reverse [1;2;3] — returns [3;2;1]
6. **[P2]** List.reverse_append [1;2] [3;4] — returns [2;1;3;4]
7. **[P2]** List.concat [[1;2]; []; [3]] — returns [1;2;3]
8. **[P2]** List.init ~count:5 (fun i -> i) — returns [0;1;2;3;4]
9. **[P2]** List.head [] — returns None
10. **[P2]** List.head [42] — returns Some 42
11. **[P2]** List.tail [] — returns []
12. **[P2]** List.get valid index — returns Some element
13. **[P2]** List.get negative index — returns None
14. **[P2]** List.get_unchecked valid index — returns the element
15. **[P2]** List.map square [1;2;3] — returns [1;4;9]
16. **[P2]** List.for_each preserves input order — observed side effects are left-to-right
17. **[P1]** List.fold_left subtraction — uses left-associative folding
18. **[P1]** List.fold_right cons-like reconstruction — rebuilds the list in the original order
19. **[P1]** List.enumerate ["a";"b"] — returns [(0,"a");(1,"b")]
20. **[P2]** List.all even [2;4;6] — returns true
21. **[P2]** List.any even [1;3;4] — returns true
22. **[P2]** List.contains value present — returns true
23. **[P2]** List.find matching predicate — returns first matching element
24. **[P1]** List.filter_map mixed Some/None — drops Nones and unwraps Somes

## Collections.Queue

1. **[P2]** Queue.create — starts empty
2. **[P2]** Queue.with_capacity ~size:4 — starts empty with usable capacity
3. **[P2]** Queue.from_list [1;2;3] — preserves FIFO order
4. **[P2]** Queue.push then front — front returns the earliest pushed value
5. **[P2]** Queue.pop on empty — returns None
6. **[P2]** Queue.pop after pushes — returns values in FIFO order
7. **[P2]** Queue.length after push/pop sequence — tracks live element count exactly
8. **[P2]** Queue.is_empty after removing all items — returns true
9. **[P2]** Queue.clear — empties the queue
10. **[P2]** Queue.for_each — visits values in FIFO order
11. **[P2]** Queue.fold_left — accumulates in FIFO order
12. **[P2]** Queue.to_list — returns elements in FIFO order
13. **[P2]** Queue.contains existing value — returns true
14. **[P2]** Queue.contains missing value — returns false
15. **[P1]** Queue.append left right — moves right's items to the end of left and empties right
16. **[P1]** Queue.transfer ~src ~dst — moves src items to dst in order and empties src
17. **[P2]** Queue.iter — produces FIFO order
18. **[P1]** Queue.mut_iter after partial pops — produces only remaining items in FIFO order

## Collections.Vector

1. **[P2]** Vector.create — starts empty with length 0
2. **[P2]** Vector.with_capacity ~size:8 — starts empty with capacity at least 8
3. **[P2]** Vector.push on empty vector — increments length to 1 and first/last become that value
4. **[P1]** Vector.push enough elements to force growth — preserves all existing values and increases capacity
5. **[P2]** Vector.pop on empty vector — returns None
6. **[P2]** Vector.pop on non-empty vector — returns the last pushed value and decrements length
7. **[P2]** Vector.insert at index 0 — prepends the value
8. **[P2]** Vector.insert at length — acts like append
9. **[P1]** Vector.insert in the middle — shifts tail elements right by one
10. **[P2]** Vector.remove out of bounds — returns None
11. **[P2]** Vector.remove middle element — returns removed value and compacts the tail
12. **[P2]** Vector.get valid index — returns Some value
13. **[P2]** Vector.get invalid index — returns None
14. **[P2]** Vector.set valid index — returns Ok () and mutates the slot
15. **[P1]** Vector.set invalid index — returns Error (OutOfBoundsSet ...)
16. **[P2]** Vector.reserve ~size:0 — does not change length or contents
17. **[P2]** Vector.clear after several pushes — resets length to 0 while keeping reusable capacity
18. **[P2]** Vector.to_array after mutations — contains exactly the live prefix
19. **[P1]** Vector.append left right — moves right's contents into left and empties right
20. **[P1]** Vector.split_off ~at:0 — moves all elements into the returned vector and empties the original
21. **[P1]** Vector.split_off at length — returns an empty vector and leaves original unchanged
22. **[P2]** Vector.sort on unsorted ints — orders ascending using default compare
23. **[P2]** Vector.sort_by custom descending compare — orders by the custom comparator
24. **[P2]** Vector.reverse on odd length — reverses elements in place
25. **[P2]** Vector.first on empty — returns None
26. **[P2]** Vector.first and last on non-empty — return the first and last live elements
27. **[P2]** Vector.iter over current length — visits only live elements in order
28. **[P1]** Vector.mut_iter clone/consumption semantics — yields live elements in order without exposing garbage capacity

## Crypto

1. **[P1]** Crypto.hash_string on the same string twice — returns equal hashes deterministically
2. **[P1]** Crypto.hash_bytes on the UTF-8 bytes of a string — matches Crypto.hash_string for the same content
3. **[P1]** Crypto.hash_unit — is deterministic and stable across calls
4. **[P1]** Crypto.hash_int on the same int twice — returns equal hashes
5. **[P1]** Crypto.hash_float on the same float twice — returns equal hashes
6. **[P1]** Crypto.hash_bool true vs false — returns different hashes
7. **[P1]** Crypto.hash_list with same element hasher and same list contents — is deterministic and order-sensitive
8. **[P1]** Crypto.hash_array with same contents as hash_list — matches or intentionally differs consistently according to the library contract
9. **[P1]** Crypto.Hash.of_bytes |> Crypto.Hash.to_bytes — round-trips the byte content exactly
10. **[P1]** Crypto.Hash.length on a SHA-256 hash — returns 32
11. **[P1]** Crypto.Hash.equal on identical hashes — returns true
12. **[P1]** Crypto.Hash.compare on identical hashes — returns 0
13. **[P1]** Crypto.Digest.hex on a SHA-256 hash — returns lowercase hex with length 64
14. **[P1]** Crypto.Digest.base64 on a SHA-256 hash — returns valid padded Base64
15. **[P1]** Crypto.Digest.base64_url on a SHA-256 hash — uses URL-safe alphabet and no unsafe '+' or '/' characters
16. **[P1]** Crypto.Digest.bytes — returns the raw digest bytes
17. **[P1]** Crypto.Digest.to_int64 / to_int on the same hash — are deterministic across repeated calls
18. **[P1]** Crypto.Sha1 hash of 'abc' — matches the standard SHA-1 test vector
19. **[P1]** Crypto.Sha256 hash of 'abc' — matches the standard SHA-256 test vector
20. **[P1]** Crypto.Sha512 hash of 'abc' — matches the standard SHA-512 test vector
21. **[P1]** Crypto.Md5 hash of 'abc' — matches the standard MD5 test vector
22. **[P1]** Incremental hashing via create/write/write/finish — matches the one-shot hash of the concatenated input

## Env

1. **[P1]** Env.get String for present variable — returns Some original string value
2. **[P1]** Env.get String for missing variable — returns None
3. **[P1]** Env.get Int for decimal integer — returns Some parsed integer
4. **[P1]** Env.get Int for non-integer text — returns None
5. **[P1]** Env.get Float for decimal float — returns Some parsed float
6. **[P1]** Env.get Float for invalid float text — returns None
7. **[P1]** Env.get Bool for 'true' — returns Some true
8. **[P1]** Env.get Bool for 'false' — returns Some false
9. **[P1]** Env.get Bool for '1' and '0' — parses them as true and false respectively
10. **[P1]** Env.get Bool for 'yes' and 'no' — parses them as true and false respectively
11. **[P1]** Env.get Bool for mixed-case boolean text — either parses case-insensitively or rejects consistently; the behavior should be fixed by test
12. **[P1]** Env.get Char for single-character value — returns Some that character
13. **[P1]** Env.get Char for multi-character value — uses the first character only
14. **[P1]** Env.set on previously unset variable — returns None and the variable becomes visible through Env.get
15. **[P1]** Env.set on previously set variable — returns Some old_value and stores the new value
16. **[P1]** Env.vars after setting multiple variables — contains the inserted key/value pairs
17. **[P1]** Env.current_dir after Env.set_current_dir to a valid directory — reports the new directory
18. **[P1]** Env.set_current_dir to a non-directory path — returns Error

## Float

1. **[P2]** Float.from_int/to_int roundtrip for 42 — returns 42.0 then 42
2. **[P2]** Float.parse "3.14" — returns Some 3.14-ish value
3. **[P1]** Float.parse "nan" — returns Some NaN or the runtime's NaN token
4. **[P2]** Float.to_string ~precision:2 3.14159 — rounds to two decimal places
5. **[P2]** Float.is_finite 1.0 — returns true
6. **[P2]** Float.is_infinite infinity — returns true
7. **[P2]** Float.is_nan nan — returns true
8. **[P2]** Float.rem 7.5 2.0 — returns 1.5-ish remainder
9. **[P2]** Float.sqrt 9.0 and cbrt 27.0 — return 3.0 and 3.0
10. **[P2]** Float.floor/ceil/round on 2.6 and 2.4 — match runtime float rounding semantics

## Format

1. **[P2]** Format.format [] — returns empty string
2. **[P2]** Format.format with str/char/bool/int/bytes fragments — concatenates fragments in order into one string
3. **[P2]** Format.to_string on each constructor — matches the text used by Format.format

## Graph.Dot+Mermaid

1. **[P1]** Graph.Dot.create ~style:Directed — renders a 'digraph name { ... }' header
2. **[P1]** Graph.Dot.create ~style:Undirected — renders a 'graph name { ... }' header and undirected edges
3. **[P1]** Graph.Dot.add_node with label and attrs — renders the node with quoted label and attributes
4. **[P1]** Graph.Dot.add_edge directed — renders 'from -> to'
5. **[P1]** Graph.Dot.add_edge undirected — renders 'from -- to'
6. **[P1]** Graph.Dot.to_string on a graph with graph_attrs — includes graph-level attributes in the output
7. **[P1]** Graph.Dot.to_string preserves all added nodes and edges — contains one definition per added item
8. **[P1]** Graph.Dot node/edge labels containing quotes or special characters — are escaped or rendered consistently so the DOT remains valid
9. **[P1]** Graph.Mermaid.create default direction — renders 'graph TD'
10. **[P1]** Graph.Mermaid.create ~direction:LR — renders 'graph LR'
11. **[P1]** Graph.Mermaid.add_node with Rectangle/Circle/Diamond shapes — uses the expected Mermaid delimiters for each shape
12. **[P1]** Graph.Mermaid.add_edge with default style — renders a solid '-->' edge
13. **[P1]** Graph.Mermaid.add_edge with Dotted style — renders a '-.->' edge
14. **[P1]** Graph.Mermaid.add_edge with Thick style — renders a '==>' edge
15. **[P1]** Graph.Mermaid labeled edge — renders the label inline on the edge
16. **[P1]** Graph.Mermaid.to_string preserves insertion of all nodes and edges — contains each added node and edge exactly once

## Graph.SimpleGraph

1. **[P1]** SimpleGraph.make on a new graph — starts empty and topo_sort returns Ok []
2. **[P1]** SimpleGraph.add_node then get_node by id — returns Some node with the same id and value
3. **[P1]** SimpleGraph.get_node on unknown id — returns None
4. **[P1]** SimpleGraph.add_edge for A depends_on B — records B in A.deps
5. **[P1]** SimpleGraph.topo_sort on a single node — returns that node
6. **[P1]** SimpleGraph.topo_sort on a simple chain A<-B<-C — returns [A; B; C] in dependency order
7. **[P1]** SimpleGraph.topo_sort on two independent roots — returns a deterministic order based on node IDs
8. **[P1]** SimpleGraph.topo_sort on a diamond DAG — returns an order where each dependency appears before its dependents
9. **[P1]** SimpleGraph.topo_sort on a self-cycle — returns Error containing the cycle node ID
10. **[P1]** SimpleGraph.topo_sort on a two-node cycle — returns Error with the nodes involved in the cycle
11. **[P1]** SimpleGraph.topo_sort on a longer cycle — returns Error containing a representative cycle path
12. **[P1]** SimpleGraph.iter — visits every node exactly once
13. **[P1]** SimpleGraph.map — returns one mapped item per node
14. **[P1]** SimpleGraph.reachable_from on a leaf node — returns that node and all of its dependencies
15. **[P1]** SimpleGraph.reachable_from on multiple starting nodes — returns the union of all reachable dependency IDs
16. **[P1]** SimpleGraph.reachable_from on repeated starting nodes — does not duplicate reachable IDs
17. **[P1]** Node_id.next called repeatedly — returns unique monotonically increasing IDs
18. **[P1]** Node_id.eq / to_int / to_string — are internally consistent for a given node ID

## IO.Buffer

1. **[P2]** IO.Buffer.create ~size:0 — starts empty with length 0
2. **[P2]** IO.Buffer.add_char twice — appends both chars in order
3. **[P2]** IO.Buffer.add_string "hello" — appends the whole string
4. **[P2]** IO.Buffer.add_bytes bytes — appends the full bytes payload
5. **[P2]** IO.Buffer.add_subbytes interior slice — appends exactly that bytes slice
6. **[P2]** IO.Buffer.add_subbytes with zero length — is a no-op
7. **[P1]** IO.Buffer.add_subbytes with negative offset — panics with an invalid range
8. **[P1]** IO.Buffer.add_subbytes with length past source — panics with an invalid range
9. **[P2]** IO.Buffer.add_substring interior slice — appends exactly that string slice
10. **[P2]** IO.Buffer.add_substring with zero length — is a no-op
11. **[P1]** IO.Buffer.add_substring with negative length — panics with an invalid range
12. **[P1]** IO.Buffer.add_utf_8_uchar on a multibyte rune — encodes valid UTF-8 bytes into the buffer
13. **[P2]** IO.Buffer.get valid index — returns Some char
14. **[P2]** IO.Buffer.get invalid index — returns None
15. **[P2]** IO.Buffer.clear after writes — resets length to 0 and contents to empty string

## IO.Bytes

1. **[P2]** IO.Bytes.create ~size:4 — creates a bytes buffer of length 4
2. **[P2]** IO.Bytes.length — returns the current length
3. **[P2]** IO.Bytes.get valid index — returns Some char
4. **[P2]** IO.Bytes.get invalid index — returns None
5. **[P2]** IO.Bytes.get_unchecked valid index — returns the byte directly
6. **[P2]** IO.Bytes.set valid index — returns Ok () and mutates the byte
7. **[P1]** IO.Bytes.set invalid index — returns Error (OutOfBoundSet ...)
8. **[P2]** IO.Bytes.set_unchecked valid index — mutates without returning Result
9. **[P2]** IO.Bytes.blit full copy — copies source bytes into destination
10. **[P1]** IO.Bytes.blit overlapping same buffer right shift — matches bytes overlap semantics
11. **[P1]** IO.Bytes.blit overlapping same buffer left shift — matches bytes overlap semantics
12. **[P1]** IO.Bytes.blit invalid source slice — returns Error
13. **[P1]** IO.Bytes.blit invalid destination slice — returns Error
14. **[P2]** IO.Bytes.blit_unchecked on valid slices — copies without error wrapping
15. **[P2]** IO.Bytes.blit_string full string — copies exact characters into destination
16. **[P2]** IO.Bytes.fill interior range — fills only the selected range
17. **[P2]** IO.Bytes.from_string then to_string — roundtrip content exactly
18. **[P2]** IO.Bytes.sub interior slice — returns Ok slice with expected bytes
19. **[P2]** IO.Bytes.sub zero length — returns Ok empty bytes
20. **[P1]** IO.Bytes.sub invalid negative offset — returns Error
21. **[P1]** IO.Bytes.sub invalid too-long length — returns Error
22. **[P2]** IO.Bytes.sub_unchecked valid slice — returns exact bytes
23. **[P2]** IO.Bytes multiple mutations then to_string — reflect all writes in order
24. **[P1]** IO.Bytes independent copies from sub — mutating source after sub does not mutate the returned bytes

## IO.Iovec

1. **[P2]** IO.Iovec.create ~size:16 () — creates an empty iovec with writable capacity
2. **[P2]** IO.Iovec.with_capacity 4 — creates an empty iovec with room for segments
3. **[P2]** IO.Iovec.from_bytes — wraps one segment whose to_bytes equals the original bytes
4. **[P2]** IO.Iovec.from_string — to_string equals the original string
5. **[P2]** IO.Iovec.from_bytes_array — concatenates all byte segments in order
6. **[P2]** IO.Iovec.from_string_array — concatenates all string segments in order
7. **[P2]** IO.Iovec.length on multi-segment vec — returns total byte length across segments
8. **[P2]** IO.Iovec.for_each over segments — visits each segment in insertion order
9. **[P2]** IO.Iovec.sub ~pos:0 ~len:n — returns prefix bytes
10. **[P1]** IO.Iovec.sub interior range across segment boundary — returns the exact cross-boundary slice
11. **[P1]** IO.Iovec.sub exact full length — returns all bytes unchanged
12. **[P1]** IO.Iovec.to_bytes on mixed segments — returns exact concatenated bytes
13. **[P1]** IO.Iovec.to_string on mixed segments — returns exact concatenated string
14. **[P2]** IO.Iovec empty input arrays — produce an empty vector with length 0

## IO.Reader/Writer

1. **[P2]** IO.Reader.empty read into non-empty buffer — returns Ok 0
2. **[P2]** IO.Reader.from_string then read small buffer repeatedly — returns sequential chunks until EOF
3. **[P2]** IO.Reader.from_bytes then read_to_end — copies the entire content into the buffer and returns total bytes read
4. **[P1]** IO.Reader.read_vectored into two segments — fills vectored buffers in order and returns bytes read
5. **[P2]** IO.Reader.map_err on a failing reader — transforms the error value
6. **[P2]** IO.Reader.from_string read after EOF — returns Ok 0 consistently
7. **[P2]** IO.Writer.write to collecting sink — returns bytes written and appends exact content
8. **[P1]** IO.Writer.write_all to collecting sink — writes the whole buffer or returns the first error
9. **[P1]** IO.Writer.write_owned_vectored on two segments — writes concatenated data in order and returns total bytes
10. **[P1]** IO.Writer.write_all_vectored on two segments — writes all bytes in order
11. **[P2]** IO.Writer.map_err on failing writer — transforms the error value
12. **[P2]** IO.Writer.flush on buffered sink — forces pending data and returns Ok ()
13. **[P2]** IO.Reader + IO.Writer copy loop — reconstructs the original payload exactly
14. **[P2]** IO.Reader.from_string with empty source — immediately returns EOF
15. **[P1]** IO.Writer partial write then write_all — eventually leaves sink with the complete payload
16. **[P1]** IO.Reader.read with zero-length buffer — returns Ok 0 and does not advance source
17. **[P1]** IO.Reader.read_vectored with empty iovec — returns Ok 0
18. **[P2]** IO.Writer.write with empty string — returns Ok 0 or leaves sink unchanged according to implementation contract
19. **[P2]** IO.Writer.write_all with empty string — is a no-op success
20. **[P2]** IO.Writer.write_all_vectored with empty iovec — is a no-op success
21. **[P2]** IO.Reader.map_err on a reader that never errors — leaves successful reads unchanged
22. **[P2]** IO.Writer.map_err on a writer that never errors — leaves successful writes unchanged
23. **[P1]** String.to_reader with invalid chunk_size 0 — raises Invalid_argument
24. **[P1]** String.to_reader with chunk_size 1 then read_to_end — still reproduces the original string exactly

## Int

1. **[P2]** Int.zero and Int.one constants — are 0 and 1
2. **[P2]** Int.add/sub/mul on small positives — match OCaml arithmetic
3. **[P2]** Int.div 7 3 — returns 2
4. **[P2]** Int.rem 7 3 — returns 1
5. **[P2]** Int.abs (-5) — returns 5
6. **[P2]** Int.min 3 8 — returns 3
7. **[P2]** Int.max 3 8 — returns 8
8. **[P2]** Int.succ 41 and Int.pred 41 — return 42 and 40
9. **[P2]** Int.parse "12345" — returns Some 12345
10. **[P2]** Int.parse "12x" — returns None

## Int32

1. **[P2]** Int32.from_int/to_int roundtrip for 123 — roundtrips exactly
2. **[P2]** Int32.neg 5l — returns -5l
3. **[P2]** Int32.abs (-5l) — returns 5l
4. **[P2]** Int32.add/sub/mul/div/rem on small values — match Int32 semantics
5. **[P2]** Int32.logand/logor/logxor — produce expected bitwise results
6. **[P2]** Int32.shift_left 1l 3 — returns 8l
7. **[P2]** Int32.shift_right_logical (-1l) 1 — fills with zero bits on the left
8. **[P2]** Int32.from_float 12.9 — truncates toward zero to 12l
9. **[P2]** Int32.parse "-42" — returns Some -42l
10. **[P2]** Int32.parse "not-an-int32" — returns None

## Int64

1. **[P2]** Int64.from_int/to_int roundtrip for 123 — roundtrips exactly
2. **[P2]** Int64.from_int32/to_int32 roundtrip — preserves 32-bit value
3. **[P2]** Int64.lognot 0L — returns -1L
4. **[P2]** Int64.shift_left 1L 40 — returns 1L << 40
5. **[P2]** Int64.shift_right_logical (-1L) 1 — fills with zero bits on the left
6. **[P2]** Int64.add/sub/mul/div/rem on small values — match Int64 semantics
7. **[P2]** Int64.succ 9L and pred 9L — return 10L and 8L
8. **[P2]** Int64.from_float 12.9 — truncates toward zero to 12L
9. **[P1]** Int64.parse "9223372036854775807" — returns Some max_int
10. **[P2]** Int64.parse "foo" — returns None

## Iter.Cursor

1. **[P2]** Cursor.create source/position/length_remaining — start at position 0 with full length remaining
2. **[P2]** Cursor.peek on fresh cursor — returns first character without advancing
3. **[P2]** Cursor.peek_n 1 — returns second character without advancing
4. **[P2]** Cursor.advance on non-EOF — returns a new cursor with position +1
5. **[P2]** Cursor.advance at EOF — returns None
6. **[P2]** Cursor.advance_by within bounds — returns a new cursor at the requested position
7. **[P2]** Cursor.advance_by past EOF — returns None
8. **[P2]** Cursor.take_while on prefix digits — returns the prefix and an updated cursor after it
9. **[P2]** Cursor.skip_while on prefix spaces — returns a cursor advanced past all matching bytes
10. **[P1]** Cursor.take_until delimiter present — returns substring before delimiter and cursor positioned at delimiter
11. **[P1]** Cursor.take_n / remaining — return exact slice and remaining suffix

## Iter.Iterator

1. **[P2]** Iterator.make over finite source then to_list — produces the full sequence in order
2. **[P2]** Iterator.next on exhausted iterator — returns None and keeps iterator exhausted
3. **[P2]** Iterator.size on fresh finite iterator — equals remaining item count
4. **[P2]** Iterator.map over [1;2;3] — yields transformed values in the same order
5. **[P2]** Iterator.filter even over [1;2;3;4] — yields [2;4]
6. **[P2]** Iterator.filter_map mixed Some/None — drops None results and unwraps Some results
7. **[P1]** Iterator.fold with left-to-right subtraction — uses the documented fold argument order
8. **[P2]** Iterator.reduce on non-empty iterator — returns Some accumulated value
9. **[P2]** Iterator.reduce on empty iterator — returns None
10. **[P2]** Iterator.count after filter — returns the number of surviving elements
11. **[P2]** Iterator.find with first matching element — returns the first match only
12. **[P2]** Iterator.any with one matching element — returns true
13. **[P2]** Iterator.all with one failing element — returns false
14. **[P1]** Iterator.take/drop/enumerate/zip/chain composition — preserves expected lazy ordering across composed iterators

## Iter.MutCursor

1. **[P2]** MutCursor.create source/position/length_remaining — start at position 0 with full length remaining
2. **[P2]** MutCursor.peek on fresh cursor — returns first character without advancing
3. **[P2]** MutCursor.advance on non-EOF — mutates position by +1
4. **[P2]** MutCursor.advance at EOF — leaves position unchanged
5. **[P2]** MutCursor.advance_by within bounds — mutates position to the requested offset
6. **[P2]** MutCursor.advance_by past EOF — leaves position unchanged
7. **[P2]** MutCursor.take_while on prefix letters — returns prefix and mutates cursor past it
8. **[P2]** MutCursor.skip_while on spaces — mutates cursor past all matching bytes
9. **[P1]** MutCursor.take_until delimiter present — returns substring before delimiter and leaves cursor at delimiter
10. **[P1]** MutCursor.take_until delimiter absent — returns None and restores the original position
11. **[P1]** MutCursor.take_n / remaining — consume exact slice and expose remaining suffix

## Iter.MutIterator

1. **[P2]** MutIterator.empty |> to_list — returns []
2. **[P2]** MutIterator.singleton x then next twice — returns Some x then None
3. **[P1]** MutIterator.clone before consumption — gives an independent iterator state
4. **[P2]** MutIterator.map over [1;2;3] — yields transformed values in order
5. **[P2]** MutIterator.filter even over [1;2;3;4] — yields [2;4]
6. **[P2]** MutIterator.filter_map mixed Some/None — drops None results and unwraps Some results
7. **[P1]** MutIterator.flat_map on nested iterators — concatenates inner iterators in outer order
8. **[P1]** MutIterator.fold with subtraction — uses the documented fold argument order
9. **[P2]** MutIterator.reduce on empty — returns None
10. **[P2]** MutIterator.find first matching — returns first match and advances underlying iterator appropriately
11. **[P2]** MutIterator.take 2 then to_list — returns exactly two items
12. **[P2]** MutIterator.drop 2 then to_list — skips exactly two items
13. **[P1]** MutIterator.enumerate then zip with another iterator — pairs indexes/items correctly until the shorter side ends
14. **[P1]** MutIterator.chain first second — yields all of first, then all of second

## Log

1. **[P1]** Log.get_level default — returns Info unless explicitly changed by earlier test setup
2. **[P1]** Log.set_level then Log.get_level — returns the newly configured level
3. **[P1]** Log.attach then Log.list_handlers — includes the handler ID
4. **[P1]** Log.attach with an existing handler ID — replaces the old handler rather than registering a duplicate
5. **[P1]** Log.detach removes one handler ID — that ID no longer appears in Log.list_handlers
6. **[P1]** Log.detach_all — removes every registered handler
7. **[P1]** A handler attached at Info level receives Log.info events — is invoked exactly once per event
8. **[P1]** A handler does not receive events below the configured minimum level — e.g. Debug is filtered out when level is Info
9. **[P1]** A handler still receives more severe events above the configured minimum level — e.g. Error passes through when level is Warn
10. **[P1]** Multiple handlers attached — all are invoked for a matching event
11. **[P1]** A handler that raises an exception — does not prevent other handlers from running
12. **[P1]** Log.Metadata.merge — uses right-hand non-None fields and concatenates custom key/value pairs

## Net.Addr

1. **[P1]** Addr.loopback |> Addr.tcp 8080 — creates a stream address whose port is 8080
2. **[P1]** Addr.loopback |> Addr.udp 5353 — creates a datagram address whose port is 5353
3. **[P1]** Addr.of_host_and_port ~host:'127.0.0.1' ~port:8080 — returns Ok address with ip '127.0.0.1' and port 8080
4. **[P1]** Addr.of_host_and_port with an invalid port — returns Error (Invalid_port_number ...) or a system error consistently
5. **[P1]** Addr.parse '127.0.0.1:8080' — returns Ok address with host 127.0.0.1 and port 8080
6. **[P1]** Addr.parse 'localhost:80' — resolves and returns Ok address
7. **[P1]** Addr.parse missing port text — returns Error (Invalid_format 'missing port')
8. **[P1]** Addr.parse non-numeric port — returns Error (Invalid_port_number ...)
9. **[P1]** Addr.parse IPv6 bracket form '[::1]:8080' — returns Ok address with port 8080
10. **[P1]** Addr.parse IPv6 without brackets '::1:8080' — either rejects or interprets consistently; the test should lock down the chosen behavior
11. **[P1]** Addr.parse_datagram '127.0.0.1:53' — returns Ok datagram address
12. **[P1]** Addr.ip on a parsed address — returns the resolved IP string
13. **[P1]** Addr.port on a parsed address — returns the parsed port number
14. **[P1]** Addr.parse_datagram missing port — returns Error (Invalid_format 'missing port')

## Net.Http.Header

1. **[P1]** Header.empty — is empty and has length 0
2. **[P1]** Header.of_list then Header.to_list — round-trips the supplied name/value pairs in stored order
3. **[P1]** Header.set on an empty header set — stores the header and makes it retrievable
4. **[P1]** Header.set twice for the same name with different casing — replaces previous values case-insensitively
5. **[P1]** Header.remove on an existing name — removes all values for that name case-insensitively
6. **[P1]** Header.get on a missing name — returns None
7. **[P1]** Header.get is case-insensitive — finds 'Content-Type' via 'content-type' and similar variants
8. **[P1]** Header.get_all on a missing name — returns []
9. **[P1]** Header.add twice for the same name — preserves both values
10. **[P1]** Header.get_all after two adds — returns both values in insertion order
11. **[P0]** Header.get after two adds for the same name — returns the first-added value as documented
12. **[P1]** Header.has is case-insensitive — returns true for any name casing when the header exists
13. **[P1]** Header.iter visits every stored entry — calls the callback once per header pair, including duplicates
14. **[P1]** Header.fold counts every stored entry — sees duplicates as separate entries
15. **[P1]** Header.length counts duplicates — returns 2 after adding two values for the same header name
16. **[P1]** Header.Name constants used with Header.set/get — behave the same as raw strings
17. **[P1]** Header.Value.parse_content_type on 'application/json; charset=utf-8' — returns media_type 'application/json' and parameter ('charset','utf-8')
18. **[P1]** Header.Value.parse_content_type on an invalid content type string — returns Error `InvalidContentType

## Net.Http.Method+Version+Status

1. **[P1]** Method.of_string 'GET' — returns Method.Get
2. **[P0]** Method.of_string 'get' — returns Method.Get because standard methods are documented as case-insensitive
3. **[P1]** Method.of_string 'PURGE' — returns Method.Extension 'PURGE'
4. **[P1]** Method.to_string Method.Patch — returns 'PATCH'
5. **[P1]** Method.is_safe on GET/HEAD/OPTIONS/TRACE — returns true for each
6. **[P1]** Method.is_safe on POST — returns false
7. **[P1]** Method.is_idempotent on PUT and DELETE — returns true
8. **[P1]** Method.is_idempotent on POST — returns false
9. **[P1]** Method.is_cacheable on GET and HEAD — returns true
10. **[P1]** Method.is_cacheable on DELETE — returns false
11. **[P1]** Method.compare/equal on representative standard methods — obey a stable total order and equality law
12. **[P1]** Version.of_string 'HTTP/1.1' — returns Ok Http11
13. **[P1]** Version.of_string 'HTTP/2.0' — returns Ok Http2
14. **[P1]** Version.of_string invalid text — returns Error `InvalidVersion
15. **[P1]** Version.to_string Http3 — returns 'HTTP/3'
16. **[P1]** Version.compare Http10 Http11 — is < 0
17. **[P1]** Version.equal on identical versions — returns true
18. **[P1]** Version.is_supported Http11 — returns true
19. **[P1]** Version.is_supported Http2 or Http3 in the current implementation — returns false unless support is explicitly added
20. **[P1]** Status.of_int 200 — returns Status.Ok
21. **[P1]** Status.of_int 999 — returns Status.Extension 999
22. **[P1]** Status.of_string '404' — returns Ok Status.NotFound
23. **[P1]** Status.of_string 'abc' — returns Error `InvalidStatus
24. **[P1]** Status.to_int / to_string / reason_phrase for representative codes — match the standard code and phrase
25. **[P1]** Status.is_success on 2xx — returns true
26. **[P1]** Status.is_redirection on 3xx — returns true
27. **[P1]** Status.is_client_error on 4xx — returns true
28. **[P1]** Status.is_server_error on 5xx — returns true

## Net.Http.Request+Response

1. **[P1]** Request.create Method.Get uri — creates a request with method GET, the given URI, HTTP/1.1, empty headers, and no body
2. **[P1]** Request.with_method — returns a new request with only the method changed
3. **[P1]** Request.with_uri — returns a new request with only the URI changed
4. **[P1]** Request.with_version — returns a new request with only the HTTP version changed
5. **[P1]** Request.with_headers — replaces all headers at once
6. **[P1]** Request.with_header twice for the same name — keeps only the most recent value for that header
7. **[P1]** Request.add_header twice for the same name — preserves both values
8. **[P1]** Request.remove_header — removes all values for that header
9. **[P1]** Request.with_body then Request.body — returns Some body_text
10. **[P1]** Request.without_body after setting a body — returns None for the body
11. **[P1]** Request.get_header and has_header — delegate to Header semantics and are case-insensitive
12. **[P1]** Request.Builder.create ... |> build — produces the same request as the equivalent functional update chain
13. **[P1]** Request.Builder.header called twice with same name — behaves like Request.with_header and replaces the earlier value
14. **[P1]** Request.get/head/delete/options convenience constructors — set the expected method and no body
15. **[P1]** Request.post/put/patch convenience constructors — set the expected method and Some body
16. **[P1]** Response.create Status.Ok — creates a response with status 200, HTTP/1.1, empty headers, and no body
17. **[P1]** Response.with_status / with_version — return new responses with only the targeted field changed
18. **[P1]** Response.with_headers — replaces all headers at once
19. **[P1]** Response.with_header twice for the same name — keeps only the most recent value
20. **[P1]** Response.add_header twice for the same name — preserves both values
21. **[P1]** Response.with_body then Response.body — returns Some body_text
22. **[P1]** Response.without_body after setting a body — returns None for the body
23. **[P1]** Response.Builder.create ... |> build — matches the equivalent functional update chain
24. **[P1]** Response.ok / created / accepted / no_content / not_found — use the expected status codes and body presence

## Option

1. **[P2]** Option.some 42 — equals Some 42
2. **[P2]** Option.none — equals None
3. **[P2]** Option.equal on Some/Some with equal payloads — returns true
4. **[P2]** Option.equal on Some/None — returns false
5. **[P2]** Option.is_some (Some x) — returns true
6. **[P2]** Option.is_none None — returns true
7. **[P2]** Option.is_some_and predicate on matching Some — returns true
8. **[P2]** Option.is_some_and predicate on None — returns false
9. **[P2]** Option.is_none_or predicate on None — returns true
10. **[P2]** Option.is_none_or predicate on failing Some — returns false
11. **[P2]** Option.map over Some — applies the function and wraps in Some
12. **[P1]** Option.map over None — returns None without calling the function
13. **[P2]** Option.map_or over Some — returns mapped value, not the default
14. **[P2]** Option.map_or over None — returns the eager default
15. **[P1]** Option.map_or_else over Some — does not call the default thunk
16. **[P1]** Option.map_or_else over None — calls the default thunk exactly once
17. **[P2]** Option.and_ Some x Some y — returns Some y
18. **[P2]** Option.and_ None Some y — returns None
19. **[P2]** Option.and_then Some x with successful callback — returns callback result
20. **[P1]** Option.and_then None — returns None without calling callback
21. **[P2]** Option.or_ Some x Some y — returns the first Some
22. **[P1]** Option.or_else Some x — does not call fallback thunk
23. **[P1]** Option.or_else None — calls fallback thunk and returns its result
24. **[P2]** Option.xor Some x None — returns Some x
25. **[P2]** Option.xor None Some y — returns Some y
26. **[P2]** Option.xor Some x Some y — returns None
27. **[P2]** Option.xor None None — returns None
28. **[P2]** Option.unwrap Some x — returns x

## Path

1. **[P1]** Path.from_string on valid ASCII relative path — returns Ok path and Path.to_string round-trips the exact input
2. **[P1]** Path.from_string on valid UTF-8 path containing non-ASCII characters — returns Ok path and preserves the UTF-8 text unchanged
3. **[P1]** Path.from_string on invalid UTF-8 bytes — returns Error (InvalidUtf8 ...)
4. **[P1]** Path.v on valid literal path — constructs the path without error
5. **[P1]** Path.join with relative child — inserts exactly one separator between base and child
6. **[P1]** Path.join with empty base — returns the child path unchanged
7. **[P1]** Path.join where base already ends with slash — does not duplicate the separator
8. **[P1]** Path.join with absolute second path — returns the second path unchanged
9. **[P1]** Path.( / ) chained three times — produces the same result as nested Path.join calls
10. **[P1]** Path.parent of absolute file path — returns Some parent directory
11. **[P1]** Path.parent of root path — returns None
12. **[P1]** Path.parent of single relative component — returns None
13. **[P1]** Path.parent of '.' — returns None
14. **[P1]** Path.basename of simple file path — returns the final path component
15. **[P1]** Path.basename of path ending with slash — returns the last non-empty component, e.g. '/home/user/' -> 'user'
16. **[P1]** Path.basename of root '/' — returns empty string
17. **[P1]** Path.dirname of file path — returns the directory portion without the basename
18. **[P1]** Path.dirname of directory path ending in slash — returns the parent directory of that directory
19. **[P1]** Path.dirname of root — returns root
20. **[P1]** Path.extension of 'file.txt' — returns Some 'txt'
21. **[P1]** Path.extension of 'archive.tar.gz' — returns Some 'gz'
22. **[P1]** Path.extension of 'README' — returns None
23. **[P1]** Path.extension of '.gitignore' — returns None
24. **[P1]** Path.extension of 'file.' — returns Some ''
25. **[P1]** Path.remove_extension on file with one extension — drops only the final extension
26. **[P1]** Path.remove_extension on file with multiple extensions — drops only the last extension
27. **[P1]** Path.remove_extension on file with no extension — returns the original path unchanged
28. **[P1]** Path.add_extension with ext lacking leading dot — adds a dot before the extension
29. **[P1]** Path.add_extension with ext already containing dot — does not add a second dot
30. **[P1]** Path.replace_extension on file with existing extension — replaces only the final extension
31. **[P1]** Path.replace_extension on file without extension — appends the new extension
32. **[P1]** Path.is_absolute on '/a/b' — returns true
33. **[P1]** Path.is_relative on 'a/b' — returns true
34. **[P1]** Path.components on relative path 'a/b/c' — returns ['a'; 'b'; 'c'] as path components
35. **[P1]** Path.components on absolute path '/usr/local/bin' — returns ['/'; 'usr'; 'local'; 'bin']
36. **[P1]** Path.components preserves '.' and '..' components — includes '.' and '..' in the returned list instead of normalizing
37. **[P1]** Path.normalize removes '.' segments — returns a path without '.' components
38. **[P1]** Path.normalize resolves '..' within a relative path — collapses parent traversals when possible
39. **[P1]** Path.normalize on '/../..' — does not climb above root and returns '/'
40. **[P1]** Path.normalize on './a/b/../c/.'' — returns 'a/c'

## Random

1. **[P1]** Random.init ~seed:'abc' twice with sampling from the default RNG — produces the same sequence both times
2. **[P1]** Random.Rng.standard ~seed:'abc' twice — creates deterministic RNGs that yield identical sample sequences
3. **[P1]** Random.sample with an explicit seeded RNG — does not depend on the default global RNG
4. **[P1]** Random.bits returns a non-negative int — value is >= 0
5. **[P1]** Random.bits32 returns an int32 — successfully returns a 32-bit sample
6. **[P1]** Random.bits64 returns an int64 — successfully returns a 64-bit sample
7. **[P1]** Random.bool sampled many times from a seeded RNG — returns only true or false and is deterministic for the same seed
8. **[P1]** Random.char sampled many times — always returns a valid char value
9. **[P1]** Random.int with bound 1 — always returns 0
10. **[P1]** Random.int with positive bound n — always returns a value in [0, n)
11. **[P1]** Random.int with bound 0 — returns Error (InvalidIntBound { bound = 0 })
12. **[P1]** Random.int with negative bound — returns Error (InvalidIntBound ...)
13. **[P1]** Random.int_range with min=max — always returns that exact value
14. **[P1]** Random.int_range with min<max — always returns a value in the inclusive range [min, max]
15. **[P1]** Random.int_range with min>max — returns Error (InvalidIntRange ...)
16. **[P1]** Random.int32 with bound 1l — always returns 0l
17. **[P1]** Random.int32 with invalid bound 0l — returns Error (InvalidInt32Bound ...)
18. **[P1]** Random.int32_range with equal bounds — returns the common bound
19. **[P1]** Random.int32_range with min>max — returns Error (InvalidInt32Range ...)
20. **[P1]** Random.int64 with bound 1L — always returns 0L
21. **[P1]** Random.int64 with invalid bound 0L — returns Error (InvalidInt64Bound ...)
22. **[P1]** Random.int64_range with equal bounds — returns the common bound
23. **[P1]** Random.int64_range with min>max — returns Error (InvalidInt64Range ...)
24. **[P1]** Random.float with positive bound — always returns a float in [0.0, bound) or very close to the upper edge by floating-point rounding
25. **[P1]** Random.float with bound 0.0 — always returns 0.0
26. **[P1]** Random.float_range with equal bounds — always returns that exact bound
27. **[P1]** Random.float_range with min<max — always returns a float in [min, max) or within the documented interpretation
28. **[P1]** Random.float_range with min>max — returns Error (InvalidFloatRange ...)
29. **[P1]** Random.Distribution.bernoulli ~p:0.0 — always returns false
30. **[P1]** Random.Distribution.bernoulli ~p:1.0 — always returns true
31. **[P1]** Random.Distribution.bernoulli with p outside [0,1] — returns Error (InvalidProbability ...)
32. **[P1]** Random.one_of on an empty list — returns Error EmptyPopulation
33. **[P1]** Random.one_of on a singleton list — always returns the only element
34. **[P1]** Random.choose_n with count 0 — returns Ok []
35. **[P1]** Random.choose_n with count equal to population size — returns all original elements exactly once, in a sampled order
36. **[P1]** Random.choose_n with count greater than population size — returns Error (InvalidSampleSize ...)

## Regex

1. **[P1]** Regex.literal on text containing metacharacters like '.+*?[](){}|^$\\' — escapes every metacharacter in the rendered pattern
2. **[P1]** Regex.char_class [Single 'a'; Single '-'; Single ']'] — escapes class metacharacters correctly in the rendered pattern
3. **[P1]** Regex.char_class ~negated:true [Range ('a','z')] — renders a negated character class
4. **[P1]** Regex.seq [] — optimizes to Empty and renders as an empty pattern
5. **[P1]** Regex.seq [Regex.literal 'a'; Regex.literal 'b'] — optimizes to a single literal 'ab'
6. **[P1]** Regex.alt [] — optimizes to Empty
7. **[P1]** Regex.alt [single_item] — optimizes to the single item without an alternation wrapper
8. **[P1]** Regex.repeat ~min:1 ~max:(Some 1) expr — optimizes back to expr
9. **[P1]** Regex.optional expr — renders a '?' quantifier around expr
10. **[P1]** Regex.zero_or_more expr — renders a '*' quantifier
11. **[P1]** Regex.one_or_more expr — renders a '+' quantifier
12. **[P1]** Regex.repeat on a non-atomic alternation — wraps the inner expression in a noncapturing group before applying the quantifier
13. **[P1]** Regex.to_string on nested seq/alt/repeat — produces a stable concrete regex string
14. **[P1]** Regex.compile of a valid AST — returns Ok compiled regex
15. **[P1]** Regex.from_string on invalid raw pattern — returns Error compile_error
16. **[P1]** Regex.source after Regex.compile — returns the concrete pattern string used for compilation
17. **[P1]** Regex.is_match on a compiled literal — returns true only when the haystack contains that literal
18. **[P1]** Regex.find on a haystack with a match — returns the start and stop offsets of the first match
19. **[P1]** Regex.start_of_text combined with end_of_text around a literal — matches only the whole string
20. **[P1]** Regex.any_char followed by one_or_more digit class — matches representative strings and rejects non-matching ones

## Result

1. **[P2]** Result.ok 42 — equals Ok 42
2. **[P2]** Result.err "boom" — equals Error "boom"
3. **[P2]** Result.is_ok (Ok x) — returns true
4. **[P2]** Result.is_err (Error e) — returns true
5. **[P2]** Result.is_error alias on Error — returns true
6. **[P2]** Result.is_ok_and on matching Ok — returns true
7. **[P1]** Result.is_ok_and on Error — returns false without calling predicate
8. **[P2]** Result.is_err_and on matching Error — returns true
9. **[P2]** Result.map over Ok — maps the success value
10. **[P1]** Result.map over Error — returns original Error unchanged
11. **[P2]** Result.map_err over Error — maps the error value
12. **[P1]** Result.map_err over Ok — returns original Ok unchanged
13. **[P2]** Result.map_or over Ok — returns mapped value
14. **[P2]** Result.map_or over Error — returns default
15. **[P1]** Result.map_or_else over Ok — uses ok callback only
16. **[P1]** Result.map_or_else over Error — uses error callback only
17. **[P2]** Result.and_then Ok -> Ok — returns downstream Ok
18. **[P2]** Result.and_then Ok -> Error — propagates downstream Error
19. **[P1]** Result.and_then Error — returns original Error without calling callback
20. **[P2]** Result.or_ Ok x Error e — keeps the first Ok
21. **[P2]** Result.or_ Error e Ok x — returns fallback Ok
22. **[P1]** Result.or_else Error e — calls fallback callback with e
23. **[P2]** Result.unwrap Ok x — returns x
24. **[P2]** Result.unwrap_or Error e — returns provided default
25. **[P1]** Result.unwrap_or_else Error e — computes default lazily
26. **[P2]** Result.unwrap_err Error e — returns e
27. **[P2]** Result.ok_value (Ok x) — returns Some x
28. **[P2]** Result.err_value (Error e) — returns Some e
29. **[P2]** Result.to_option (Ok x) — returns Some x
30. **[P2]** Result.from_option ~error:e None — returns Error e

## String

1. **[P2]** String.empty — equals ""
2. **[P2]** String.is_empty "" — returns true
3. **[P2]** String.is_empty "a" — returns false
4. **[P2]** String.length "abc" — returns 3
5. **[P2]** String.get "abc" ~at:1 — returns Some 'b'
6. **[P2]** String.get out of bounds — returns None
7. **[P2]** String.get_unchecked "abc" ~at:2 — returns 'c'
8. **[P2]** String.sub "abcdef" ~offset:2 ~len:3 — returns "cde"
9. **[P2]** String.init ~len:4 with digits — builds the expected 4-character string
10. **[P2]** String.make ~len:5 ~char:'x' — returns "xxxxx"
11. **[P2]** String.append "foo" "bar" — returns "foobar"
12. **[P2]** String.concat "," ["a";"b";"c"] — returns "a,b,c"
13. **[P2]** String.contains "hello world" "world" — returns true
14. **[P2]** String.contains "hello" "" — returns true
15. **[P2]** String.contains "hello" "bye" — returns false
16. **[P2]** String.starts_with ~prefix:"pre" "prefix" — returns true
17. **[P2]** String.starts_with ~prefix:"" "prefix" — returns true
18. **[P2]** String.ends_with ~suffix:"fix" "prefix" — returns true
19. **[P2]** String.ends_with ~suffix:"" "prefix" — returns true
20. **[P2]** String.equal on equal strings — returns true
21. **[P2]** String.compare "abc" "abd" — is negative
22. **[P2]** String.index_of "banana" ~char:'a' — returns first matching index
23. **[P2]** String.index_of missing char — returns None
24. **[P2]** String.last_index "banana" 'a' — returns last matching index
25. **[P2]** String.trim on leading/trailing ASCII spaces — strips both ends
26. **[P2]** String.split ~by:"," "a,b,c" — returns ["a"; "b"; "c"]
27. **[P2]** String.split with separator not present — returns the original string as a singleton list
28. **[P2]** String.lowercase_ascii "AbC!" — returns "abc!"
29. **[P2]** String.uppercase_ascii "AbC!" — returns "ABC!"
30. **[P2]** String.capitalize_ascii "hello" — returns "Hello"
31. **[P2]** String.map over bytes — applies mapping to each byte in order
32. **[P2]** String.exists predicate over matching string — returns true
33. **[P2]** String.for_all predicate over all-matching string — returns true
34. **[P2]** String.fold_left over "abc" — can reconstruct or aggregate left-to-right
35. **[P2]** String.escaped on quotes and backslashes — escapes them according to OCaml string rules
36. **[P2]** String.from_bytes then String.to_bytes — roundtrips content exactly
37. **[P1]** String.unsafe_from_bytes shares underlying bytes — mutating the original bytes is reflected in the string view
38. **[P1]** String.get_utf_8_rune at ASCII start — returns a one-byte rune decode
39. **[P1]** String.get_utf_8_rune at multibyte rune start — returns the decoded rune and consumed width
40. **[P1]** String.into_iter over "café" — yields 4 runes, not 5 bytes
41. **[P1]** String.into_mut_iter clone-like consumption — consumes runes in order until EOF
42. **[P1]** String.width "你好" — returns 4
43. **[P1]** String.rune_count "👨‍👩‍👧‍👦" — returns 7 code points
44. **[P1]** String.grapheme_count "👨‍👩‍👧‍👦" — returns 1 grapheme cluster
45. **[P1]** String.truncate_width on already-short text — returns the original string unchanged
46. **[P1]** String.truncate_width with custom tail — never exceeds the requested display width
47. **[P2]** String.pad_left to wider width — adds only enough padding to reach the width
48. **[P2]** String.pad_right to wider width — adds padding on the right only
49. **[P2]** String.pad_center with odd padding count — places the extra pad character on the right
50. **[P1]** String.to_reader with ~chunk_size:2 over "abcdef" — produces a reader that yields the full content in small chunks

## Supervisor

1. **[P1]** Supervisor.child_spec with defaults — uses restart=Permanent, a finite timeout shutdown, child_type=Worker, and significant=false
2. **[P1]** Supervisor.start_link with no children — starts successfully and which_children returns []
3. **[P1]** Supervisor.which_children after starting with two workers — returns both child IDs with Some pid values
4. **[P1]** Supervisor.count_children on a mixed worker/supervisor set — reports correct specs/active/workers/supervisors counts
5. **[P0]** Supervisor.terminate_child on a running child — stops that child and leaves the spec present
6. **[P1]** Supervisor.restart_child after terminate_child — starts a fresh child and returns a new PID
7. **[P1]** Supervisor.delete_child on a stopped child — removes the spec and later which_children omits it
8. **[P1]** Supervisor.delete_child on a running child — returns Error because the child is still running
9. **[P0]** Supervisor OneForOne strategy — restarts only the failed child
10. **[P0]** Supervisor OneForAll strategy — restarts all children when one child fails
11. **[P0]** Supervisor RestForOne strategy — restarts the failed child and all children started after it
12. **[P1]** Supervisor significant:true child termination — causes the supervisor itself to terminate as documented

## Sync

1. **[P1]** Sync.Atomic.make then get — returns the initial value
2. **[P1]** Sync.Atomic.set then get — returns the new value
3. **[P1]** Sync.Atomic.exchange — returns the old value and stores the new one
4. **[P1]** Sync.Atomic.compare_and_set success case — returns true and updates the value
5. **[P1]** Sync.Atomic.compare_and_set failure case — returns false and leaves the value unchanged
6. **[P1]** Sync.Atomic.fetch_and_add — returns the previous value and increments atomically
7. **[P1]** Sync.Cell.create then get — returns the initial value
8. **[P1]** Sync.Cell.! operator — returns the same value as Cell.get
9. **[P1]** Sync.Cell.set / := operator — mutate the stored value
10. **[P1]** Sync.Cell.update — applies the update function to the current value
11. **[P1]** Sync.Cell.incr on int cell — increments by 1
12. **[P1]** Sync.Cell.decr on int cell — decrements by 1
13. **[P1]** Sync.Cell.replace — returns the old value and stores the new one
14. **[P1]** Sync.Cell.take ~default — returns the old value and replaces it with the default
15. **[P1]** Sync.Cell.swap — exchanges the contents of two cells
16. **[P1]** Sync.Cell.compare_and_swap success case — returns true and swaps in the new value
17. **[P1]** Sync.Cell.compare_and_swap failure case — returns false and leaves the old value intact
18. **[P1]** Sync.Cell.equal on two cells containing equal values — returns true
19. **[P1]** Sync.OnceCell.create then get — returns None
20. **[P1]** Sync.OnceCell.set first time — returns Ok () and initializes the cell
21. **[P1]** Sync.OnceCell.set second time — returns Error `AlreadyInitialized
22. **[P1]** Sync.OnceCell.is_initialized before and after set — transitions from false to true
23. **[P1]** Sync.OnceCell.get_or_init on an empty cell — runs the initializer exactly once and stores the value
24. **[P1]** Sync.OnceCell.get_or_init on an already initialized cell — does not rerun the initializer
25. **[P1]** Sync.OnceCell.get_or_try_init when initializer returns Error — returns the same Error and leaves the cell uninitialized
26. **[P1]** Sync.OnceCell.get_or_try_init after a previous Error — can still initialize successfully on a later successful call
27. **[P1]** Sync.OnceCell.take on an initialized cell — returns Some value and leaves the cell uninitialized again
28. **[P1]** Sync.LazyCell.create then is_initialized — starts false
29. **[P1]** Sync.LazyCell.get first call — runs the initializer and stores the result
30. **[P1]** Sync.LazyCell.get second call — returns the cached result without rerunning the initializer
31. **[P1]** Sync.LazyCell.take after initialization — returns Some cached value and resets initialization state
32. **[P1]** Sync.LazyCell.get after take — reruns the initializer and caches a fresh value
33. **[P1]** Sync.RefCell.create then is_borrowed — starts unborrowed with count 0
34. **[P1]** Sync.RefCell.borrow once — succeeds, marks the cell borrowed, and sets borrow_count to 1
35. **[P1]** Sync.RefCell.borrow twice concurrently — allows shared borrows and increments borrow_count to 2
36. **[P1]** Sync.RefCell.borrow_mut while shared borrowed — raises BorrowMutError or returns Error via try_borrow_mut
37. **[P1]** Sync.RefCell.borrow while mutably borrowed — raises BorrowError or returns Error via try_borrow
38. **[P1]** Sync.RefCell.borrow_mut then get_mut/set_mut — allows reading and mutating the underlying value
39. **[P1]** Sync.RefCell.release_borrow / release_borrow_mut — restores the cell to an available state
40. **[P1]** Sync.RefCell.with_borrow and with_borrow_mut — automatically acquire and release borrows around the callback

## System

1. **[P1]** System.Host.to_string followed by System.Host.from_string — round-trips the current host triplet exactly
2. **[P1]** System.Host.from_string on malformed triplet — returns Error
3. **[P1]** System.Host.equal on identical parsed hosts — returns true
4. **[P1]** System.OS.to_string current matches legacy os_type string — is consistent with System.os_type
5. **[P1]** System.OS.is_unix / is_win32 / is_cygwin — exactly one reflects the current platform

## Task

1. **[P1]** Task.async of a pure function then Task.await — returns Ok result
2. **[P1]** Task.async of a function that raises — Task.await returns Error exn
3. **[P1]** Task.await on the same completed task more than once — either returns the same stored result or the contract should explicitly reject double-await; the test should lock down intended semantics
4. **[P1]** Task.await_all on a singleton task list — returns a singleton result list
5. **[P0]** Task.await_all on an empty task list — returns [] immediately
6. **[P1]** Task.await_all with multiple successful tasks — returns one Ok result per task
7. **[P1]** Task.await_all with mixed success and exception — preserves both Ok and Error results
8. **[P0]** Task.await_all preserves input order even when tasks complete out of order — results appear in the same order as the input list
9. **[P1]** Task.await_all does not lose messages from unrelated tasks — ignores non-matching messages and still returns the correct results for the awaited tasks
10. **[P1]** Task.async starts eagerly — the task begins executing before Task.await is called

## Telemetry+Test+Bench

1. **[P1]** Telemetry.start called twice — returns the same server PID both times
2. **[P1]** Telemetry.attach then list_handlers — includes the handler name
3. **[P1]** Telemetry.attach with an existing handler name — replaces the old handler rather than duplicating it
4. **[P1]** Telemetry.detach removes one handler — that name disappears from list_handlers
5. **[P1]** Telemetry.detach_all — removes every attached handler
6. **[P1]** Telemetry.emit before Telemetry.start — is a no-op and does not raise
7. **[P1]** Telemetry handler that raises — does not prevent other handlers from receiving the same event
8. **[P1]** Telemetry.stop is safe to call multiple times — does not raise and leaves the system stopped
9. **[P1]** Test.case produces a UnitTest test_case — the runner reports it as a normal unit test
10. **[P1]** Test.property with ~examples:1000 — records property metadata with the requested example count
11. **[P1]** Test.skip — is reported as skipped and does not execute the body
12. **[P1]** Test.todo — is reported as todo/skipped placeholder without a body
13. **[P1]** Bench.make_case / Bench.compare — build a comparison benchmark containing the supplied cases
14. **[P1]** Bench.with_config — stores the provided iterations and warmup values
15. **[P1]** Bench_result.make_statistics on known timings — computes min/max/mean/median/iterations/total_time consistently
16. **[P1]** Bench_result.make_summary on mixed completed/failed/skipped results — counts each category correctly

## Time.Duration

1. **[P1]** Duration.zero — is zero and converts to 0 seconds
2. **[P1]** Duration.make ~secs:1 ~nanos:500_000_000 — represents 1.5 seconds
3. **[P0]** Duration.make with nanos >= 1_000_000_000 — normalizes excess nanoseconds into the seconds component
4. **[P0]** Duration.make with negative nanos — normalizes by borrowing from the seconds component so the stored nanos are non-negative
5. **[P1]** Duration.from_days 7 — converts to 604800 seconds
6. **[P1]** Duration.from_hours 2 — converts to 7200 seconds
7. **[P1]** Duration.from_mins 30 — converts to 1800 seconds
8. **[P1]** Duration.from_secs 60 — converts to 60 seconds
9. **[P1]** Duration.from_millis 1500 — converts to 1.5 seconds
10. **[P1]** Duration.from_micros 1_000_000 — converts to exactly 1 second
11. **[P1]** Duration.from_nanos 1_000_000_001 — converts to 1 second and 1 nanosecond
12. **[P1]** Duration.from_weeks 2 — converts to 14 days
13. **[P1]** Duration.from_secs_float 1.25 — converts to 1 second and 250_000_000 nanoseconds
14. **[P1]** Duration.to_secs on a fractional duration — returns only the whole-second component
15. **[P1]** Duration.to_secs_float on a fractional duration — preserves the fractional portion
16. **[P1]** Duration.to_secs_string default precision — rounds and formats with 2 decimal places
17. **[P1]** Duration.to_secs_string ~precision:0 — returns just the rounded integral seconds string
18. **[P1]** Duration.to_millis on 1.5 seconds — returns 1500
19. **[P1]** Duration.to_micros on 1.5 seconds — returns 1_500_000
20. **[P1]** Duration.to_nanos on 1.5 seconds — returns 1_500_000_000L
21. **[P1]** Duration.subsec_millis on nanos 123_456_789 — returns 123
22. **[P1]** Duration.subsec_micros on nanos 123_456_789 — returns 123456
23. **[P1]** Duration.subsec_nanos on nanos 123_456_789 — returns 123_456_789
24. **[P1]** Duration.is_zero on Duration.zero — returns true
25. **[P1]** Duration.add 5s and 500ms — returns 5.5s
26. **[P1]** Duration.sub 10s and 3s — returns 7s
27. **[P0]** Duration.sub where rhs > lhs — raises or otherwise fails for a negative result per the documented contract
28. **[P1]** Duration.mul 5s by 3 — returns 15s
29. **[P1]** Duration.mul by 0 — returns zero
30. **[P1]** Duration.div 10s by 2 — returns 5s
31. **[P1]** Duration.div by 0 — raises or panics
32. **[P1]** Duration.checked_add near max_duration overflow boundary — returns None on overflow
33. **[P1]** Duration.checked_sub with a>b — returns Some positive difference
34. **[P1]** Duration.checked_sub with a<b — returns None
35. **[P1]** Duration.checked_mul with negative factor — returns None
36. **[P1]** Duration.checked_div by 0 — returns None
37. **[P1]** Duration.saturating_add overflowing max_duration — returns max_duration
38. **[P1]** Duration.saturating_sub where rhs > lhs — returns zero
39. **[P1]** Duration.saturating_mul overflowing max_duration — returns max_duration
40. **[P1]** Duration.mul_f64 10s by 1.5 — returns approximately 15s
41. **[P1]** Duration.div_f64 10s by 2.5 — returns approximately 4s
42. **[P1]** Duration.abs_diff is symmetric — gives the same positive duration regardless of argument order
43. **[P1]** Duration.min between 5s and 10s — returns 5s
44. **[P1]** Duration.max between 5s and 10s — returns 10s
45. **[P1]** Duration.compare/equal on 5s and 5000ms — compare returns 0 and equal returns true

## Time.Instant+SystemTime

1. **[P1]** SystemTime.epoch — has 0 seconds, 0 nanoseconds, and Unix timestamp 0
2. **[P1]** SystemTime.from_seconds 1.5 — represents exactly 1.5 seconds, i.e. 1 whole second and 500_000_000 nanoseconds
3. **[P1]** SystemTime.from_nanos 1_500_000_000L — represents 1.5 seconds exactly
4. **[P1]** SystemTime.secs / secs_float / nanos on a value from from_nanos — agree with each other numerically
5. **[P1]** SystemTime.now () compared to itself later — is typically nondecreasing under compare
6. **[P1]** SystemTime.duration_since ~earlier:t t — returns zero duration
7. **[P1]** SystemTime.elapsed for a freshly captured time — returns a small non-negative duration
8. **[P1]** SystemTime.add then duration_since — adding a duration and subtracting from the result recovers that duration
9. **[P1]** SystemTime.sub is inverse of add for a representable duration — SystemTime.add (SystemTime.sub t d) d equals t
10. **[P1]** SystemTime.checked_add on a representable value — returns Some future time
11. **[P1]** SystemTime.checked_sub on a value smaller than the duration since epoch — returns None
12. **[P1]** SystemTime.compare / equal / min / max — obey standard ordering laws on representative values
13. **[P1]** SystemTime.to_unix_timestamp on a value with fractional nanoseconds — drops the fractional part and returns whole seconds
14. **[P1]** SystemTime.duration_since_epoch () — returns a non-negative duration
15. **[P1]** Instant.now () compared to itself later — is nondecreasing under compare
16. **[P1]** Instant.duration_since ~earlier:t t — returns zero duration
17. **[P1]** Instant.saturating_duration_since with earlier later swapped — returns zero instead of raising
18. **[P1]** Instant.elapsed for a freshly captured instant — returns a small non-negative duration
19. **[P1]** Instant.add then Instant.duration_since — recovers the original duration
20. **[P1]** Instant.checked_sub underflow — returns None
21. **[P1]** Instant.compare / equal / min / max — obey standard ordering laws
22. **[P0]** Instant.duration_since with earlier > later — raises or panics per the documented contract

## UUID

1. **[P1]** UUID.of_string on lowercase canonical UUID — returns Ok uuid
2. **[P1]** UUID.of_string on uppercase canonical UUID — returns Ok uuid
3. **[P1]** UUID.of_string on malformed text — returns Error (`Invalid_uuid ...)
4. **[P1]** UUID.to_string after UUID.of_string — returns the canonical dashed lowercase representation by default
5. **[P1]** UUID.to_string ~upper:true — returns the canonical dashed uppercase representation
6. **[P1]** UUID.to_string_nodash on parsed UUID — returns 32 hex digits with no hyphens
7. **[P1]** UUID.to_string_nodash ~upper:true — returns uppercase hex with no hyphens
8. **[P1]** UUID.to_bytes after UUID.of_string — returns exactly 16 bytes
9. **[P1]** UUID.of_bytes on a 16-byte buffer — returns Ok uuid
10. **[P1]** UUID.of_bytes on a buffer with length != 16 — returns Error (`Invalid_uuid ...)
11. **[P1]** UUID.equal on the same parsed UUID — returns true
12. **[P1]** UUID.compare on equal UUID values — returns 0
13. **[P1]** UUID.nil is_nil — returns true
14. **[P1]** UUID.max is_nil — returns false
15. **[P1]** UUID.version UUID.nil — returns None or a non-version result because nil is not a valid versioned UUID
16. **[P1]** UUID.v4 () — returns a UUID whose version is Some 4
17. **[P1]** Two consecutive UUID.v4 () calls — almost certainly produce different values
18. **[P0]** UUID.v4_from_bytes with 16 input bytes — produces a UUID whose version/variant bits are set for v4
19. **[P1]** UUID.v4_from_bytes with non-16-byte input — raises Invalid_argument
20. **[P1]** UUID.v7 () — returns a UUID whose version is Some 7
21. **[P1]** Two UUID.v7 () values generated in order — compare in nondecreasing order in typical operation
22. **[P1]** UUID.v7_monotonic () called repeatedly — returns a strictly increasing sequence under UUID.compare
23. **[P0]** UUID.v7_from_parts with explicit timestamp/random parts — returns a UUID whose version is Some 7 and whose ordering follows time_ms
24. **[P0]** UUID.v5 with same namespace and same name — returns the same UUID every time
25. **[P0]** UUID.v5 with same namespace and different names — returns different UUIDs
26. **[P0]** UUID.v3 with same namespace and same name — returns the same UUID every time
27. **[P0]** UUID.v3 and UUID.v5 for the same namespace/name — return different UUID values because they use different hash algorithms
28. **[P1]** UUID.namespace constants parsed to string — match the standard RFC namespace UUID strings

## Version

1. **[P1]** Version.parse of '1.2.3' — returns Ok version with major=1 minor=2 patch=3 pre=[] build=None
2. **[P1]** Version.parse of '0.0.0' — returns Ok zero version
3. **[P1]** Version.parse of '1.2.3-alpha' — returns Ok with pre=[Alphanumeric 'alpha']
4. **[P1]** Version.parse of '1.2.3-alpha.1' — returns Ok with mixed alphanumeric and numeric pre-release segments
5. **[P1]** Version.parse of '1.2.3+build.5' — returns Ok with build=Some 'build.5'
6. **[P1]** Version.parse of '1.2.3-alpha+build.5' — returns Ok with both pre-release and build metadata
7. **[P1]** Version.parse rejects missing patch version '1.2' — returns Error
8. **[P1]** Version.parse rejects too many core segments '1.2.3.4' — returns Error
9. **[P1]** Version.parse rejects negative segments — returns Error
10. **[P1]** Version.parse rejects invalid pre-release characters — returns Error (Invalid_pre_release_segment ...)
11. **[P1]** Version.to_string after parse on a version with pre-release and build — round-trips to a canonical semver string
12. **[P1]** Version.compare on equal versions with different build metadata — treats them as equal because build metadata is ignored
13. **[P1]** Version.compare stable release vs pre-release — stable release compares greater than the corresponding pre-release
14. **[P1]** Version.compare numeric pre-release segments — orders 1.0.0-alpha.1 < 1.0.0-alpha.2
15. **[P1]** Version.compare numeric vs alphanumeric pre-release segments — numeric segment compares lower than alphanumeric
16. **[P1]** Version.compare shorter vs longer equal-prefix pre-release lists — shorter list compares lower, e.g. alpha < alpha.1
17. **[P1]** Version.lt / lte / gt / gte — agree with Version.compare for representative pairs
18. **[P1]** Version.make default optional fields — creates a version with empty pre-release list and no build
19. **[P1]** Version.parse_requirement '*' — returns the unconstrained Any requirement
20. **[P1]** Version.parse_requirement '== 1.2.3' — returns an exact-match requirement
21. **[P1]** Version.parse_requirement '!= 1.2.3' — returns a not-equal requirement
22. **[P1]** Version.parse_requirement '>= 1.2.3' — returns a greater-than-or-equal requirement
23. **[P1]** Version.parse_requirement '~> 1.2.3' — returns a tilde requirement anchored at 1.2.3
24. **[P1]** Version.parse_requirement bare major '1' — returns PrefixMajorRequirement 1
25. **[P1]** Version.parse_requirement bare minor prefix '1.2' — returns PrefixMinorRequirement (1,2)
26. **[P1]** Version.requirement_to_string after parsing '>= 1.2.3' — returns the canonical string '>= 1.2.3'
27. **[P1]** Version.matches exact requirement — is true only for the same semantic version
28. **[P1]** Version.matches prefix major requirement '1' — accepts any 1.x.y version and rejects 2.x.y
29. **[P1]** Version.matches prefix minor requirement '1.2' — accepts 1.2.x and rejects 1.3.x
30. **[P1]** Version.matches tilde requirement '~> 1.2.3' — accepts >=1.2.3 and <1.3.0

## WorkerPool

1. **[P1]** SimpleWorkerPool.run on an empty task list — returns [] immediately
2. **[P1]** SimpleWorkerPool.run on a singleton task list — returns [(0, result)]
3. **[P1]** SimpleWorkerPool.run with tasks [a;b;c] — returns index/result pairs for all tasks
4. **[P0]** SimpleWorkerPool.run preserves input task order in the returned list — results are ordered by task index, not completion time
5. **[P1]** SimpleWorkerPool.run with concurrency 1 — behaves like a sequential indexed map
6. **[P1]** SimpleWorkerPool.run with concurrency greater than task count — still returns one result per task with no duplicates or omissions
7. **[P1]** SimpleWorkerPool.run propagates worker exceptions in a defined way — either fails the run or captures the exception consistently; the test should lock down the intended contract
8. **[P1]** DynamicWorkerPool.start sends WorkerReady messages to the owner — the owner receives readiness notifications before assigning tasks
9. **[P1]** DynamicWorkerPool.send_task executes the supplied worker function with the right task payload — the worker receives exactly the task sent for that typed worker
10. **[P1]** DynamicWorkerPool type safety across task refs — prevents sending a task value to a worker pool of a different task type

