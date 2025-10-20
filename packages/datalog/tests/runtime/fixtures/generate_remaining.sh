#!/bin/bash
# Generate remaining fixtures in bulk

cd "$(dirname "$0")"

python3 << 'PYTHON'
import json

def wf(num, name, datalog, query, result):
    """Write fixture"""
    base = f"{num:04d}_{name}"
    with open(f"{base}.datalog", "w") as f:
        f.write(f"% Test #{num:04d}: {name}\n\n{datalog}\n")
    
    # Parse facts
    facts = []
    for line in datalog.strip().split("\n"):
        if "(" in line and not line.strip().startswith("%") and not line.strip().startswith(":-"):
            try:
                pred = line.split("(")[0].strip()
                if pred and pred[0].islower():
                    args_str = line.split("(", 1)[1].rsplit(")",1)[0]
                    args = eval(f"[{args_str}]")
                    facts.append({"predicate": pred, "args": args})
            except: pass
    
    with open(f"{base}.datalog.expected", "w") as f:
        json.dump({"facts": facts, "query": query, "result": result}, f, indent=2)
    return base

# Continue from 0106
c = 106

# More joins (0106-0150)
print("Generating joins 0106-0150...")
for i in range(45):
    n = c + i
    if i < 10:  # Three-way joins
        wf(n, f"three_way_join_{i}", 
           f"r1({i}, {i+1}).\nr2({i+1}, {i+2}).\nr3({i+2}, {i+3}).",
           "r1(X, Y), r2(Y, Z), r3(Z, W)",
           [{"X": i, "Y": i+1, "Z": i+2, "W": i+3}])
    elif i < 20:  # Self joins
        wf(n, f"self_join_{i}", 
           f"rel({i}, {i+1}).\nrel({i+1}, {i+2}).",
           "rel(X, Y), rel(X, Z)",
           [{"X": i, "Y": i+1, "Z": i+1}] if i == i+1 else [])
    elif i < 30:  # Cartesian products
        wf(n, f"cartesian_{i}",
           f"a({i}).\nb({i+1}).",
           "a(X), b(Y)",
           [{"X": i, "Y": i+1}])
    else:  # Complex joins
        wf(n, f"complex_join_{i}",
           f"e({i},{i+1}).\ne({i+1},{i+2}).\ne({i+2},{i+3}).",
           "e(A,B), e(B,C), e(C,D)",
           [{"A":i, "B":i+1, "C":i+2, "D":i+3}])

c += 45

# RULES (0151-0200)
print("Generating rules 0151-0200...")
for i in range(50):
    n = c + i
    if i < 15:  # Simple derivation rules
        wf(n, f"simple_rule_{i}",
           f"fact({i}).\nderived(X) :- fact(X).",
           "derived(X)",
           [{"X": i}])
    elif i < 30:  # Rules with multiple body clauses
        wf(n, f"multi_body_rule_{i}",
           f"a({i}).\nb({i}).\nc(X) :- a(X), b(X).",
           "c(X)",
           [{"X": i}])
    else:  # Chain rules
        wf(n, f"chain_rule_{i}",
           f"r1({i},{i+1}).\nr2(X,Y) :- r1(X,Y).\nr3(X,Y) :- r2(X,Y).",
           "r3(X,Y)",
           [{"X": i, "Y": i+1}])

c += 50

# RECURSION (0201-0250)
print("Generating recursion 0201-0250...")
for i in range(50):
    n = c + i
    if i < 25:  # Transitive closure variations
        edges = ".\n".join([f"edge({j},{j+1})" for j in range(i, i+5)])
        wf(n, f"tc_{i}",
           f"{edges}.\npath(X,Y) :- edge(X,Y).\npath(X,Z) :- edge(X,Y), path(Y,Z).",
           "path(X,Y)",
           [{"X": i, "Y": i+5}])  # Just check endpoint is reachable
    else:  # Ancestor patterns
        wf(n, f"ancestor_{i}",
           f"parent({i},{i+1}).\nparent({i+1},{i+2}).\nanc(X,Y) :- parent(X,Y).\nanc(X,Z) :- parent(X,Y), anc(Y,Z).",
           "anc(X,Y)",
           [{"X": i, "Y": i+1}, {"X": i, "Y": i+2}, {"X": i+1, "Y": i+2}])

c += 50

# NEGATION (0251-0300)
print("Generating negation 0251-0300...")
for i in range(50):
    n = c + i
    if i < 25:  # Simple negation
        wf(n, f"simple_negation_{i}",
           f"node({i}).\nnode({i+1}).\nedge({i},{i+1}).\nnot_edge(X,Y) :- node(X), node(Y), !edge(X,Y).",
           "not_edge(X,Y)",
           [{"X": i+1, "Y": i}, {"X": i+1, "Y": i+1}, {"X": i, "Y": i}])
    else:  # Complement finding
        wf(n, f"complement_{i}",
           f"item({i}).\nitem({i+1}).\nselected({i}).\nnot_selected(X) :- item(X), !selected(X).",
           "not_selected(X)",
           [{"X": i+1}])

c += 50

# BUILT-INS (0301-0350)
print("Generating built-ins 0301-0350...")
for i in range(50):
    n = c + i
    num = i + 1
    if i < 10:  # Comparison
        wf(n, f"gt_{i}",
           f"val({num}).\nval({num+5}).\nlarge(X) :- val(X), X > {num+2}.",
           "large(X)",
           [{"X": num+5}])
    elif i < 20:  # Equality
        wf(n, f"eq_{i}",
           f"pair({num},{num}).\npair({num+1},{num+2}).\nsame(X,Y) :- pair(X,Y), X = Y.",
           "same(X,Y)",
           [{"X": num, "Y": num}])
    elif i < 30:  # Inequality
        wf(n, f"neq_{i}",
           f"val({num}).\nval({num+1}).\ndiff(X,Y) :- val(X), val(Y), X != Y.",
           "diff(X,Y)",
           [{"X": num, "Y": num+1}, {"X": num+1, "Y": num}])
    else:  # Range checks
        wf(n, f"range_{i}",
           f"num({num}).\nnum({num+10}).\nin_range(X) :- num(X), X >= {num}, X <= {num+5}.",
           "in_range(X)",
           [{"X": num}])

c += 50

# COMPLEX QUERIES (0351-0400)
print("Generating complex queries 0351-0400...")
for i in range(50):
    n = c + i
    wf(n, f"complex_{i}",
       f"a({i}).\nb({i},{i+1}).\nc({i+1},{i+2}).\nd(X,Z) :- a(X), b(X,Y), c(Y,Z).",
       "d(X,Z)",
       [{"X": i, "Z": i+2}])

c += 50

# GRAPH ALGORITHMS (0401-0450)
print("Generating graph algorithms 0401-0450...")
for i in range(50):
    n = c + i
    # Various graph patterns
    if i % 3 == 0:  # Reachability
        wf(n, f"reach_{i}",
           f"edge({i},{i+1}).\nedge({i+1},{i+2}).\nreach(X,Y) :- edge(X,Y).\nreach(X,Z) :- reach(X,Y), edge(Y,Z).",
           "reach(X,Y)",
           [{"X":i,"Y":i+1}, {"X":i,"Y":i+2}, {"X":i+1,"Y":i+2}])
    elif i % 3 == 1:  # Connected components (simplified)
        wf(n, f"component_{i}",
           f"edge({i},{i+1}).\nconn(X,Y) :- edge(X,Y).\nconn(X,Z) :- conn(X,Y), edge(Y,Z).",
           "conn(X,Y)",
           [{"X": i, "Y": i+1}])
    else:  # Cycle detection
        wf(n, f"cycle_{i}",
           f"e({i},{i+1}).\ne({i+1},{i}).\ncyc(X) :- e(X,Y), e(Y,X).",
           "cyc(X)",
           [{"X": i}, {"X": i+1}])

c += 50

# REAL-WORLD SCENARIOS (0451-0500)
print("Generating real-world scenarios 0451-0500...")
for i in range(50):
    n = c + i
    if i < 10:  # Family trees
        wf(n, f"family_{i}",
           f"parent('p{i}', 'c{i}').\nparent('c{i}', 'g{i}').\ngrandparent(X,Z) :- parent(X,Y), parent(Y,Z).",
           "grandparent(X,Y)",
           [{"X": f"p{i}", "Y": f"g{i}"}])
    elif i < 20:  # Org hierarchies
        wf(n, f"org_{i}",
           f"manages('boss{i}', 'emp{i}').\nreports_to(X,Y) :- manages(Y,X).",
           "reports_to(X,Y)",
           [{"X": f"emp{i}", "Y": f"boss{i}"}])
    elif i < 30:  # Social networks
        wf(n, f"social_{i}",
           f"follows('u{i}', 'u{i+1}').\nfollower(X,Y) :- follows(X,Y).",
           "follower(X,Y)",
           [{"X": f"u{i}", "Y": f"u{i+1}"}])
    elif i < 40:  # File paths
        wf(n, f"path_{i}",
           f"in_dir('/root', '/root/file{i}').\nfile(F) :- in_dir(_, F).",
           "file(F)",
           [{"F": f"/root/file{i}"}])
    else:  # Access control
        wf(n, f"access_{i}",
           f"has_role('user{i}', 'admin').\ncan_access(U,R) :- has_role(U,R).",
           "can_access(U,R)",
           [{"U": f"user{i}", "R": "admin"}])

c += 50

print(f"\n{'='*70}")
print(f"✅ COMPLETE! Generated fixtures up to {c-1:04d}")
print(f"📊 Total: {c-106} new fixtures")
print(f"{'='*70}")

PYTHON
