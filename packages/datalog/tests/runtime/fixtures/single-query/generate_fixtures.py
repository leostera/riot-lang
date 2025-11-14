#!/usr/bin/env python3
"""
Generate comprehensive Datalog test fixtures
"""
import json
import os

def write_fixture(num, name, datalog, facts, query, result, rules=None):
    """Write a fixture pair to disk"""
    filename = f"{num:04d}_{name}"
    
    with open(f"{filename}.datalog", "w") as f:
        f.write(f"% Test: {name}\n")
        f.write(f"% Number: {num:04d}\n\n")
        f.write(datalog)
        f.write("\n")
    
    expected = {
        "facts": facts,
        "query": query,
        "result": result
    }
    if rules:
        expected["rules"] = rules
    
    with open(f"{filename}.datalog.expected", "w") as f:
        json.dump(expected, f, indent=2)
    
    return filename

# Category 1: More Basic Facts (0018-0050)
fixtures = []

# 0018-0025: Different predicates
current = 18
for i in range(8):
    name = f"different_predicate_{i}"
    preds = [f"pred{j}" for j in range(i+1)]
    datalog_lines = [f'{p}("value{j}").' for j, p in enumerate(preds)]
    datalog = "\n".join(datalog_lines)
    
    facts = [{"predicate": p, "args": [f"value{j}"]} for j, p in enumerate(preds)]
    query = f'{preds[0]}(X)' if preds else 'pred0(X)'
    result = [{"X": f"value0"}] if preds else []
    
    write_fixture(current, name, datalog, facts, query, result)
    current += 1

# 0026-0035: Varying fact counts
for count in [1, 5, 10, 20, 50, 100, 200, 500, 1000, 2000]:
    name = f"fact_count_{count}"
    datalog_lines = [f"item({i})." for i in range(count)]
    datalog = "\n".join(datalog_lines)
    
    facts = [{"predicate": "item", "args": [i]} for i in range(count)]
    query = "item(X)"
    result = [{"X": i} for i in range(count)]
    
    write_fixture(current, name, datalog, facts, query, result)
    current += 1

# 0036-0040: Edge cases
edge_cases = [
    ("empty_string", 'str("").\nstr("a").', 
     [{"predicate": "str", "args": [""]}, {"predicate": "str", "args": ["a"]}],
     "str(X)", [{"X": ""}, {"X": "a"}]),
    
    ("zero_value", "val(0).\nval(-0).\nval(1).",
     [{"predicate": "val", "args": [0]}, {"predicate": "val", "args": [0]}, {"predicate": "val", "args": [1]}],
     "val(X)", [{"X": 0}, {"X": 1}]),
    
    ("duplicate_facts", "dup(1).\ndup(1).\ndup(2).",
     [{"predicate": "dup", "args": [1]}, {"predicate": "dup", "args": [1]}, {"predicate": "dup", "args": [2]}],
     "dup(X)", [{"X": 1}, {"X": 2}]),
    
    ("long_string", 'text("this_is_a_very_long_string_value_that_goes_on_and_on").',
     [{"predicate": "text", "args": ["this_is_a_very_long_string_value_that_goes_on_and_on"]}],
     "text(X)", [{"X": "this_is_a_very_long_string_value_that_goes_on_and_on"}]),
    
    ("large_int", "big(9999999).\nbig(-9999999).",
     [{"predicate": "big", "args": [9999999]}, {"predicate": "big", "args": [-9999999]}],
     "big(X)", [{"X": 9999999}, {"X": -9999999}]),
]

for name, datalog, facts, query, result in edge_cases:
    write_fixture(current, name, datalog, facts, query, result)
    current += 1

# 0041-0050: Complex fact patterns
patterns = [
    ("symmetrical_relation", "sym(1, 1).\nsym(2, 2).\nsym(3, 3).",
     [{"predicate": "sym", "args": [1, 1]}, {"predicate": "sym", "args": [2, 2]}, {"predicate": "sym", "args": [3, 3]}],
     "sym(X, X)", [{"X": 1}, {"X": 2}, {"X": 3}]),
    
    ("antisymmetrical", "rel(1, 2).\nrel(2, 1).\nrel(3, 4).\nrel(4, 3).",
     [{"predicate": "rel", "args": [1, 2]}, {"predicate": "rel", "args": [2, 1]}, 
      {"predicate": "rel", "args": [3, 4]}, {"predicate": "rel", "args": [4, 3]}],
     "rel(X, Y)", [{"X": 1, "Y": 2}, {"X": 2, "Y": 1}, {"X": 3, "Y": 4}, {"X": 4, "Y": 3}]),
    
    ("chain_relation", "next(1, 2).\nnext(2, 3).\nnext(3, 4).\nnext(4, 5).",
     [{"predicate": "next", "args": [1, 2]}, {"predicate": "next", "args": [2, 3]},
      {"predicate": "next", "args": [3, 4]}, {"predicate": "next", "args": [4, 5]}],
     "next(X, Y)", [{"X": 1, "Y": 2}, {"X": 2, "Y": 3}, {"X": 3, "Y": 4}, {"X": 4, "Y": 5}]),
    
    ("tree_relation", "parent(1, 2).\nparent(1, 3).\nparent(2, 4).\nparent(2, 5).\nparent(3, 6).\nparent(3, 7).",
     [{"predicate": "parent", "args": [1, 2]}, {"predicate": "parent", "args": [1, 3]},
      {"predicate": "parent", "args": [2, 4]}, {"predicate": "parent", "args": [2, 5]},
      {"predicate": "parent", "args": [3, 6]}, {"predicate": "parent", "args": [3, 7]}],
     "parent(X, Y)", [{"X": 1, "Y": 2}, {"X": 1, "Y": 3}, {"X": 2, "Y": 4}, 
                      {"X": 2, "Y": 5}, {"X": 3, "Y": 6}, {"X": 3, "Y": 7}]),
    
    ("dag_relation", "edge(1, 2).\nedge(1, 3).\nedge(2, 4).\nedge(3, 4).",
     [{"predicate": "edge", "args": [1, 2]}, {"predicate": "edge", "args": [1, 3]},
      {"predicate": "edge", "args": [2, 4]}, {"predicate": "edge", "args": [3, 4]}],
     "edge(X, Y)", [{"X": 1, "Y": 2}, {"X": 1, "Y": 3}, {"X": 2, "Y": 4}, {"X": 3, "Y": 4}]),
    
    ("cycle_relation", "link(1, 2).\nlink(2, 3).\nlink(3, 1).",
     [{"predicate": "link", "args": [1, 2]}, {"predicate": "link", "args": [2, 3]}, {"predicate": "link", "args": [3, 1]}],
     "link(X, Y)", [{"X": 1, "Y": 2}, {"X": 2, "Y": 3}, {"X": 3, "Y": 1}]),
    
    ("disconnected_components", "conn(1, 2).\nconn(2, 3).\nconn(4, 5).\nconn(5, 6).",
     [{"predicate": "conn", "args": [1, 2]}, {"predicate": "conn", "args": [2, 3]},
      {"predicate": "conn", "args": [4, 5]}, {"predicate": "conn", "args": [5, 6]}],
     "conn(X, Y)", [{"X": 1, "Y": 2}, {"X": 2, "Y": 3}, {"X": 4, "Y": 5}, {"X": 5, "Y": 6}]),
]

for name, datalog, facts, query, result in patterns:
    write_fixture(current, name, datalog, facts, query, result)
    current += 1

print(f"Generated fixtures up to {current-1:04d}")
print(f"Total new fixtures: {current - 18}")
