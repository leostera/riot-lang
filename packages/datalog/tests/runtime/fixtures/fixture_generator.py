#!/usr/bin/env python3
"""
Comprehensive Datalog fixture generator
Generates ~500 test fixtures covering all aspects of Datalog
"""
import json

def write_fixture(num, name, datalog, facts, query, result, rules=None):
    filename_base = f"{num:04d}_{name}"
    
    # Write .datalog file
    with open(f"{filename_base}.datalog", "w") as f:
        f.write(f"% Test #{num:04d}: {name.replace('_', ' ').title()}\n\n")
        f.write(datalog.strip())
        f.write("\n")
    
    # Write .expected file
    expected = {"facts": facts, "query": query, "result": result}
    if rules:
        expected["rules"] = rules
    
    with open(f"{filename_base}.datalog.expected", "w") as f:
        json.dump(expected, f, indent=2)
    
    print(f"✓ {num:04d}_{name}")

print("="*70)
print("DATALOG FIXTURE GENERATOR")
print("="*70)

# Track progress
total_generated = 0

# CATEGORY 3: SIMPLE JOINS (0101-0150)
print("\n📦 Category 3: Simple Joins (0101-0150)")
current = 101

# Two-way joins (0101-0115)
join_tests = [
    ("two_way_join_basic", 
     "r1(1, 2).\nr1(2, 3).\nr2(2, 'a').\nr2(3, 'b').",
     'r1(X, Y), r2(Y, Z)',
     [{"X": 1, "Y": 2, "Z": "a"}, {"X": 2, "Y": 3, "Z": "b"}]),
    
    ("two_way_join_multiple_matches",
     "person('alice', 30).\nperson('bob', 25).\nage_group(30, 'senior').\nage_group(25, 'junior').",
     "person(Name, Age), age_group(Age, Group)",
     [{"Name": "alice", "Age": 30, "Group": "senior"}, {"Name": "bob", "Age": 25, "Group": "junior"}]),
    
    ("join_no_matches",
     "r1(1, 2).\nr2(3, 4).",
     "r1(X, Y), r2(Y, Z)",
     []),
    
    ("join_one_to_many",
     "parent('alice', 'bob').\nparent('alice', 'carol').\nage('bob', 10).\nage('carol', 8).",
     "parent('alice', Child), age(Child, Age)",
     [{"Child": "bob", "Age": 10}, {"Child": "carol", "Age": 8}]),
    
    ("join_many_to_one",
     "likes('alice', 'pizza').\nlikes('bob', 'pizza').\nprice('pizza', 10).",
     "likes(Person, Food), price(Food, Price)",
     [{"Person": "alice", "Food": "pizza", "Price": 10}, 
      {"Person": "bob", "Food": "pizza", "Price": 10}]),
]

for i, (name, datalog, query, result) in enumerate(join_tests):
    # Parse facts from datalog
    facts = []
    for line in datalog.strip().split("\n"):
        if "(" in line and line.strip():
            pred = line.split("(")[0].strip()
            args_str = line.split("(", 1)[1].rsplit(")", 1)[0]
            try:
                args = eval(f"[{args_str}]")
                facts.append({"predicate": pred, "args": args})
            except: pass
    write_fixture(current + i, name, datalog, facts, query, result)

current += len(join_tests)
total_generated += len(join_tests)

# Let me know how many we've generated so far
print(f"\n✅ Generated {total_generated} fixtures so far (up to {current-1:04d})")
print(f"📊 Progress: {total_generated}/500 ({100*total_generated//500}%)")

