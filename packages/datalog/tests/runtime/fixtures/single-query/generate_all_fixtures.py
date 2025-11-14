#!/usr/bin/env python3
"""
Generate comprehensive Datalog test fixtures (~500 total)
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

print("Generating Category 2: Simple Queries (0051-0100)")
current = 51

# 0051-0060: Query with wildcards
wildcard_tests = [
    ("query_wildcard_first", 'r(1, 2).\nr(3, 4).', "r(_, Y)", [{"Y": 2}, {"Y": 4}]),
    ("query_wildcard_second", 'r(1, 2).\nr(3, 4).', "r(X, _)", [{"X": 1}, {"X": 3}]),
    ("query_both_wildcards", 'r(1, 2).\nr(3, 4).', "r(_, _)", [{}]),
    ("query_wildcard_ternary", 'r(1, 2, 3).\nr(4, 5, 6).', "r(_, X, _)", [{"X": 2}, {"X": 5}]),
    ("query_multiple_wildcards", 'r(1, 2, 3, 4).\nr(5, 6, 7, 8).', "r(_, X, _, Y)", 
     [{"X": 2, "Y": 4}, {"X": 6, "Y": 8}]),
    ("query_constant_match", 'r(1, 2).\nr(1, 3).\nr(2, 3).', "r(1, X)", [{"X": 2}, {"X": 3}]),
    ("query_constant_both", 'r(1, 2).\nr(1, 3).\nr(2, 3).', "r(1, 2)", [{}]),
    ("query_no_match_constant", 'r(1, 2).\nr(3, 4).', "r(5, X)", []),
    ("query_mixed_vars_consts", 'r(1, 2, 3).\nr(1, 4, 5).', "r(1, X, Y)", 
     [{"X": 2, "Y": 3}, {"X": 4, "Y": 5}]),
    ("query_var_repetition", 'r(1, 1).\nr(2, 2).\nr(1, 2).', "r(X, X)", [{"X": 1}, {"X": 2}]),
]

for i, (name, facts_str, query, result) in enumerate(wildcard_tests):
    facts_list = [{"predicate": fact.split("(")[0], "args": eval(f'[{fact.split("(")[1].split(")")[0]}]')}
                  for fact in facts_str.strip().split(".\n") if fact.strip()]
    write_fixture(current + i, name, facts_str, facts_list, query, result)
current += len(wildcard_tests)

# 0061-0070: Multiple variables
multi_var_tests = [
    ("two_variables", 'edge(1, 2).\nedge(3, 4).', "edge(X, Y)", 
     [{"X": 1, "Y": 2}, {"X": 3, "Y": 4}]),
    ("three_variables", 'triple(1, 2, 3).\ntriple(4, 5, 6).', "triple(X, Y, Z)",
     [{"X": 1, "Y": 2, "Z": 3}, {"X": 4, "Y": 5, "Z": 6}]),
    ("four_variables", 'quad(1, 2, 3, 4).', "quad(A, B, C, D)",
     [{"A": 1, "B": 2, "C": 3, "D": 4}]),
    ("five_variables", 'quint(1, 2, 3, 4, 5).', "quint(A, B, C, D, E)",
     [{"A": 1, "B": 2, "C": 3, "D": 4, "E": 5}]),
    ("shared_variables", 'rel(1, 2, 1).\nrel(3, 4, 3).', "rel(X, Y, X)",
     [{"X": 1, "Y": 2}, {"X": 3, "Y": 4}]),
    ("all_same_variable", 'same(5, 5, 5).', "same(X, X, X)", [{"X": 5}]),
    ("partial_sharing", 'r(1, 2, 1, 3).\nr(4, 5, 4, 6).', "r(X, Y, X, Z)",
     [{"X": 1, "Y": 2, "Z": 3}, {"X": 4, "Y": 5, "Z": 6}]),
    ("string_variables", 'word("hello", "world").\nword("foo", "bar").', 'word(X, Y)',
     [{"X": "hello", "Y": "world"}, {"X": "foo", "Y": "bar"}]),
    ("mixed_type_variables", 'data(1, "a").\ndata(2, "b").', 'data(X, Y)',
     [{"X": 1, "Y": "a"}, {"X": 2, "Y": "b"}]),
    ("empty_result_vars", 'nothing(1).', 'nothing(X), nothing(Y), X != Y', []),
]

for i, (name, datalog, query, result) in enumerate(multi_var_tests):
    facts_list = []
    for line in datalog.strip().split(".\n"):
        if line.strip() and "(" in line:
            pred = line.split("(")[0]
            args_str = line.split("(")[1].split(")")[0]
            try:
                args = eval(f'[{args_str}]')
                facts_list.append({"predicate": pred, "args": args})
            except:
                pass
    write_fixture(current + i, name, datalog, facts_list, query, result)
current += len(multi_var_tests)

# 0071-0080: Query variations
query_variations = [
    ("query_single_fact", 'single(42).', 'single(X)', [{"X": 42}]),
    ("query_no_facts", '', 'anything(X)', []),
    ("query_wrong_predicate", 'exists(1).', 'missing(X)', []),
    ("query_wrong_arity", 'binary(1, 2).', 'binary(X)', []),
    ("query_excess_arity", 'unary(1).', 'unary(X, Y)', []),
    ("query_all_constants", 'fact(1, 2, 3).', 'fact(1, 2, 3)', [{}]),
    ("query_partial_constants", 'r(1, 2, 3).\nr(1, 4, 5).', 'r(1, X, Y)',
     [{"X": 2, "Y": 3}, {"X": 4, "Y": 5}]),
    ("query_multiple_same_pred", 'p(1).\np(2).\np(3).\np(4).\np(5).', 'p(X)',
     [{"X": 1}, {"X": 2}, {"X": 3}, {"X": 4}, {"X": 5}]),
    ("query_filter_by_value", 'val(10).\nval(20).\nval(30).', 'val(20)', [{}]),
    ("query_complex_pattern", 'r(1, 2, 1, 2).\nr(3, 4, 3, 4).', 'r(X, Y, X, Y)',
     [{"X": 1, "Y": 2}, {"X": 3, "Y": 4}]),
]

for i, (name, datalog, query, result) in enumerate(query_variations):
    facts_list = []
    for line in datalog.strip().split(".\n"):
        if line.strip() and "(" in line:
            pred = line.split("(")[0]
            args_str = line.split("(")[1].split(")")[0]
            try:
                args = eval(f'[{args_str}]')
                facts_list.append({"predicate": pred, "args": args})
            except:
                pass
    write_fixture(current + i, name, datalog, facts_list, query, result)
current += len(query_variations)

# 0081-0090: String queries
string_tests = [
    ('query_strings', 'name("alice").\nname("bob").', 'name(X)', 
     [{"X": "alice"}, {"X": "bob"}]),
    ('query_string_constant', 'name("alice").\nname("bob").', 'name("alice")', [{}]),
    ('query_empty_string', 'val("").\nval("a").', 'val(X)', [{"X": ""}, {"X": "a"}]),
    ('query_long_strings', 'text("the quick brown fox").\ntext("jumps over").', 'text(X)',
     [{"X": "the quick brown fox"}, {"X": "jumps over"}]),
    ('query_special_chars', 'sym("!@#").\nsym("$%^").', 'sym(X)', 
     [{"X": "!@#"}, {"X": "$%^"}]),
    ('query_numbers_as_strings', 'code("123").\ncode("456").', 'code(X)',
     [{"X": "123"}, {"X": "456"}]),
    ('query_spaces', 'phrase("hello world").\nphrase("foo bar").', 'phrase(X)',
     [{"X": "hello world"}, {"X": "foo bar"}]),
    ('query_case_sensitive', 'word("Hello").\nword("hello").', 'word(X)',
     [{"X": "Hello"}, {"X": "hello"}]),
    ('query_unicode', 'char("α").\nchar("β").', 'char(X)', [{"X": "α"}, {"X": "β"}]),
    ('query_mixed_string_int', 'pair("a", 1).\npair("b", 2).', 'pair(X, Y)',
     [{"X": "a", "Y": 1}, {"X": "b", "Y": 2}]),
]

for i, (name, datalog, query, result) in enumerate(string_tests):
    facts_list = []
    for line in datalog.strip().split(".\n"):
        if line.strip() and "(" in line:
            pred = line.split("(")[0]
            args_str = line.split("(")[1].rsplit(")", 1)[0]
            try:
                args = eval(f'[{args_str}]')
                facts_list.append({"predicate": pred, "args": args})
            except:
                pass
    write_fixture(current + i, name, datalog, facts_list, query, result)
current += len(string_tests)

# 0091-0100: Edge case queries
edge_queries = [
    ('query_negative_numbers', 'num(-5).\nnum(-10).\nnum(0).', 'num(X)',
     [{"X": -5}, {"X": -10}, {"X": 0}]),
    ('query_large_numbers', 'big(1000000).\nbig(9999999).', 'big(X)',
     [{"X": 1000000}, {"X": 9999999}]),
    ('query_duplicate_results', 'dup(1).\ndup(1).', 'dup(X)', [{"X": 1}]),
    ('query_many_results', '\n'.join([f'item({i}).' for i in range(20)]), 'item(X)',
     [{"X": i} for i in range(20)]),
    ('query_single_var_multiple_times', 'eq(1, 1).\neq(2, 2).', 'eq(X, X)',
     [{"X": 1}, {"X": 2}]),
    ('query_different_vars_same_value', 'rel(1, 1).\nrel(2, 3).', 'rel(X, Y)',
     [{"X": 1, "Y": 1}, {"X": 2, "Y": 3}]),
    ('query_nested_structure_simulation', 'tree(1, 2, 3, 4).', 'tree(A, B, C, D)',
     [{"A": 1, "B": 2, "C": 3, "D": 4}]),
    ('query_sparse_data', 'sparse(1, 100).\nsparse(2, 200).', 'sparse(X, Y)',
     [{"X": 1, "Y": 100}, {"X": 2, "Y": 200}]),
    ('query_sequential_ids', '\n'.join([f'id({i}).' for i in range(10)]), 'id(X)',
     [{"X": i} for i in range(10)]),
    ('query_gaps_in_sequence', 'gap(1).\ngap(3).\ngap(5).\ngap(7).', 'gap(X)',
     [{"X": 1}, {"X": 3}, {"X": 5}, {"X": 7}]),
]

for i, (name, datalog, query, result) in enumerate(edge_queries):
    facts_list = []
    for line in datalog.strip().split(".\n"):
        if line.strip() and "(" in line and not line.startswith("%"):
            pred = line.split("(")[0]
            args_str = line.split("(")[1].rsplit(")", 1)[0]
            try:
                args = eval(f'[{args_str}]')
                facts_list.append({"predicate": pred, "args": args})
            except:
                pass
    write_fixture(current + i, name, datalog, facts_list, query, result)
current += len(edge_queries)

print(f"Category 2 complete: Generated fixtures 0051-{current-1:04d}")
print(f"Total: {current - 51} fixtures")
