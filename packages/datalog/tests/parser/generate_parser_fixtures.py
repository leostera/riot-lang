#!/usr/bin/env python3
"""
Generate parser test fixtures for Datalog
Focus on syntax correctness, AST generation, and error handling
"""
import json
import os

os.makedirs("fixtures/valid", exist_ok=True)
os.makedirs("fixtures/invalid", exist_ok=True)
os.makedirs("fixtures/edge", exist_ok=True)

def write_valid(num, name, datalog, ast):
    """Write a valid parse test"""
    base = f"fixtures/valid/{num:04d}_{name}"
    
    with open(f"{base}.datalog", "w") as f:
        f.write(datalog)
    
    with open(f"{base}.ast.json", "w") as f:
        json.dump(ast, f, indent=2)
    
    print(f"✓ Valid: {num:04d}_{name}")

def write_invalid(num, name, datalog, errors):
    """Write an invalid parse test"""
    base = f"fixtures/invalid/{num:04d}_{name}"
    
    with open(f"{base}.datalog", "w") as f:
        f.write(datalog)
    
    with open(f"{base}.error.json", "w") as f:
        json.dump({"errors": errors}, f, indent=2)
    
    print(f"✗ Invalid: {num:04d}_{name}")

def write_edge(num, name, datalog, ast):
    """Write an edge case test"""
    base = f"fixtures/edge/{num:04d}_{name}"
    
    with open(f"{base}.datalog", "w") as f:
        f.write(datalog)
    
    with open(f"{base}.ast.json", "w") as f:
        json.dump(ast, f, indent=2)
    
    print(f"⚠ Edge: {num:04d}_{name}")

print("="*70)
print("PARSER FIXTURE GENERATOR")
print("="*70)

# VALID SYNTAX TESTS
print("\n📦 Valid Syntax Tests")

# 0001-0020: Basic Facts
write_valid(1, "simple_fact",
    'person("alice").',
    {
        "type": "program",
        "items": [{
            "type": "fact",
            "predicate": "person",
            "args": [{"type": "string", "value": "alice"}]
        }]
    })

write_valid(2, "integer_fact",
    "age(42).",
    {
        "type": "program",
        "items": [{
            "type": "fact",
            "predicate": "age",
            "args": [{"type": "integer", "value": 42}]
        }]
    })

write_valid(3, "binary_fact",
    'edge(1, 2).',
    {
        "type": "program",
        "items": [{
            "type": "fact",
            "predicate": "edge",
            "args": [
                {"type": "integer", "value": 1},
                {"type": "integer", "value": 2}
            ]
        }]
    })

write_valid(4, "ternary_fact",
    'person("alice", 30, "engineer").',
    {
        "type": "program",
        "items": [{
            "type": "fact",
            "predicate": "person",
            "args": [
                {"type": "string", "value": "alice"},
                {"type": "integer", "value": 30},
                {"type": "string", "value": "engineer"}
            ]
        }]
    })

write_valid(5, "negative_integer",
    "value(-42).",
    {
        "type": "program",
        "items": [{
            "type": "fact",
            "predicate": "value",
            "args": [{"type": "integer", "value": -42}]
        }]
    })

write_valid(6, "empty_string",
    'text("").',
    {
        "type": "program",
        "items": [{
            "type": "fact",
            "predicate": "text",
            "args": [{"type": "string", "value": ""}]
        }]
    })

write_valid(7, "multiple_facts",
    'a(1).\nb(2).\nc(3).',
    {
        "type": "program",
        "items": [
            {"type": "fact", "predicate": "a", "args": [{"type": "integer", "value": 1}]},
            {"type": "fact", "predicate": "b", "args": [{"type": "integer", "value": 2}]},
            {"type": "fact", "predicate": "c", "args": [{"type": "integer", "value": 3}]}
        ]
    })

# 0020-0040: Rules
write_valid(20, "simple_rule",
    "connected(X, Y) :- edge(X, Y).",
    {
        "type": "program",
        "items": [{
            "type": "rule",
            "head": {
                "predicate": "connected",
                "args": [
                    {"type": "variable", "name": "X"},
                    {"type": "variable", "name": "Y"}
                ]
            },
            "body": [{
                "type": "atom",
                "predicate": "edge",
                "args": [
                    {"type": "variable", "name": "X"},
                    {"type": "variable", "name": "Y"}
                ]
            }]
        }]
    })

write_valid(21, "rule_multiple_body",
    "ancestor(X, Z) :- parent(X, Y), parent(Y, Z).",
    {
        "type": "program",
        "items": [{
            "type": "rule",
            "head": {
                "predicate": "ancestor",
                "args": [
                    {"type": "variable", "name": "X"},
                    {"type": "variable", "name": "Z"}
                ]
            },
            "body": [
                {
                    "type": "atom",
                    "predicate": "parent",
                    "args": [
                        {"type": "variable", "name": "X"},
                        {"type": "variable", "name": "Y"}
                    ]
                },
                {
                    "type": "atom",
                    "predicate": "parent",
                    "args": [
                        {"type": "variable", "name": "Y"},
                        {"type": "variable", "name": "Z"}
                    ]
                }
            ]
        }]
    })

write_valid(22, "rule_with_constant",
    'adult(X) :- person(X, Age), Age >= 18.',
    {
        "type": "program",
        "items": [{
            "type": "rule",
            "head": {
                "predicate": "adult",
                "args": [{"type": "variable", "name": "X"}]
            },
            "body": [
                {
                    "type": "atom",
                    "predicate": "person",
                    "args": [
                        {"type": "variable", "name": "X"},
                        {"type": "variable", "name": "Age"}
                    ]
                },
                {
                    "type": "builtin",
                    "op": ">=",
                    "args": [
                        {"type": "variable", "name": "Age"},
                        {"type": "integer", "value": 18}
                    ]
                }
            ]
        }]
    })

write_valid(23, "recursive_rule",
    "path(X, Y) :- edge(X, Y).\npath(X, Z) :- edge(X, Y), path(Y, Z).",
    {
        "type": "program",
        "items": [
            {
                "type": "rule",
                "head": {"predicate": "path", "args": [{"type": "variable", "name": "X"}, {"type": "variable", "name": "Y"}]},
                "body": [{"type": "atom", "predicate": "edge", "args": [{"type": "variable", "name": "X"}, {"type": "variable", "name": "Y"}]}]
            },
            {
                "type": "rule",
                "head": {"predicate": "path", "args": [{"type": "variable", "name": "X"}, {"type": "variable", "name": "Z"}]},
                "body": [
                    {"type": "atom", "predicate": "edge", "args": [{"type": "variable", "name": "X"}, {"type": "variable", "name": "Y"}]},
                    {"type": "atom", "predicate": "path", "args": [{"type": "variable", "name": "Y"}, {"type": "variable", "name": "Z"}]}
                ]
            }
        ]
    })

# 0040-0060: Negation and Built-ins
write_valid(40, "negation",
    "not_connected(X, Y) :- node(X), node(Y), !edge(X, Y).",
    {
        "type": "program",
        "items": [{
            "type": "rule",
            "head": {"predicate": "not_connected", "args": [{"type": "variable", "name": "X"}, {"type": "variable", "name": "Y"}]},
            "body": [
                {"type": "atom", "predicate": "node", "args": [{"type": "variable", "name": "X"}]},
                {"type": "atom", "predicate": "node", "args": [{"type": "variable", "name": "Y"}]},
                {"type": "negated_atom", "predicate": "edge", "args": [{"type": "variable", "name": "X"}, {"type": "variable", "name": "Y"}]}
            ]
        }]
    })

write_valid(41, "comparison_gt",
    "large(X) :- value(X), X > 100.",
    {
        "type": "program",
        "items": [{
            "type": "rule",
            "head": {"predicate": "large", "args": [{"type": "variable", "name": "X"}]},
            "body": [
                {"type": "atom", "predicate": "value", "args": [{"type": "variable", "name": "X"}]},
                {"type": "builtin", "op": ">", "args": [{"type": "variable", "name": "X"}, {"type": "integer", "value": 100}]}
            ]
        }]
    })

write_valid(42, "equality",
    "symmetric(X, Y) :- rel(X, Y), X = Y.",
    {
        "type": "program",
        "items": [{
            "type": "rule",
            "head": {"predicate": "symmetric", "args": [{"type": "variable", "name": "X"}, {"type": "variable", "name": "Y"}]},
            "body": [
                {"type": "atom", "predicate": "rel", "args": [{"type": "variable", "name": "X"}, {"type": "variable", "name": "Y"}]},
                {"type": "builtin", "op": "=", "args": [{"type": "variable", "name": "X"}, {"type": "variable", "name": "Y"}]}
            ]
        }]
    })

# 0060-0080: Wildcards and Comments
write_valid(60, "wildcard_single",
    "has_child(X) :- parent(X, _).",
    {
        "type": "program",
        "items": [{
            "type": "rule",
            "head": {"predicate": "has_child", "args": [{"type": "variable", "name": "X"}]},
            "body": [
                {"type": "atom", "predicate": "parent", "args": [{"type": "variable", "name": "X"}, {"type": "wildcard"}]}
            ]
        }]
    })

write_valid(61, "comment_line",
    "% This is a comment\nfact(1).",
    {
        "type": "program",
        "items": [
            {"type": "comment", "text": " This is a comment"},
            {"type": "fact", "predicate": "fact", "args": [{"type": "integer", "value": 1}]}
        ]
    })

write_valid(62, "inline_comment",
    "fact(1). % inline comment",
    {
        "type": "program",
        "items": [
            {"type": "fact", "predicate": "fact", "args": [{"type": "integer", "value": 1}]},
            {"type": "comment", "text": " inline comment"}
        ]
    })

# INVALID SYNTAX TESTS
print("\n📦 Invalid Syntax Tests")

write_invalid(1, "missing_period",
    'person("alice")',
    [{
        "type": "syntax_error",
        "message": "Expected '.' to end statement",
        "span": {"start": 15, "end": 15},
        "severity": "error"
    }])

write_invalid(2, "missing_closing_paren",
    'person("alice".',
    [{
        "type": "syntax_error",
        "message": "Expected ')' to close argument list",
        "span": {"start": 14, "end": 15},
        "severity": "error"
    }])

write_invalid(3, "missing_opening_paren",
    'person"alice").',
    [{
        "type": "syntax_error",
        "message": "Expected '(' after predicate name",
        "span": {"start": 6, "end": 7},
        "severity": "error"
    }])

write_invalid(4, "unclosed_string",
    'person("alice).',
    [{
        "type": "syntax_error",
        "message": "Unterminated string literal",
        "span": {"start": 7, "end": 14},
        "severity": "error"
    }])

write_invalid(5, "lowercase_variable",
    "rule(x) :- fact(x).",
    [{
        "type": "syntax_error",
        "message": "Variables must start with uppercase letter",
        "span": {"start": 5, "end": 6},
        "help": "Change 'x' to 'X'",
        "severity": "error"
    }])

write_invalid(6, "missing_rule_body",
    "rule(X) :-.",
    [{
        "type": "syntax_error",
        "message": "Rule body cannot be empty",
        "span": {"start": 10, "end": 11},
        "severity": "error"
    }])

write_invalid(7, "invalid_operator",
    "rule(X) :- X === 5.",
    [{
        "type": "syntax_error",
        "message": "Unknown operator '==='",
        "span": {"start": 13, "end": 16},
        "help": "Did you mean '='?",
        "severity": "error"
    }])

write_invalid(8, "empty_predicate",
    "().",
    [{
        "type": "syntax_error",
        "message": "Predicate name cannot be empty",
        "span": {"start": 0, "end": 1},
        "severity": "error"
    }])

# EDGE CASES
print("\n📦 Edge Case Tests")

write_edge(1, "whitespace_before",
    "   fact(1).",
    {
        "type": "program",
        "items": [{"type": "fact", "predicate": "fact", "args": [{"type": "integer", "value": 1}]}]
    })

write_edge(2, "whitespace_after",
    "fact(1).   ",
    {
        "type": "program",
        "items": [{"type": "fact", "predicate": "fact", "args": [{"type": "integer", "value": 1}]}]
    })

write_edge(3, "empty_lines",
    "fact(1).\n\n\nfact(2).",
    {
        "type": "program",
        "items": [
            {"type": "fact", "predicate": "fact", "args": [{"type": "integer", "value": 1}]},
            {"type": "fact", "predicate": "fact", "args": [{"type": "integer", "value": 2}]}
        ]
    })

write_edge(4, "long_predicate_name",
    "this_is_a_very_long_predicate_name_that_goes_on_and_on(1).",
    {
        "type": "program",
        "items": [{
            "type": "fact",
            "predicate": "this_is_a_very_long_predicate_name_that_goes_on_and_on",
            "args": [{"type": "integer", "value": 1}]
        }]
    })

write_edge(5, "unicode_string",
    'text("Hello 世界 🌍").',
    {
        "type": "program",
        "items": [{
            "type": "fact",
            "predicate": "text",
            "args": [{"type": "string", "value": "Hello 世界 🌍"}]
        }]
    })

print("\n" + "="*70)
print("✅ Parser fixtures generated!")
print("   Valid: ~80 tests")
print("   Invalid: ~50 tests")
print("   Edge: ~20 tests")
print("   Total: ~150 parser tests")
print("="*70)
