#!/usr/bin/env python3
"""
Debug trivia preservation issues in syn parser.

Usage: python3 debug_trivia.py <source_file>

This script:
1. Generates tokens.json, green-tree.json, and red-tree.json
2. Finds the first position where coverage diverges
3. Shows the surrounding context
"""

import sys
import json
import subprocess
from pathlib import Path

def run_syn_command(command, source_file):
    """Run a syn command and return JSON output."""
    result = subprocess.run(
        ["./target/debug/syn"] + command + [source_file],
        capture_output=True,
        text=True,
        cwd=Path(__file__).parent.parent.parent.parent
    )
    
    if result.returncode != 0:
        print(f"Error running syn {' '.join(command)}:")
        print(result.stderr)
        sys.exit(1)
    
    # Parse JSON from output
    output = result.stdout.strip()
    if not output:
        print(f"No output from syn {' '.join(command)}")
        sys.exit(1)
    
    try:
        return json.loads(output)
    except json.JSONDecodeError as e:
        print(f"Failed to parse JSON from syn {' '.join(command)}: {e}")
        print(f"Output: {output[:200]}")
        sys.exit(1)

def generate_files(source_file):
    """Generate tokens, green tree, and red tree JSON files."""
    print(f"Analyzing: {source_file}")
    print("=" * 80)
    
    # Read source file to annotate tokens with text
    with open(source_file, 'r') as f:
        source = f.read()
    
    # Generate tokens
    print("Generating tokens...")
    tokens = run_syn_command(["tokenize", "--json"], source_file)
    
    # Add text to each token
    for token in tokens:
        start = token["start"]
        end = token["end"]
        token["text"] = source[start:end]
    
    # Generate green tree
    print("Generating green tree...")
    green_tree = run_syn_command(["parse", "--json"], source_file)
    
    # Generate red tree
    print("Generating red tree...")
    red_tree = run_syn_command(["parse", "--json", "--red-tree"], source_file)
    
    return tokens, green_tree, red_tree

def extract_tokens_from_red_tree(node, tokens_list):
    """Recursively extract all tokens from a red tree node."""
    if isinstance(node, dict):
        if node.get("type") == "token":
            # This is a token with span
            span = node.get("span", {})
            tokens_list.append({
                "kind": node["kind"],
                "start": span.get("start", 0),
                "end": span.get("end", 0),
                "text": node.get("text", "")
            })
        elif node.get("type") == "node":
            # This is a node with children
            for child in node.get("children", []):
                extract_tokens_from_red_tree(child, tokens_list)
        elif "tree" in node:
            # Root node
            extract_tokens_from_red_tree(node["tree"], tokens_list)

def find_first_divergence(source_file, tokens, green_tree, red_tree):
    """Find the first position where token coverage diverges."""
    # Read source file
    with open(source_file, 'r') as f:
        source = f.read()
    
    source_len = len(source)
    
    # Extract all tokens from red tree (which has actual positions)
    red_tokens = []
    extract_tokens_from_red_tree(red_tree, red_tokens)
    
    # Calculate coverage
    covered = [False] * source_len
    for token in red_tokens:
        start = token["start"]
        end = token["end"]
        for i in range(start, end):
            if i < source_len:
                covered[i] = True
    
    # Find first uncovered position
    first_gap = None
    for i in range(source_len):
        if not covered[i]:
            first_gap = i
            break
    
    print(f"\nSource file length: {source_len} bytes")
    print(f"Red tree coverage: {sum(covered)}/{source_len} bytes")
    print(f"Red tree tokens: {len(red_tokens)}")
    
    if first_gap is None:
        print("✓ Complete coverage!")
        return
    
    print(f"\n⚠ First gap at position {first_gap}")
    print("=" * 80)
    
    # Find the token that should cover this position
    expected_token = None
    for token in tokens:
        if token["start"] <= first_gap < token["end"]:
            expected_token = token
            break
        if token["start"] == first_gap:
            expected_token = token
            break
    
    if expected_token:
        print(f"\nExpected token at position {first_gap}:")
        print(f"  Kind: {expected_token['kind']}")
        print(f"  Span: [{expected_token['start']}..{expected_token['end']}]")
        print(f"  Text: {repr(expected_token.get('text', ''))}")
    
    # Show context around the gap
    context_start = max(0, first_gap - 50)
    context_end = min(source_len, first_gap + 50)
    
    print(f"\nSource context around position {first_gap}:")
    print("-" * 80)
    
    context = source[context_start:context_end]
    gap_offset = first_gap - context_start
    
    # Show context with marker
    lines = context.split('\n')
    for i, line in enumerate(lines):
        print(f"  {line}")
    
    print("-" * 80)
    print(f"Gap starts at: {repr(source[first_gap:first_gap+10])}")
    
    # Find last red tree token before gap
    last_before_gap = None
    for token in red_tokens:
        if token["end"] <= first_gap:
            if last_before_gap is None or token["end"] > last_before_gap["end"]:
                last_before_gap = token
    
    if last_before_gap:
        print(f"\nLast token in green tree before gap:")
        print(f"  Kind: {last_before_gap['kind']}")
        print(f"  Span: [{last_before_gap['start']}..{last_before_gap['end']}]")
        print(f"  Text: {repr(last_before_gap.get('text', ''))}")
    
    # Show what tokens are missing
    missing_tokens = []
    for token in tokens:
        if token["start"] >= first_gap:
            missing_tokens.append(token)
    
    if missing_tokens:
        print(f"\nMissing tokens (from position {first_gap}):")
        for i, token in enumerate(missing_tokens[:10]):  # Show first 10
            print(f"  {i+1}. [{token['start']}..{token['end']}] {token['kind']}: {repr(token.get('text', ''))}")
        
        if len(missing_tokens) > 10:
            print(f"  ... and {len(missing_tokens) - 10} more tokens")
    
    print("\n" + "=" * 80)

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 debug_trivia.py <source_file>")
        sys.exit(1)
    
    source_file = sys.argv[1]
    
    if not Path(source_file).exists():
        print(f"Error: File not found: {source_file}")
        sys.exit(1)
    
    # Generate files
    tokens, green_tree, red_tree = generate_files(source_file)
    
    # Find divergence
    find_first_divergence(source_file, tokens, green_tree, red_tree)

if __name__ == "__main__":
    main()
