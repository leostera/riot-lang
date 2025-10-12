#!/usr/bin/env python3
import json
import sys
import os

if len(sys.argv) < 5:
    print("Usage: verify_parse.py <source_file> <tokens_json> <green_tree_json> <red_tree_json> [--verbose]")
    sys.exit(1)

source_file = sys.argv[1]
tokens_file = sys.argv[2]
green_tree_file = sys.argv[3]
red_tree_file = sys.argv[4]
verbose = '--verbose' in sys.argv

# Read the source file
with open(source_file, 'r') as f:
    source = f.read()

# Read the JSON files
with open(tokens_file, 'r') as f:
    tokens = json.load(f)

with open(green_tree_file, 'r') as f:
    green_tree = json.load(f)

with open(red_tree_file, 'r') as f:
    red_tree = json.load(f)

if verbose:
    print("=== SOURCE FILE ===")
    print(f"Length: {len(source)} chars")
    print(f"First 100 chars: {repr(source[:100])}")
    print()

if verbose:
    print("=== TOKENS VERIFICATION ===")
    print(f"Total tokens: {len(tokens)}")

# Verify tokens cover the entire source
errors = []
for i, token in enumerate(tokens):
    start = token['start']
    end = token['end']
    kind = token['kind']
    text = source[start:end]
    
    if verbose and i < 20:  # Print first 20 tokens
        print(f"Token {i:3d}: [{start:4d}..{end:4d}] {kind:15s} = {repr(text)}")
    
    # Check for gaps
    if i > 0:
        prev_end = tokens[i-1]['end']
        if start != prev_end:
            errors.append(f"GAP: Token {i-1} ends at {prev_end}, token {i} starts at {start}")

# Check coverage
if tokens:
    first_start = tokens[0]['start']
    last_end = tokens[-1]['end']
    
    if first_start != 0:
        errors.append(f"ERROR: First token doesn't start at 0 (starts at {first_start})")
    
    if last_end != len(source):
        errors.append(f"ERROR: Last token doesn't end at source length (ends at {last_end}, source is {len(source)})")
    
    if verbose:
        print(f"\nToken coverage: {first_start} to {last_end} (source: 0 to {len(source)})")

if verbose:
    print()
    print("=== GREEN TREE VERIFICATION ===")

def verify_green_node(node, offset=0, depth=0):
    indent = "  " * depth
    node_type = node['type']
    
    if node_type == 'token':
        width = node['width']
        text = node.get('text', '')
        kind = node['kind']
        actual_text = source[offset:offset+width]
        
        if text != actual_text:
            errors.append(f"GREEN TOKEN MISMATCH at offset {offset}: expected {repr(text)}, got {repr(actual_text)}")
        
        if verbose and depth < 3:
            print(f"{indent}Token [{offset}..{offset+width}] {kind} = {repr(text)}")
        return offset + width
        
    elif node_type == 'node':
        kind = node['kind']
        width = node['width']
        
        if verbose and depth < 3:
            print(f"{indent}Node [{offset}..{offset+width}] {kind}")
        
        current = offset
        for child in node.get('children', []):
            current = verify_green_node(child, current, depth + 1)
        
        actual_width = current - offset
        if actual_width != width:
            errors.append(f"GREEN WIDTH MISMATCH at {kind} offset {offset}: declared {width}, actual {actual_width}")
        
        return current
    
    return offset

green_total = verify_green_node(green_tree['tree'])
if verbose:
    print(f"\nGreen tree total width: {green_total} (source length: {len(source)})")

if green_total != len(source):
    errors.append(f"GREEN TREE doesn't cover entire source: {green_total} != {len(source)}")

if verbose:
    print()
    print("=== RED TREE VERIFICATION ===")

def verify_red_node(node, depth=0):
    indent = "  " * depth
    node_type = node['type']
    
    if node_type == 'token':
        span = node['span']
        start = span['start']
        end = span['end']
        text = node.get('text', '')
        kind = node['kind']
        actual_text = source[start:end]
        
        if text != actual_text:
            errors.append(f"RED TOKEN MISMATCH at span [{start}..{end}]: expected {repr(text)}, got {repr(actual_text)}")
        
        if verbose and depth < 3:
            print(f"{indent}Token [{start}..{end}] {kind} = {repr(text)}")
        
    elif node_type == 'node':
        kind = node['kind']
        span = node['span']
        start = span['start']
        end = span['end']
        
        if verbose and depth < 3:
            print(f"{indent}Node [{start}..{end}] {kind}")
        
        children = node.get('children', [])
        if children:
            first_child_start = get_span_start(children[0])
            last_child_end = get_span_end(children[-1])
            
            if first_child_start != start:
                errors.append(f"RED NODE START MISMATCH at {kind}: node starts at {start}, first child at {first_child_start}")
            
            if last_child_end != end:
                errors.append(f"RED NODE END MISMATCH at {kind}: node ends at {end}, last child at {last_child_end}")
        
        for child in children:
            verify_red_node(child, depth + 1)

def get_span_start(node):
    return node['span']['start']

def get_span_end(node):
    return node['span']['end']

verify_red_node(red_tree)

red_start = red_tree['span']['start']
red_end = red_tree['span']['end']
if verbose:
    print(f"\nRed tree span: [{red_start}..{red_end}] (source length: {len(source)})")

if red_start != 0 or red_end != len(source):
    errors.append(f"RED TREE doesn't cover entire source: [{red_start}..{red_end}] != [0..{len(source)}]")

if verbose:
    print()

# Calculate coverage metrics
source_len = len(source)
green_coverage = green_total
red_coverage = red_end - red_start if red_start == 0 else 0

green_missing = source_len - green_coverage
red_missing = source_len - red_coverage

if errors:
    if verbose:
        print(f"✗ {source_file}: Found {len(errors)} errors")
        for error in errors:
            print(f"  - {error}")
    else:
        # Show concise coverage info
        if green_missing > 0 or red_missing > 0:
            print(f"✗ {source_file}: {green_coverage}/{source_len} chars ({green_missing} chars missing, {len(errors)} mismatches)")
        else:
            print(f"✗ {source_file}: {len(errors)} mismatches (coverage complete)")
    sys.exit(1)
else:
    if verbose:
        print("✓ No errors found! Parsing and spans are correct.")
    else:
        print(f"✓ {source_file}: {green_coverage}/{source_len} chars (100.00%)")
    sys.exit(0)
