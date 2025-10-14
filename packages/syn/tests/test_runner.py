#!/usr/bin/env python3
"""
Unified test runner for syn parser.

Usage:
  # Run all fixture tests
  python3 test_runner.py fixtures
  
  # Run specific fixture tests (filter by prefix/pattern)
  python3 test_runner.py fixtures --filter 08
  python3 test_runner.py fixtures --filter type
  
  # Run diagnostic tests (bad syntax with expected errors)
  python3 test_runner.py diagnostics
  
  # Test entire codebase
  python3 test_runner.py codebase
  
  # Debug a specific file with trivia analysis
  python3 test_runner.py debug <file>
  
  # Run all tests (fixtures + diagnostics + codebase)
  python3 test_runner.py all
"""

import sys
import json
import subprocess
from pathlib import Path
from typing import List, Dict, Tuple, Optional
import argparse

# ANSI colors
GREEN = '\033[0;32m'
RED = '\033[0;31m'
YELLOW = '\033[0;33m'
BLUE = '\033[0;34m'
NC = '\033[0m'  # No Color

class TestRunner:
    def __init__(self, workspace_root: Path):
        self.workspace_root = workspace_root
        self.syn_binary = workspace_root / "target" / "debug" / "syn"
        self.fixtures_dir = workspace_root / "packages" / "syn" / "tests" / "fixtures"
        self.diagnostics_dir = workspace_root / "packages" / "syn" / "tests" / "diagnostics"
        self.packages_dir = workspace_root / "packages"
        
    def run_syn(self, args: List[str], file_path: Path) -> Tuple[str, int]:
        """Run syn command and return (output, returncode)."""
        cmd = [str(self.syn_binary)] + args + [str(file_path)]
        result = subprocess.run(cmd, capture_output=True, text=True)
        return result.stdout, result.returncode
    
    def run_syn_json(self, args: List[str], file_path: Path) -> Optional[Dict]:
        """Run syn command expecting JSON output."""
        output, returncode = self.run_syn(args, file_path)
        if returncode != 0:
            return None
        
        try:
            return json.loads(output.strip())
        except json.JSONDecodeError:
            return None
    
    def extract_tokens_from_red_tree(self, node, tokens_list):
        """Recursively extract all tokens from a red tree node."""
        if isinstance(node, dict):
            if node.get("type") == "token":
                span = node.get("span", {})
                tokens_list.append({
                    "kind": node["kind"],
                    "start": span.get("start", 0),
                    "end": span.get("end", 0),
                    "text": node.get("text", "")
                })
            elif node.get("type") == "node":
                for child in node.get("children", []):
                    self.extract_tokens_from_red_tree(child, tokens_list)
            elif "tree" in node:
                self.extract_tokens_from_red_tree(node["tree"], tokens_list)
    
    def debug_file(self, file_path: Path, verbose: bool = True) -> Tuple[int, int]:
        """
        Debug a single file with trivia analysis.
        Returns (covered_bytes, total_bytes).
        """
        if verbose:
            print(f"\n{BLUE}Analyzing: {file_path}{NC}")
            print("=" * 80)
        
        # Read source
        with open(file_path, 'r') as f:
            source = f.read()
        source_len = len(source)
        
        if verbose:
            print("Generating tokens...")
        tokens_data = self.run_syn_json(["tokenize", "--json"], file_path)
        if not tokens_data:
            if verbose:
                print(f"{RED}Failed to tokenize{NC}")
            return 0, source_len
        
        # Add text to tokens
        tokens = tokens_data if isinstance(tokens_data, list) else []
        for token in tokens:
            start = token.get("start", 0)
            end = token.get("end", 0)
            token["text"] = source[start:end]
        
        if verbose:
            print("Generating green tree...")
        green_tree = self.run_syn_json(["parse", "--json"], file_path)
        if not green_tree:
            if verbose:
                print(f"{RED}Failed to parse green tree{NC}")
            return 0, source_len
        
        if verbose:
            print("Generating red tree...")
        red_tree = self.run_syn_json(["parse", "--json", "--red-tree"], file_path)
        if not red_tree:
            if verbose:
                print(f"{RED}Failed to parse red tree{NC}")
            return 0, source_len
        
        # Extract tokens from red tree
        red_tokens = []
        self.extract_tokens_from_red_tree(red_tree, red_tokens)
        
        # Calculate coverage
        covered = [False] * source_len
        for token in red_tokens:
            start = token["start"]
            end = token["end"]
            for i in range(start, end):
                if i < source_len:
                    covered[i] = True
        
        covered_count = sum(covered)
        
        if verbose:
            print(f"\n{BLUE}Source file length:{NC} {source_len} bytes")
            print(f"{BLUE}Red tree coverage:{NC} {covered_count}/{source_len} bytes")
            print(f"{BLUE}Red tree tokens:{NC} {len(red_tokens)}")
        
        # Find first gap
        first_gap = None
        for i in range(source_len):
            if not covered[i]:
                first_gap = i
                break
        
        if first_gap is None:
            if verbose:
                print(f"{GREEN}✓ Complete coverage!{NC}")
            return covered_count, source_len
        
        # Show gap details
        if verbose:
            print(f"\n{YELLOW}⚠ First gap at position {first_gap}{NC}")
            print("=" * 80)
            
            # Find expected token
            expected_token = None
            for token in tokens:
                if token["start"] <= first_gap < token["end"] or token["start"] == first_gap:
                    expected_token = token
                    break
            
            if expected_token:
                print(f"\nExpected token at position {first_gap}:")
                print(f"  Kind: {expected_token['kind']}")
                print(f"  Span: [{expected_token['start']}..{expected_token['end']}]")
                print(f"  Text: {repr(expected_token.get('text', ''))}")
            
            # Show context
            context_start = max(0, first_gap - 50)
            context_end = min(source_len, first_gap + 50)
            
            print(f"\nSource context around position {first_gap}:")
            print("-" * 80)
            context = source[context_start:context_end]
            print(f"  {context}")
            print("-" * 80)
            print(f"Gap starts at: {repr(source[first_gap:first_gap+10])}")
            
            # Last token before gap
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
            
            # Missing tokens
            missing_tokens = [t for t in tokens if t["start"] >= first_gap]
            if missing_tokens:
                print(f"\nMissing tokens (from position {first_gap}):")
                for i, token in enumerate(missing_tokens[:10]):
                    print(f"  {i+1}. [{token['start']}..{token['end']}] {token['kind']}: {repr(token.get('text', ''))}")
                if len(missing_tokens) > 10:
                    print(f"  ... and {len(missing_tokens) - 10} more tokens")
            
            print("\n" + "=" * 80)
        
        return covered_count, source_len
    
    def test_fixture(self, fixture_path: Path, verbose: bool = False) -> bool:
        """Test a single fixture file. Returns True if passed."""
        expected_path = Path(str(fixture_path) + ".expected")
        
        if not expected_path.exists():
            if verbose:
                print(f"{RED}No expected file for {fixture_path.name}{NC}")
            return False
        
        # Parse the file
        output, returncode = self.run_syn(["parse", "--json"], fixture_path)
        if returncode != 0:
            if verbose:
                print(f"{RED}Parser failed for {fixture_path.name}{NC}")
            return False
        
        # Read expected
        with open(expected_path, 'r') as f:
            expected = f.read().strip()
        
        actual = output.strip()
        
        # Check for ERROR/MISSING tokens
        if '"ERROR"' in actual or '"MISSING"' in actual:
            if verbose:
                print(f"{RED}Parser produces ERROR/MISSING tokens{NC}")
            return False
        
        if '"ERROR"' in expected or '"MISSING"' in expected:
            if verbose:
                print(f"{RED}Expected file contains ERROR/MISSING tokens{NC}")
            return False
        
        # Check for empty parse tree
        if '"width":0,"children":[]' in actual:
            if verbose:
                print(f"{RED}Empty parse tree - feature not implemented{NC}")
            return False
        
        if '"width":0,"children":[]' in expected:
            if verbose:
                print(f"{RED}Expected file has empty parse tree{NC}")
            return False
        
        # Compare output
        return actual == expected
    
    def run_fixtures(self, filter_pattern: Optional[str] = None) -> Tuple[int, int]:
        """Run fixture tests. Returns (passed, failed)."""
        print(f"\n{BLUE}Running fixture tests...{NC}")
        if filter_pattern:
            print(f"Filter: {filter_pattern}")
        print()
        
        # Find matching fixtures
        pattern = f"*{filter_pattern}*.ml" if filter_pattern else "*.ml"
        fixtures = sorted(self.fixtures_dir.glob(pattern))
        
        if not fixtures:
            print(f"{YELLOW}No fixtures found matching: {pattern}{NC}")
            return 0, 0
        
        passed = 0
        failed = 0
        
        for fixture in fixtures:
            # Skip .expected files
            if fixture.suffix == ".expected":
                continue
            
            basename = fixture.name
            result = self.test_fixture(fixture)
            
            if result:
                print(f"{basename}... {GREEN}PASSED{NC}")
                passed += 1
            else:
                print(f"{basename}... {RED}FAILED{NC}")
                failed += 1
        
        print(f"\n{BLUE}Results:{NC} {passed} passed, {failed} failed")
        return passed, failed
    
    def test_diagnostic(self, test_file: Path, verbose: bool = False) -> bool:
        """Test a single diagnostic file. Returns True if passed."""
        diagnostic_path = Path(str(test_file) + ".diagnostic")
        
        # Parse the file
        output, returncode = self.run_syn(["parse", "--json"], test_file)
        if returncode != 0:
            if verbose:
                print(f"{RED}Parser failed for {test_file.name}{NC}")
            return False
        
        try:
            result = json.loads(output.strip())
        except json.JSONDecodeError:
            if verbose:
                print(f"{RED}Invalid JSON output{NC}")
            return False
        
        # Extract diagnostics from result
        actual_diagnostics = result.get("diagnostics", [])
        
        # If .diagnostic file doesn't exist, create it with actual diagnostics
        if not diagnostic_path.exists():
            if verbose:
                print(f"{YELLOW}Creating .diagnostic file for {test_file.name}{NC}")
            with open(diagnostic_path, 'w') as f:
                json.dump(actual_diagnostics, f, indent=2)
                f.write('\n')
            return True
        
        # Read expected diagnostics
        with open(diagnostic_path, 'r') as f:
            expected_diagnostics_text = f.read().strip()
        
        # Check if it's TBD (not yet implemented)
        if "TBD" in expected_diagnostics_text:
            if verbose:
                print(f"{YELLOW}Test not yet implemented (TBD) - SKIPPING{NC}")
            # TBD tests are automatically PASSED until implementation
            # This allows us to add diagnostic tests before implementing them
            return True
        
        try:
            expected_diagnostics = json.loads(expected_diagnostics_text)
        except json.JSONDecodeError:
            if verbose:
                print(f"{RED}Invalid JSON in .diagnostic file{NC}")
            return False
        
        # Compare diagnostics - they must be exactly the same
        if actual_diagnostics != expected_diagnostics:
            if verbose:
                print(f"{RED}Diagnostics don't match{NC}")
                if len(actual_diagnostics) != len(expected_diagnostics):
                    print(f"  Expected {len(expected_diagnostics)} diagnostics, got {len(actual_diagnostics)}")
                else:
                    print(f"  Diagnostic content differs")
                    import difflib
                    expected_str = json.dumps(expected_diagnostics, indent=2)
                    actual_str = json.dumps(actual_diagnostics, indent=2)
                    diff = difflib.unified_diff(
                        expected_str.splitlines(keepends=True),
                        actual_str.splitlines(keepends=True),
                        fromfile='expected',
                        tofile='actual'
                    )
                    print(''.join(diff))
            return False
        
        return True
    
    def run_diagnostics(self, filter_pattern: Optional[str] = None) -> Tuple[int, int]:
        """Run diagnostic tests. Returns (passed, failed)."""
        print(f"\n{BLUE}Running diagnostic tests...{NC}")
        if filter_pattern:
            print(f"Filter: {filter_pattern}")
        print()
        
        # Find matching diagnostic tests
        pattern = f"*{filter_pattern}*.ml" if filter_pattern else "*.ml"
        test_files = sorted(self.diagnostics_dir.glob(pattern))
        
        # Filter out .diagnostic files
        test_files = [f for f in test_files if not str(f).endswith('.diagnostic')]
        
        if not test_files:
            print(f"{YELLOW}No diagnostic tests found matching: {pattern}{NC}")
            return 0, 0
        
        passed = 0
        failed = 0
        
        for test_file in test_files:
            basename = test_file.name
            
            # Check if it's a TBD test
            diagnostic_path = Path(str(test_file) + ".diagnostic")
            is_tbd = False
            if diagnostic_path.exists():
                with open(diagnostic_path, 'r') as f:
                    is_tbd = "TBD" in f.read()
            
            result = self.test_diagnostic(test_file)
            
            if result:
                if is_tbd:
                    print(f"{basename}... {YELLOW}SKIPPED{NC} (TBD)")
                else:
                    print(f"{basename}... {GREEN}PASSED{NC}")
                passed += 1
            else:
                print(f"{basename}... {RED}FAILED{NC}")
                failed += 1
        
        print(f"\n{BLUE}Results:{NC} {passed} passed, {failed} failed")
        return passed, failed
    
    def run_codebase(self, verbose: bool = False) -> Tuple[int, int]:
        """Test parser on entire codebase. Returns (passed, failed)."""
        print(f"\n{BLUE}Testing syn parser on entire codebase...{NC}\n")
        
        # Find all .ml and .mli files
        ml_files = []
        for ext in ["*.ml", "*.mli"]:
            ml_files.extend(self.packages_dir.glob(f"**/{ext}"))
        
        # Filter out test fixtures and generated files
        ml_files = [
            f for f in ml_files
            if "/tests/fixtures/" not in str(f) and "/tests/generated/" not in str(f)
        ]
        
        ml_files = sorted(ml_files)
        
        if not ml_files:
            print(f"{YELLOW}No source files found in packages/{NC}")
            return 0, 0
        
        print(f"Found {len(ml_files)} files to test\n")
        
        passed = 0
        failed = 0
        failed_files = []
        
        for file_path in ml_files:
            relative_path = file_path.relative_to(self.workspace_root)
            
            covered, total = self.debug_file(file_path, verbose=False)
            
            if covered == total:
                print(f"{GREEN}✓{NC} {relative_path} ({covered}/{total} bytes)")
                passed += 1
            else:
                missing = total - covered
                print(f"{RED}✗{NC} {relative_path} ({covered}/{total} bytes, {missing} missing)")
                failed += 1
                failed_files.append((relative_path, covered, total))
        
        print(f"\n{BLUE}========================================={NC}")
        print(f"{BLUE}Results:{NC} {passed} passed, {failed} failed")
        print(f"{BLUE}========================================={NC}")
        
        if failed_files:
            print(f"\n{YELLOW}Failed files:{NC}")
            for path, covered, total in failed_files:
                missing = total - covered
                pct = (covered / total * 100) if total > 0 else 0
                print(f"  - {path}: {covered}/{total} bytes ({pct:.2f}%, {missing} missing)")
        
        return passed, failed

def main():
    parser = argparse.ArgumentParser(
        description="Unified test runner for syn parser",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    
    parser.add_argument(
        "command",
        choices=["fixtures", "diagnostics", "codebase", "debug", "all"],
        help="Test mode to run"
    )
    
    parser.add_argument(
        "file",
        nargs="?",
        help="File to debug (for 'debug' command)"
    )
    
    parser.add_argument(
        "--filter",
        help="Filter pattern for fixtures (e.g., '08' or 'type')"
    )
    
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Verbose output"
    )
    
    args = parser.parse_args()
    
    # Find workspace root
    script_dir = Path(__file__).parent
    workspace_root = script_dir.parent.parent.parent
    
    runner = TestRunner(workspace_root)
    
    # Check if syn binary exists
    if not runner.syn_binary.exists():
        print(f"{RED}Error: syn binary not found at {runner.syn_binary}{NC}")
        print(f"{YELLOW}Run: tusk build -p syn{NC}")
        sys.exit(1)
    
    exit_code = 0
    
    if args.command == "debug":
        if not args.file:
            print(f"{RED}Error: 'debug' command requires a file argument{NC}")
            sys.exit(1)
        
        file_path = Path(args.file)
        if not file_path.exists():
            print(f"{RED}Error: File not found: {file_path}{NC}")
            sys.exit(1)
        
        runner.debug_file(file_path, verbose=True)
    
    elif args.command == "fixtures":
        passed, failed = runner.run_fixtures(args.filter)
        if failed > 0:
            exit_code = 1
    
    elif args.command == "diagnostics":
        passed, failed = runner.run_diagnostics(args.filter)
        if failed > 0:
            exit_code = 1
    
    elif args.command == "codebase":
        passed, failed = runner.run_codebase(args.verbose)
        if failed > 0:
            exit_code = 1
    
    elif args.command == "all":
        print(f"{BLUE}Running all tests{NC}")
        print("=" * 80)
        
        # Run fixtures
        fix_passed, fix_failed = runner.run_fixtures()
        
        # Run diagnostics
        diag_passed, diag_failed = runner.run_diagnostics()
        
        # Run codebase
        code_passed, code_failed = runner.run_codebase(args.verbose)
        
        # Summary
        total_passed = fix_passed + diag_passed + code_passed
        total_failed = fix_failed + diag_failed + code_failed
        
        print(f"\n{BLUE}{'=' * 80}{NC}")
        print(f"{BLUE}OVERALL RESULTS{NC}")
        print(f"{BLUE}{'=' * 80}{NC}")
        print(f"Fixtures: {fix_passed} passed, {fix_failed} failed")
        print(f"Diagnostics: {diag_passed} passed, {diag_failed} failed")
        print(f"Codebase: {code_passed} passed, {code_failed} failed")
        print(f"{BLUE}Total: {total_passed} passed, {total_failed} failed{NC}")
        
        if total_failed > 0:
            exit_code = 1
    
    sys.exit(exit_code)

if __name__ == "__main__":
    main()
