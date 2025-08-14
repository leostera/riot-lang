#!/usr/bin/env python3
import subprocess
import sys
import time

def test_merlin_bridge():
    # Start the merlin bridge process
    proc = subprocess.Popen(
        ['./target/debug/tusk', 'lsp', 'ocaml-merlin'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=0  # Unbuffered
    )
    
    # Test commands to send (these are Csexp-encoded commands)
    # Format: (length:atomlength:atom) for a list of atoms
    test_commands = [
        '(4:File25:packages/tusk/src/main.ml)',   # File command (25 chars)
        '(4:File24:packages/sexp/src/lib.ml)',    # Another file (24 chars)
        '(4:Halt)',                                # Halt command
    ]
    
    print("Testing merlin bridge...")
    print("-" * 50)
    
    for cmd in test_commands:
        print(f"Sending: {cmd}")
        
        # Send command with newline
        proc.stdin.write(cmd + '\n')
        proc.stdin.flush()
        
        # Read response (should be a single line)
        try:
            response = proc.stdout.readline()
            if response:
                print(f"Response: {response.strip()}")
            else:
                print("No response received")
        except Exception as e:
            print(f"Error reading response: {e}")
        
        print("-" * 50)
        
        # Check if process has exited after Halt
        if 'Halt' in cmd:
            time.sleep(0.1)  # Give it a moment to exit
            if proc.poll() is not None:
                print(f"Process exited with code: {proc.poll()}")
                break
    
    # Clean up
    if proc.poll() is None:
        proc.terminate()
        proc.wait()
    
    print("Test complete!")

if __name__ == "__main__":
    test_merlin_bridge()