# Check if our bytecode opcodes are correct
# From OCaml's instruct.h

opcodes = {
    0x00: "ACC0",
    0x09: "PUSH",
    0x31: "C_CALL1", 
    0x5B: "CONSTINT",
    0x5E: "ADDINT",
    0x7F: "STOP",
}

print("Hello example: [0x5B, 0x2A, 0x31, 0x00, 0x7F]")
print("  0x5B (91) = CONSTINT")
print("  0x2A (42) = argument 42")
print("  0x31 (49) = C_CALL1")
print("  0x00 (0)  = primitive index 0")
print("  0x7F (127) = STOP")
print()

print("Math example: [0x5B, 0x0A, 0x09, 0x5B, 0x14, 0x5E, 0x09, 0x5B, 0x0C, 0x5E, 0x31, 0x00, 0x7F]")
print("  0x5B (91) = CONSTINT")
print("  0x0A (10) = 10")
print("  0x09 (9)  = PUSH")
print("  0x5B (91) = CONSTINT")
print("  0x14 (20) = 20")
print("  0x5E (94) = ADDINT")
print("  0x09 (9)  = PUSH")
print("  0x5B (91) = CONSTINT")
print("  0x0C (12) = 12")
print("  0x5E (94) = ADDINT")
print("  0x31 (49) = C_CALL1")
print("  0x00 (0)  = primitive index 0")
print("  0x7F (127) = STOP")
