# OCaml Runtime Features - Implementation Checklist

Based on analysis of the OCaml 5.3.0 runtime (~69 C files, ~38K LOC).

## Core Value System
- [x] Tagged integers (1-bit LSB tagging)
- [x] Block pointers (heap-allocated values)
- [x] Block headers (size, tag, GC color)
- [x] Special values (unit, true, false, empty list, None)
- [ ] Float representation (boxed doubles)
- [ ] String representation (byte sequences)
- [ ] Custom blocks (opaque C data)

## Memory Management

### Allocation
- [x] Minor heap (bump allocator)
- [x] Major heap (free-list allocator)
- [ ] Block allocation fast path
- [ ] Large block allocation
- [ ] Atom table (pre-allocated empty blocks)
- [ ] Out-of-heap values
- [ ] Allocation profiling hooks

### Minor GC
- [ ] Stop-and-copy collection
- [ ] Root scanning (stack, registers, globals)
- [ ] Remembered set (old→young pointers)
- [ ] Promotion to major heap
- [ ] Minor GC statistics
- [ ] Configurable minor heap size

### Major GC
- [ ] Tri-color marking (white/gray/black)
- [ ] Mark phase (root scanning)
- [ ] Sweep phase (free dead blocks)
- [ ] Incremental collection
- [ ] Concurrent collection
- [ ] Write barriers (for concurrent GC)
- [ ] Compaction (defragmentation)
- [ ] Major GC statistics
- [ ] GC pacing/tuning

### Finalization
- [ ] Finalizer registration
- [ ] Two-phase finalization
- [ ] Resurrection detection
- [ ] Weak references
- [ ] Ephemerons (key-value weak refs)

## Bytecode Interpreter

### Core Instructions (100+ opcodes)
- [x] ACC0, ACC1, ACC (stack access)
- [x] PUSH, PUSHACC0
- [x] POP
- [ ] ASSIGN (stack write)
- [ ] ENVACC (environment access)
- [ ] PUSHENVACC
- [ ] OFFSETCLOSURE

### Constants
- [x] CONST0, CONST1, CONST2, CONST3
- [x] CONSTINT
- [ ] ATOM0, ATOM (empty blocks)
- [ ] GETGLOBAL, SETGLOBAL
- [ ] GETGLOBALFIELD
- [ ] PUSHGETGLOBAL
- [ ] PUSHGETGLOBALFIELD

### Arithmetic
- [x] NEGINT
- [x] ADDINT, SUBINT, MULINT, DIVINT
- [ ] MODINT
- [ ] ANDINT, ORINT, XORINT
- [ ] LSLINT, LSRINT, ASRINT (bit shifts)
- [ ] OFFSETINT (add constant)
- [ ] OFFSETREF (increment ref cell)

### Comparisons
- [ ] EQ, NEQ
- [ ] LTINT, LEINT, GTINT, GEINT
- [ ] ULTINT, UGEINT (unsigned)
- [ ] BOOLNOT
- [ ] COMPARE (polymorphic compare)

### Blocks
- [x] MAKEBLOCK, MAKEBLOCK1, MAKEBLOCK2, MAKEBLOCK3
- [x] GETFIELD0, GETFIELD1, GETFIELD
- [x] SETFIELD0, SETFIELD
- [ ] MAKEFLOATBLOCK
- [ ] GETFLOATFIELD, SETFLOATFIELD
- [ ] GETVECTITEM, SETVECTITEM
- [ ] GETSTRINGCHAR, SETSTRINGCHAR
- [ ] GETBYTESCHAR, SETBYTESCHAR
- [ ] VECTLENGTH, GETVECTLENGTH

### Control Flow
- [ ] BRANCH (unconditional jump)
- [ ] BRANCHIF, BRANCHIFNOT
- [ ] SWITCH (multi-way branch)
- [ ] BOOLNOT
- [ ] PUSHTRAP (exception handler)
- [ ] POPTRAP
- [ ] RAISE, RERAISE, RAISE_NOTRACE

### Function Calls
- [ ] PUSH_RETADDR
- [ ] APPLY, APPLY1, APPLY2, APPLY3
- [ ] APPTERM, APPTERM1, APPTERM2, APPTERM3 (tail calls)
- [ ] RETURN
- [ ] RESTART (currying)
- [ ] GRAB (partial application)
- [ ] CLOSURE
- [ ] CLOSUREREC (recursive closures)
- [ ] OFFSETCLOSURE, OFFSETCLOSUREM2, etc.
- [ ] GETMETHOD (object method lookup)
- [ ] GETPUBMET, GETDYNMET

### C Calls (FFI)
- [ ] C_CALL1, C_CALL2, C_CALL3, C_CALL4, C_CALL5
- [ ] C_CALLN
- [ ] Primitive table loading
- [ ] Root registration (CAMLparam/CAMLlocal equivalent)

### Effect Handlers
- [ ] PERFORM (perform effect)
- [ ] RESUME (resume continuation)
- [ ] RESUMETERM (tail resume)
- [ ] REPERFORMTERM (tail re-perform)
- [ ] Stack switching
- [ ] Continuation capture
- [ ] Stack pool/cache

### Special
- [ ] STOP (halt interpreter)
- [ ] EVENT (debugger breakpoint)
- [ ] BREAK (debugger)
- [ ] CHECKSIGNALS (async signal handling)

## Standard Primitives

### String Operations
- [ ] caml_ml_string_length
- [ ] caml_string_get
- [ ] caml_string_set
- [ ] caml_string_equal
- [ ] caml_string_compare
- [ ] caml_string_notequal
- [ ] caml_string_lessequal
- [ ] caml_string_lessthan
- [ ] caml_string_greaterequal
- [ ] caml_string_greaterthan
- [ ] caml_blit_string
- [ ] caml_fill_string
- [ ] caml_create_string (deprecated)
- [ ] caml_ml_bytes_length
- [ ] caml_bytes_get
- [ ] caml_bytes_set
- [ ] caml_bytes_equal
- [ ] caml_bytes_compare

### Array Operations
- [ ] caml_array_length
- [ ] caml_array_get
- [ ] caml_array_set
- [ ] caml_array_unsafe_get
- [ ] caml_array_unsafe_set
- [ ] caml_make_vect
- [ ] caml_make_array
- [ ] caml_array_blit
- [ ] caml_array_sub
- [ ] caml_array_append
- [ ] caml_array_concat
- [ ] caml_floatarray_get
- [ ] caml_floatarray_set
- [ ] caml_floatarray_create
- [ ] caml_make_float_vect

### Integer Operations (Int32/Int64/Nativeint)
- [ ] caml_int32_neg, caml_int64_neg
- [ ] caml_int32_add, caml_int64_add
- [ ] caml_int32_sub, caml_int64_sub
- [ ] caml_int32_mul, caml_int64_mul
- [ ] caml_int32_div, caml_int64_div
- [ ] caml_int32_mod, caml_int64_mod
- [ ] caml_int32_and, caml_int64_and
- [ ] caml_int32_or, caml_int64_or
- [ ] caml_int32_xor, caml_int64_xor
- [ ] caml_int32_shift_left, caml_int64_shift_left
- [ ] caml_int32_shift_right, caml_int64_shift_right
- [ ] caml_int32_shift_right_unsigned, caml_int64_shift_right_unsigned
- [ ] caml_int32_of_int, caml_int64_of_int
- [ ] caml_int32_to_int, caml_int64_to_int
- [ ] caml_int32_of_float, caml_int64_of_float
- [ ] caml_int32_to_float, caml_int64_to_float
- [ ] caml_int32_compare, caml_int64_compare
- [ ] caml_int32_format, caml_int64_format
- [ ] caml_int32_of_string, caml_int64_of_string
- [ ] caml_int32_bits_of_float, caml_int64_bits_of_float
- [ ] caml_int32_float_of_bits, caml_int64_float_of_bits

### Float Operations
- [ ] caml_neg_float
- [ ] caml_add_float
- [ ] caml_sub_float
- [ ] caml_mul_float
- [ ] caml_div_float
- [ ] caml_exp_float
- [ ] caml_floor_float
- [ ] caml_fmod_float
- [ ] caml_frexp_float
- [ ] caml_ldexp_float
- [ ] caml_log_float
- [ ] caml_log10_float
- [ ] caml_modf_float
- [ ] caml_sqrt_float
- [ ] caml_power_float
- [ ] caml_sin_float, caml_cos_float, caml_tan_float
- [ ] caml_asin_float, caml_acos_float, caml_atan_float
- [ ] caml_atan2_float
- [ ] caml_sinh_float, caml_cosh_float, caml_tanh_float
- [ ] caml_asinh_float, caml_acosh_float, caml_atanh_float
- [ ] caml_ceil_float
- [ ] caml_hypot_float
- [ ] caml_expm1_float, caml_log1p_float
- [ ] caml_copysign_float
- [ ] caml_signbit_float
- [ ] caml_eq_float, caml_neq_float
- [ ] caml_le_float, caml_lt_float
- [ ] caml_ge_float, caml_gt_float
- [ ] caml_float_compare
- [ ] caml_float_of_int
- [ ] caml_int_of_float
- [ ] caml_format_float
- [ ] caml_float_of_string
- [ ] caml_classify_float

### I/O Operations
- [ ] caml_ml_open_descriptor_in
- [ ] caml_ml_open_descriptor_out
- [ ] caml_ml_out_channels_list
- [ ] caml_ml_close_channel
- [ ] caml_ml_channel_size
- [ ] caml_ml_channel_size_64
- [ ] caml_ml_set_binary_mode
- [ ] caml_ml_flush
- [ ] caml_ml_output_char
- [ ] caml_ml_output_int
- [ ] caml_ml_output_partial
- [ ] caml_ml_output
- [ ] caml_ml_seek_out
- [ ] caml_ml_seek_out_64
- [ ] caml_ml_pos_out
- [ ] caml_ml_pos_out_64
- [ ] caml_ml_input_char
- [ ] caml_ml_input_int
- [ ] caml_ml_input
- [ ] caml_ml_seek_in
- [ ] caml_ml_seek_in_64
- [ ] caml_ml_pos_in
- [ ] caml_ml_pos_in_64
- [ ] caml_ml_input_scan_line
- [ ] caml_ml_set_channel_name

### System Operations
- [ ] caml_sys_exit
- [ ] caml_sys_open
- [ ] caml_sys_close
- [ ] caml_sys_file_exists
- [ ] caml_sys_is_directory
- [ ] caml_sys_remove
- [ ] caml_sys_rename
- [ ] caml_sys_chdir
- [ ] caml_sys_getcwd
- [ ] caml_sys_getenv
- [ ] caml_sys_get_argv
- [ ] caml_sys_get_config
- [ ] caml_sys_random_seed
- [ ] caml_sys_const_big_endian
- [ ] caml_sys_const_word_size
- [ ] caml_sys_const_int_size
- [ ] caml_sys_const_max_wosize
- [ ] caml_sys_const_ostype_unix
- [ ] caml_sys_const_ostype_win32
- [ ] caml_sys_const_ostype_cygwin
- [ ] caml_sys_const_backend_type
- [ ] caml_sys_read_directory
- [ ] caml_sys_time
- [ ] caml_sys_time_include_children

### Hashing
- [ ] caml_hash
- [ ] caml_hash_mix_int
- [ ] caml_hash_mix_intnat
- [ ] caml_hash_mix_int64
- [ ] caml_hash_mix_float
- [ ] caml_hash_mix_string
- [ ] caml_hash_mix_bytes

### Comparison
- [ ] caml_compare
- [ ] caml_equal
- [ ] caml_notequal
- [ ] caml_lessthan
- [ ] caml_lessequal
- [ ] caml_greaterthan
- [ ] caml_greaterequal
- [ ] caml_compare_val (internal)
- [ ] caml_int_compare
- [ ] caml_float_compare_unboxed

### Marshal (Serialization)
- [ ] caml_output_value (extern.c)
- [ ] caml_output_value_to_string
- [ ] caml_output_value_to_bytes
- [ ] caml_output_value_to_buffer
- [ ] caml_input_value (intern.c)
- [ ] caml_input_value_from_string
- [ ] caml_input_value_from_bytes
- [ ] caml_marshal_data_size
- [ ] Value sharing/back-references
- [ ] External primitive references

### Objects
- [ ] caml_get_public_method
- [ ] caml_get_method
- [ ] caml_get_method_label
- [ ] caml_set_oo_id

### Weak References
- [ ] caml_weak_create
- [ ] caml_weak_set
- [ ] caml_weak_get
- [ ] caml_weak_get_copy
- [ ] caml_weak_check
- [ ] caml_weak_blit

### Lazy Values
- [ ] caml_lazy_make_forward
- [ ] caml_lazy_reset_to_lazy
- [ ] caml_lazy_follow_forward
- [ ] caml_lazy_is_val
- [ ] caml_lazy_read

### Callbacks (OCaml calling from C)
- [ ] caml_callback
- [ ] caml_callback2
- [ ] caml_callback3
- [ ] caml_callbackN
- [ ] caml_callback_exn
- [ ] caml_callback2_exn
- [ ] caml_callback3_exn
- [ ] caml_callbackN_exn
- [ ] caml_register_named_value
- [ ] caml_named_value

### Exceptions
- [ ] caml_raise_exception
- [ ] caml_raise_constant
- [ ] caml_raise_with_arg
- [ ] caml_raise_with_args
- [ ] caml_raise_with_string
- [ ] caml_failwith
- [ ] caml_invalid_argument
- [ ] caml_raise_out_of_memory
- [ ] caml_raise_stack_overflow
- [ ] caml_raise_sys_error
- [ ] caml_raise_end_of_file
- [ ] caml_raise_zero_divide
- [ ] caml_raise_not_found
- [ ] caml_raise_sys_blocked_io

### Backtrace
- [ ] caml_record_backtrace
- [ ] caml_backtrace_status
- [ ] caml_get_exception_backtrace
- [ ] caml_get_exception_raw_backtrace
- [ ] caml_convert_raw_backtrace
- [ ] caml_raw_backtrace_length
- [ ] caml_raw_backtrace_slot
- [ ] caml_raw_backtrace_next_slot

### MD5/Hashing
- [ ] caml_md5_string
- [ ] caml_md5_chan
- [ ] caml_blake2_string
- [ ] caml_blake2_bytes

### Lexing
- [ ] caml_lex_engine
- [ ] caml_new_lex_engine

### Parsing
- [ ] caml_parse_engine
- [ ] caml_set_parser_trace

### GC Control
- [ ] caml_gc_stat
- [ ] caml_gc_quick_stat
- [ ] caml_gc_counters
- [ ] caml_gc_get
- [ ] caml_gc_set
- [ ] caml_gc_minor
- [ ] caml_gc_major
- [ ] caml_gc_full_major
- [ ] caml_gc_compaction
- [ ] caml_gc_major_slice
- [ ] caml_gc_huge_fallback_count
- [ ] caml_get_minor_free
- [ ] caml_get_major_bucket
- [ ] caml_get_major_credit

### Memory Profiling
- [ ] caml_memprof_set
- [ ] caml_memprof_start
- [ ] caml_memprof_stop
- [ ] caml_memprof_discard

### Runtime Events
- [ ] caml_runtime_events_create_cursor
- [ ] caml_runtime_events_free_cursor
- [ ] caml_runtime_events_read_poll

### Meta (Object introspection)
- [ ] caml_obj_tag
- [ ] caml_obj_size
- [ ] caml_obj_field
- [ ] caml_obj_set_field
- [ ] caml_obj_block
- [ ] caml_obj_dup
- [ ] caml_obj_truncate
- [ ] caml_obj_add_offset
- [ ] caml_obj_with_tag
- [ ] caml_obj_raw_field
- [ ] caml_obj_set_raw_field
- [ ] caml_obj_make_forward
- [ ] caml_obj_is_block
- [ ] caml_obj_reachable_words

### Random Numbers
- [ ] caml_random_seed
- [ ] caml_random_bits

## Multicore/Concurrency

### Domains
- [ ] Domain spawning
- [ ] Domain joining
- [ ] Domain-local state
- [ ] Domain ID
- [ ] Domain termination
- [ ] Domain recommendations

### Synchronization
- [ ] Stop-the-world barriers
- [ ] Interrupt handling
- [ ] Signal pending checks
- [ ] Async action processing

### Atomic Operations
- [ ] Atomic load
- [ ] Atomic store
- [ ] Atomic exchange
- [ ] Atomic compare-and-set
- [ ] Atomic fetch-and-add
- [ ] Memory barriers

### Fibers
- [ ] Fiber creation
- [ ] Fiber switching
- [ ] Parent/child fiber links
- [ ] Fiber stack allocation
- [ ] Fiber stack pool

## Debugger Support
- [ ] Event recording
- [ ] Breakpoints
- [ ] Single-stepping
- [ ] Stack inspection
- [ ] Value printing
- [ ] Debug info loading

## Dynamic Linking
- [ ] Shared library loading
- [ ] Symbol resolution
- [ ] Code patching
- [ ] Global registration

## Bigarray Support
- [ ] Bigarray creation
- [ ] Bigarray element access
- [ ] Bigarray slicing
- [ ] Multiple precision integers
- [ ] C layout support
- [ ] Fortran layout support

## Platform Support
- [ ] Unix I/O
- [ ] Windows I/O
- [ ] Signal handling (Unix)
- [ ] Signal handling (Windows)
- [ ] Thread support
- [ ] Time functions
- [ ] Path handling

## Advanced Features
- [ ] Code fragments (native)
- [ ] AFL fuzzing support
- [ ] TSAN integration
- [ ] Frame descriptors
- [ ] Instruction tracing
- [ ] Startup hooks
- [ ] Cleanup hooks

## Build Infrastructure
- [ ] Feature configuration
- [ ] Version checking
- [ ] Compatibility layer
- [ ] Installation

---

## Implementation Priority

### Phase 1: Core (Current)
- [x] Value representation
- [x] Basic memory allocation
- [x] Simple interpreter
- [x] Integer arithmetic

### Phase 2: Essential Instructions
- [ ] All stack operations
- [ ] All constant loading
- [ ] Control flow (BRANCH, SWITCH)
- [ ] Comparisons
- [ ] Block operations

### Phase 3: Memory Management
- [ ] Minor GC
- [ ] Major GC mark/sweep
- [ ] Write barriers
- [ ] Root scanning

### Phase 4: Functions
- [ ] APPLY/RETURN
- [ ] CLOSURE creation
- [ ] Currying (GRAB)
- [ ] Tail calls (APPTERM)

### Phase 5: Bytecode Loading
- [ ] File parsing
- [ ] Marshal support
- [ ] Primitive table
- [ ] Global data

### Phase 6: Essential Primitives
- [ ] String operations
- [ ] Array operations
- [ ] I/O basics
- [ ] System operations

### Phase 7: Exceptions
- [ ] RAISE/PUSHTRAP
- [ ] Exception handlers
- [ ] Backtrace support

### Phase 8: Advanced
- [ ] Effect handlers
- [ ] Multicore domains
- [ ] Concurrent GC
- [ ] Full primitive set

---

**Total Features:** ~400+
**Currently Implemented:** ~15 (4%)
**Next Target:** Complete Phase 2 (50+ instructions)
