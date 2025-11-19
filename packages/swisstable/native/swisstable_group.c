/* SwissTable SIMD Group operations - SSE2/NEON optimized */

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <string.h>

/* Platform detection and SIMD includes */
#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
  #define USE_SSE2 1
  #include <emmintrin.h>  /* SSE2 intrinsics */
#elif defined(__aarch64__) || defined(__arm64__)
  #define USE_NEON 1
  #include <arm_neon.h>   /* NEON intrinsics */
#else
  #define USE_GENERIC 1
#endif

/* Group width - number of bytes processed at once */
#if defined(USE_SSE2)
  #define GROUP_WIDTH 16  /* SSE2 processes 16 bytes */
#elif defined(USE_NEON)
  #define GROUP_WIDTH 8   /* NEON processes 8 bytes for compatibility */
#else
  #define GROUP_WIDTH 8   /* Generic fallback */
#endif

/* Tag constants (must match OCaml) */
#define TAG_EMPTY 0xFF
#define TAG_DELETED 0x80

/*
 * Load a group of control bytes from memory
 * 
 * SSE2: Load 16 bytes using _mm_loadu_si128 (unaligned load)
 * NEON: Load 8 bytes using vld1_u8
 * Generic: Load 8 bytes manually
 * 
 * Returns: int64 containing the loaded bytes
 */
CAMLprim value swisstable_group_load(value ctrl_bytes, value idx_val) {
  CAMLparam2(ctrl_bytes, idx_val);
  intnat idx = Long_val(idx_val);
  const uint8_t* ptr = Bytes_val(ctrl_bytes) + idx;
  
#if defined(USE_SSE2)
  /* SSE2: Load 16 bytes and pack into two int64s 
   * For simplicity, we'll just return the lower 8 bytes for now
   * to match the OCaml interface (int64) */
  __m128i data = _mm_loadu_si128((__m128i*)ptr);
  /* Extract lower 64 bits */
  int64_t result = _mm_cvtsi128_si64(data);
  CAMLreturn(caml_copy_int64(result));
  
#elif defined(USE_NEON)
  /* NEON: Load 8 bytes */
  uint8x8_t data = vld1_u8(ptr);
  uint64_t result = vget_lane_u64(vreinterpret_u64_u8(data), 0);
  CAMLreturn(caml_copy_int64(result));
  
#else
  /* Generic: Load 8 bytes manually */
  uint64_t result = 0;
  for (int i = 0; i < 8; i++) {
    result |= ((uint64_t)ptr[i]) << (i * 8);
  }
  CAMLreturn(caml_copy_int64(result));
#endif
}

/*
 * Match a specific tag in the group
 * 
 * SSE2: Use _mm_cmpeq_epi8 for parallel byte comparison
 * NEON: Use vceq_u8 for parallel byte comparison
 * Generic: Manual bit-parallel algorithm
 * 
 * Returns: int bitmask where bit i is set if byte i matches the tag
 */
CAMLprim value swisstable_group_match_tag(value ctrl_bytes, value idx_val, value tag_val) {
  CAMLparam3(ctrl_bytes, idx_val, tag_val);
  intnat idx = Long_val(idx_val);
  int tag = Int_val(tag_val);
  const uint8_t* ptr = Bytes_val(ctrl_bytes) + idx;
  
#if defined(USE_SSE2)
  /* SSE2: Compare all 16 bytes in parallel */
  __m128i data = _mm_loadu_si128((__m128i*)ptr);
  __m128i target = _mm_set1_epi8((char)tag);
  __m128i cmp = _mm_cmpeq_epi8(data, target);
  /* Extract bitmask: bit i is set if byte i matched */
  int mask = _mm_movemask_epi8(cmp);
  /* Only use lower 8 bits to match OCaml interface */
  CAMLreturn(Val_int(mask & 0xFF));
  
#elif defined(USE_NEON)
  /* NEON: Compare all 8 bytes in parallel */
  uint8x8_t data = vld1_u8(ptr);
  uint8x8_t target = vdup_n_u8((uint8_t)tag);
  uint8x8_t cmp = vceq_u8(data, target);
  
  /* Extract bitmask from comparison result */
  /* Each byte in cmp is either 0xFF (match) or 0x00 (no match) */
  /* We need to extract bit 7 from each byte */
  uint8_t mask = 0;
  uint8_t temp[8];
  vst1_u8(temp, cmp);
  for (int i = 0; i < 8; i++) {
    mask |= (temp[i] & 0x80) ? (1 << i) : 0;
  }
  CAMLreturn(Val_int(mask));
  
#else
  /* Generic: Bit-parallel algorithm (same as OCaml version) */
  uint64_t group = 0;
  for (int i = 0; i < 8; i++) {
    group |= ((uint64_t)ptr[i]) << (i * 8);
  }
  
  /* Replicate tag across all bytes */
  uint64_t tag_repeated = (uint64_t)tag;
  tag_repeated |= tag_repeated << 8;
  tag_repeated |= tag_repeated << 16;
  tag_repeated |= tag_repeated << 32;
  
  uint64_t cmp = group ^ tag_repeated;
  uint64_t ones = 0x0101010101010101ULL;
  uint64_t marker = 0x8080808080808080ULL;
  uint64_t result = ((cmp - ones) & ~cmp) & marker;
  
  /* Extract bitmask */
  int mask = 0;
  for (int i = 0; i < 8; i++) {
    mask |= ((result >> (i * 8 + 7)) & 1) << i;
  }
  CAMLreturn(Val_int(mask));
#endif
}

/*
 * Match EMPTY tags (0xFF) in the group
 * 
 * SSE2: Use _mm_cmpeq_epi8 with 0xFF
 * NEON: Use vceq_u8 with 0xFF
 * Generic: Bit-parallel algorithm
 * 
 * Returns: int bitmask where bit i is set if byte i is EMPTY
 */
CAMLprim value swisstable_group_match_empty(value ctrl_bytes, value idx_val) {
  CAMLparam2(ctrl_bytes, idx_val);
  intnat idx = Long_val(idx_val);
  const uint8_t* ptr = Bytes_val(ctrl_bytes) + idx;
  
#if defined(USE_SSE2)
  /* SSE2: Compare with 0xFF */
  __m128i data = _mm_loadu_si128((__m128i*)ptr);
  __m128i empty = _mm_set1_epi8((char)TAG_EMPTY);
  __m128i cmp = _mm_cmpeq_epi8(data, empty);
  int mask = _mm_movemask_epi8(cmp);
  CAMLreturn(Val_int(mask & 0xFF));
  
#elif defined(USE_NEON)
  /* NEON: Compare with 0xFF */
  uint8x8_t data = vld1_u8(ptr);
  uint8x8_t empty = vdup_n_u8(TAG_EMPTY);
  uint8x8_t cmp = vceq_u8(data, empty);
  
  uint8_t mask = 0;
  uint8_t temp[8];
  vst1_u8(temp, cmp);
  for (int i = 0; i < 8; i++) {
    mask |= (temp[i] & 0x80) ? (1 << i) : 0;
  }
  CAMLreturn(Val_int(mask));
  
#else
  /* Generic: Bit-parallel algorithm */
  uint64_t group = 0;
  for (int i = 0; i < 8; i++) {
    group |= ((uint64_t)ptr[i]) << (i * 8);
  }
  
  /* Check if top two bits are both 1 (0xFF = 1111_1111) */
  uint64_t marker = 0x8080808080808080ULL;
  uint64_t result = (group & (group << 1)) & marker;
  
  int mask = 0;
  for (int i = 0; i < 8; i++) {
    mask |= ((result >> (i * 8 + 7)) & 1) << i;
  }
  CAMLreturn(Val_int(mask));
#endif
}

/*
 * Match EMPTY or DELETED tags (high bit set) in the group
 * 
 * SSE2: Use _mm_movemask_epi8 directly (extracts high bits)
 * NEON: Check high bit of each byte
 * Generic: Extract high bits
 * 
 * Returns: int bitmask where bit i is set if byte i is EMPTY or DELETED
 */
CAMLprim value swisstable_group_match_empty_or_deleted(value ctrl_bytes, value idx_val) {
  CAMLparam2(ctrl_bytes, idx_val);
  intnat idx = Long_val(idx_val);
  const uint8_t* ptr = Bytes_val(ctrl_bytes) + idx;
  
#if defined(USE_SSE2)
  /* SSE2: Extract high bits directly - this is the fast path! */
  __m128i data = _mm_loadu_si128((__m128i*)ptr);
  int mask = _mm_movemask_epi8(data);
  CAMLreturn(Val_int(mask & 0xFF));
  
#elif defined(USE_NEON)
  /* NEON: Check high bit of each byte */
  uint8x8_t data = vld1_u8(ptr);
  uint8x8_t highbit = vshr_n_u8(data, 7);
  
  uint8_t mask = 0;
  uint8_t temp[8];
  vst1_u8(temp, highbit);
  for (int i = 0; i < 8; i++) {
    mask |= (temp[i] & 1) << i;
  }
  CAMLreturn(Val_int(mask));
  
#else
  /* Generic: Extract high bits */
  uint8_t mask = 0;
  for (int i = 0; i < 8; i++) {
    mask |= ((ptr[i] >> 7) & 1) << i;
  }
  CAMLreturn(Val_int(mask));
#endif
}

/*
 * Get the width of the group (number of bytes processed at once)
 * This is a constant but exposed as a function for FFI convenience
 */
CAMLprim value swisstable_group_width(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_int(8));  /* Always return 8 for OCaml compatibility */
}

/* ========================================================================
 * High-level search functions - move hot loops to C to reduce FFI overhead
 * ======================================================================== */

/* Helper: Count trailing zeros to find lowest set bit */
static inline int ctz(unsigned int x) {
  if (x == 0) return -1;
#if defined(__GNUC__) || defined(__clang__)
  return __builtin_ctz(x);
#else
  int count = 0;
  while ((x & 1) == 0) {
    x >>= 1;
    count++;
  }
  return count;
#endif
}

/* Helper: Fast SIMD match for a tag - returns bitmask */
static inline unsigned int match_tag_fast(const uint8_t* ctrl, int pos, uint8_t tag) {
#if defined(USE_SSE2)
  __m128i data = _mm_loadu_si128((__m128i*)(ctrl + pos));
  __m128i target = _mm_set1_epi8((char)tag);
  __m128i cmp = _mm_cmpeq_epi8(data, target);
  return _mm_movemask_epi8(cmp) & 0xFF;
#elif defined(USE_NEON)
  uint8x8_t data = vld1_u8(ctrl + pos);
  uint8x8_t target_vec = vdup_n_u8(tag);
  uint8x8_t cmp = vceq_u8(data, target_vec);
  unsigned int mask = 0;
  uint8_t temp[8];
  vst1_u8(temp, cmp);
  for (int i = 0; i < 8; i++) {
    mask |= (temp[i] & 0x80) ? (1 << i) : 0;
  }
  return mask;
#else
  /* Generic fallback */
  unsigned int mask = 0;
  for (int i = 0; i < 8; i++) {
    if (ctrl[pos + i] == tag) {
      mask |= (1 << i);
    }
  }
  return mask;
#endif
}

/* Helper: Fast SIMD match for EMPTY (0xFF) - returns bitmask */
static inline unsigned int match_empty_fast(const uint8_t* ctrl, int pos) {
#if defined(USE_SSE2)
  __m128i data = _mm_loadu_si128((__m128i*)(ctrl + pos));
  __m128i empty = _mm_set1_epi8((char)TAG_EMPTY);
  __m128i cmp = _mm_cmpeq_epi8(data, empty);
  return _mm_movemask_epi8(cmp) & 0xFF;
#elif defined(USE_NEON)
  uint8x8_t data = vld1_u8(ctrl + pos);
  uint8x8_t empty = vdup_n_u8(TAG_EMPTY);
  uint8x8_t cmp = vceq_u8(data, empty);
  unsigned int mask = 0;
  uint8_t temp[8];
  vst1_u8(temp, cmp);
  for (int i = 0; i < 8; i++) {
    mask |= (temp[i] & 0x80) ? (1 << i) : 0;
  }
  return mask;
#else
  unsigned int mask = 0;
  for (int i = 0; i < 8; i++) {
    if (ctrl[pos + i] == TAG_EMPTY) {
      mask |= (1 << i);
    }
  }
  return mask;
#endif
}

/* Helper: Fast SIMD match for EMPTY or DELETED (high bit set) - returns bitmask */
static inline unsigned int match_empty_or_deleted_fast(const uint8_t* ctrl, int pos) {
#if defined(USE_SSE2)
  __m128i data = _mm_loadu_si128((__m128i*)(ctrl + pos));
  return _mm_movemask_epi8(data) & 0xFF;
#elif defined(USE_NEON)
  uint8x8_t data = vld1_u8(ctrl + pos);
  unsigned int mask = 0;
  for (int i = 0; i < 8; i++) {
    mask |= ((ctrl[pos + i] >> 7) & 1) << i;
  }
  return mask;
#else
  unsigned int mask = 0;
  for (int i = 0; i < 8; i++) {
    mask |= ((ctrl[pos + i] >> 7) & 1) << i;
  }
  return mask;
#endif
}

/*
 * Find bucket index for insertion - entire search loop in C
 * 
 * Parameters:
 *   ctrl_bytes: control byte array
 *   hash: hash value
 *   bucket_mask: mask for wrapping bucket indices
 * 
 * Returns: bucket index to insert at, or -1 if table is full
 */
CAMLprim value swisstable_find_insert_slot(value ctrl_bytes, value hash_val, value bucket_mask_val) {
  CAMLparam3(ctrl_bytes, hash_val, bucket_mask_val);
  
  const uint8_t* ctrl = Bytes_val(ctrl_bytes);
  intnat hash = Long_val(hash_val);
  intnat bucket_mask = Long_val(bucket_mask_val);
  intnat max_probes = ((bucket_mask + 1) / GROUP_WIDTH) + 1;
  
  /* Triangular probing */
  intnat pos = hash & bucket_mask;
  intnat stride = 0;
  
  for (intnat probes = 0; probes < max_probes; probes++) {
    /* Prefetch next probe position (read-only, temporal locality) */
    intnat next_stride = stride + GROUP_WIDTH;
    intnat next_pos = (pos + next_stride + GROUP_WIDTH) & bucket_mask;
    __builtin_prefetch(&ctrl[next_pos], 0, 3);
    
    unsigned int empties = match_empty_or_deleted_fast(ctrl, pos);
    
    if (empties != 0) {
      /* Found an empty/deleted slot */
      int offset = ctz(empties);
      intnat idx = (pos + offset) & bucket_mask;
      CAMLreturn(Val_long(idx));
    }
    
    /* Move to next probe position */
    stride += GROUP_WIDTH;
    pos = (pos + stride) & bucket_mask;
  }
  
  /* Table is full - shouldn't happen */
  CAMLreturn(Val_long(-1));
}

/*
 * Find matching slots for a given h2 tag - returns list of candidate indices
 * 
 * This function finds all bucket indices that match the h2 tag in the control array.
 * OCaml code will then check which one has the matching key.
 * 
 * Parameters:
 *   ctrl_bytes: control byte array
 *   hash: hash value (for initial position)
 *   h2: tag to match (7-bit hash fragment)
 *   bucket_mask: mask for wrapping bucket indices
 * 
 * Returns: OCaml list of bucket indices to check (in probe order)
 */
CAMLprim value swisstable_find_candidates(value ctrl_bytes, value hash_val, value h2_val, value bucket_mask_val) {
  CAMLparam4(ctrl_bytes, hash_val, h2_val, bucket_mask_val);
  CAMLlocal2(result, cons);
  
  const uint8_t* ctrl = Bytes_val(ctrl_bytes);
  intnat hash = Long_val(hash_val);
  int h2 = Int_val(h2_val);
  intnat bucket_mask = Long_val(bucket_mask_val);
  intnat max_probes = ((bucket_mask + 1) / GROUP_WIDTH) + 1;
  
  result = Val_emptylist;  /* Start with empty list */
  
  /* Triangular probing */
  intnat pos = hash & bucket_mask;
  intnat stride = 0;
  
  for (intnat probes = 0; probes < max_probes; probes++) {
    /* Prefetch next probe position (read-only, temporal locality) */
    intnat next_stride = stride + GROUP_WIDTH;
    intnat next_pos = (pos + next_stride + GROUP_WIDTH) & bucket_mask;
    __builtin_prefetch(&ctrl[next_pos], 0, 3);
    
    unsigned int matches = match_tag_fast(ctrl, pos, (uint8_t)h2);
    
    /* Check each match and add to result list */
    while (matches != 0) {
      int offset = ctz(matches);
      intnat idx = (pos + offset) & bucket_mask;
      
      /* Prepend idx to result list: cons(idx, result) */
      cons = caml_alloc(2, 0);  /* Allocate cons cell */
      Store_field(cons, 0, Val_long(idx));  /* head = idx */
      Store_field(cons, 1, result);         /* tail = result */
      result = cons;
      
      /* Remove this bit from mask */
      matches &= matches - 1;
    }
    
    /* Check for EMPTY - if found, stop searching */
    unsigned int empties = match_empty_fast(ctrl, pos);
    if (empties != 0) {
      break;  /* Key doesn't exist past this point */
    }
    
    /* Move to next probe position */
    stride += GROUP_WIDTH;
    pos = (pos + stride) & bucket_mask;
  }
  
  /* Return list in reverse probe order (most recent probes first) */
  CAMLreturn(result);
}
