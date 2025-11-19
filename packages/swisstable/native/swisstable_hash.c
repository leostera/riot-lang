/* SwissTable hash functions - FNV-1a based hashing */

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/hash.h>
#include <stdint.h>

/* FNV-1a constants for 64-bit hash */
#define FNV_OFFSET_BASIS_64 14695981039346656037ULL
#define FNV_PRIME_64 1099511628211ULL

/* Fast path hash for integer keys
 * OCaml integers are tagged: LSB=1 for immediate ints
 * We can detect and hash them directly without polymorphic hashing */
CAMLprim value swisstable_hash_int(value v) {
  CAMLparam1(v);
  /* For integers, just use the value directly with mixing */
  intnat val_int = Long_val(v);
  
  /* Simple mixing for better distribution */
  uint64_t h = (uint64_t)val_int;
  h ^= h >> 33;
  h *= 0xff51afd7ed558ccdULL;
  h ^= h >> 33;
  h *= 0xc4ceb9fe1a85ec53ULL;
  h ^= h >> 33;
  
  CAMLreturn(Val_long(h & 0x7FFFFFFFFFFFFFFFLL));
}

/* Polymorphic hash function using OCaml's built-in hash
 * NOTE: This function is no longer used - we call Hashtbl.hash directly from OCaml
 * for better structural equality support. Keeping this for potential future use. */
CAMLprim value swisstable_hash(value v) {
  CAMLparam1(v);
  
  /* Fast path for integers (check if tagged as immediate int) */
  if (Is_long(v)) {
    /* It's an immediate integer - use fast hash */
    intnat val_int = Long_val(v);
    uint64_t h = (uint64_t)val_int;
    h ^= h >> 33;
    h *= 0xff51afd7ed558ccdULL;
    h ^= h >> 33;
    h *= 0xc4ceb9fe1a85ec53ULL;
    h ^= h >> 33;
    CAMLreturn(Val_long(h & 0x7FFFFFFFFFFFFFFFLL));
  }
  
  /* Use OCaml's polymorphic hash function for other types */
  intnat h = caml_hash_mix_intnat(0, v);
  /* Convert to positive int (OCaml ints are 63-bit on 64-bit platforms) */
  CAMLreturn(Val_long(h & 0x7FFFFFFFFFFFFFFFLL));
}

/* Extract h1 - bucket index from hash */
CAMLprim value swisstable_h1(value hash_val, value bucket_mask_val) {
  CAMLparam2(hash_val, bucket_mask_val);
  intnat hash = Long_val(hash_val);
  intnat bucket_mask = Long_val(bucket_mask_val);
  intnat h1 = hash & bucket_mask;
  CAMLreturn(Val_long(h1));
}

/* Extract h2 - tag from hash (top 7 bits) */
CAMLprim value swisstable_h2(value hash_val) {
  CAMLparam1(hash_val);
  intnat hash = Long_val(hash_val);
  /* Get bits from a meaningful position - use bits 20-26 for better distribution */
  int h2 = (hash >> 20) & 0x7F;
  CAMLreturn(Val_int(h2));
}

/* Mix hash bits for better distribution */
CAMLprim value swisstable_hash_mix(value hash_val) {
  CAMLparam1(hash_val);
  uint64_t h = (uint64_t)Long_val(hash_val);
  
  /* Simple mixing function from hashbrown */
  h ^= h >> 33;
  h *= 0xff51afd7ed558ccdULL;
  h ^= h >> 33;
  h *= 0xc4ceb9fe1a85ec53ULL;
  h ^= h >> 33;
  
  CAMLreturn(Val_long(h & 0x7FFFFFFFFFFFFFFFLL));
}
