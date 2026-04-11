#include <caml/alloc.h>
#include <caml/config.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <stdint.h>

static inline uint32_t serde_bin_load_u32_le(const unsigned char *src) {
  return ((uint32_t)src[0])
    | ((uint32_t)src[1] << 8)
    | ((uint32_t)src[2] << 16)
    | ((uint32_t)src[3] << 24);
}

static inline uint64_t serde_bin_load_u64_le(const unsigned char *src) {
  return ((uint64_t)src[0])
    | ((uint64_t)src[1] << 8)
    | ((uint64_t)src[2] << 16)
    | ((uint64_t)src[3] << 24)
    | ((uint64_t)src[4] << 32)
    | ((uint64_t)src[5] << 40)
    | ((uint64_t)src[6] << 48)
    | ((uint64_t)src[7] << 56);
}

static inline void serde_bin_store_u32_le(unsigned char *dst, uint32_t value) {
  dst[0] = (unsigned char)(value & 0xFFu);
  dst[1] = (unsigned char)((value >> 8) & 0xFFu);
  dst[2] = (unsigned char)((value >> 16) & 0xFFu);
  dst[3] = (unsigned char)((value >> 24) & 0xFFu);
}

static inline void serde_bin_store_u64_le(unsigned char *dst, uint64_t value) {
  dst[0] = (unsigned char)(value & 0xFFu);
  dst[1] = (unsigned char)((value >> 8) & 0xFFu);
  dst[2] = (unsigned char)((value >> 16) & 0xFFu);
  dst[3] = (unsigned char)((value >> 24) & 0xFFu);
  dst[4] = (unsigned char)((value >> 32) & 0xFFu);
  dst[5] = (unsigned char)((value >> 40) & 0xFFu);
  dst[6] = (unsigned char)((value >> 48) & 0xFFu);
  dst[7] = (unsigned char)((value >> 56) & 0xFFu);
}

static value serde_bin_val_long_u32(uint32_t value) {
#ifdef ARCH_SIXTYFOUR
  return Val_long((intnat)value);
#else
  if (value > (uint32_t)Max_long) {
    caml_invalid_argument("serde-bin decoded u32 does not fit in an OCaml int");
  }
  return Val_long((intnat)value);
#endif
}

CAMLprim value serde_bin_write_u32_le(value dst, value off, value value32) {
  unsigned char *ptr = (unsigned char *)Bytes_val(dst) + Long_val(off);
  serde_bin_store_u32_le(ptr, (uint32_t)Long_val(value32));
  return Val_unit;
}

CAMLprim value serde_bin_write_i32_le(value dst, value off, value value32) {
  unsigned char *ptr = (unsigned char *)Bytes_val(dst) + Long_val(off);
  serde_bin_store_u32_le(ptr, (uint32_t)Int32_val(value32));
  return Val_unit;
}

CAMLprim value serde_bin_write_i64_le(value dst, value off, value value64) {
  unsigned char *ptr = (unsigned char *)Bytes_val(dst) + Long_val(off);
  serde_bin_store_u64_le(ptr, (uint64_t)Int64_val(value64));
  return Val_unit;
}

CAMLprim value serde_bin_read_u32_le_string(value src, value off) {
  const unsigned char *ptr = (const unsigned char *)String_val(src) + Long_val(off);
  return serde_bin_val_long_u32(serde_bin_load_u32_le(ptr));
}

CAMLprim value serde_bin_read_u32_le_bytes(value src, value off) {
  const unsigned char *ptr = (const unsigned char *)Bytes_val(src) + Long_val(off);
  return serde_bin_val_long_u32(serde_bin_load_u32_le(ptr));
}

CAMLprim value serde_bin_read_i32_le_string(value src, value off) {
  const unsigned char *ptr = (const unsigned char *)String_val(src) + Long_val(off);
  return caml_copy_int32((int32_t)serde_bin_load_u32_le(ptr));
}

CAMLprim value serde_bin_read_i32_le_bytes(value src, value off) {
  const unsigned char *ptr = (const unsigned char *)Bytes_val(src) + Long_val(off);
  return caml_copy_int32((int32_t)serde_bin_load_u32_le(ptr));
}

CAMLprim value serde_bin_read_i64_le_string(value src, value off) {
  const unsigned char *ptr = (const unsigned char *)String_val(src) + Long_val(off);
  return caml_copy_int64((int64_t)serde_bin_load_u64_le(ptr));
}

CAMLprim value serde_bin_read_i64_le_bytes(value src, value off) {
  const unsigned char *ptr = (const unsigned char *)Bytes_val(src) + Long_val(off);
  return caml_copy_int64((int64_t)serde_bin_load_u64_le(ptr));
}
