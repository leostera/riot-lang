/* Std HashMap SwissTable probe helpers. */

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>

#if defined(__aarch64__) || defined(__arm64__)
#define STD_HASHMAP_USE_NEON 1
#include <arm_neon.h>
#endif

#if defined(__GNUC__) || defined(__clang__)
#define STD_HASHMAP_PREFETCH_READ(addr) __builtin_prefetch((addr), 0, 3)
#else
#define STD_HASHMAP_PREFETCH_READ(addr) ((void)0)
#endif

#define STD_HASHMAP_GROUP_WIDTH 8
#define STD_HASHMAP_TAG_EMPTY 0xff

static inline int std_hashmap_ctz(unsigned int x) {
  if (x == 0) {
    return -1;
  }
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

static inline unsigned int std_hashmap_match_tag(const uint8_t *ctrl, int pos, uint8_t tag) {
#if defined(STD_HASHMAP_USE_NEON)
  uint8x8_t data = vld1_u8(ctrl + pos);
  uint8x8_t target = vdup_n_u8(tag);
  uint8x8_t cmp = vceq_u8(data, target);
  uint8_t temp[STD_HASHMAP_GROUP_WIDTH];
  unsigned int mask = 0;
  vst1_u8(temp, cmp);
  for (int i = 0; i < STD_HASHMAP_GROUP_WIDTH; i++) {
    mask |= (temp[i] & 0x80) ? (1u << i) : 0u;
  }
  return mask;
#else
  unsigned int mask = 0;
  for (int i = 0; i < STD_HASHMAP_GROUP_WIDTH; i++) {
    if (ctrl[pos + i] == tag) {
      mask |= 1u << i;
    }
  }
  return mask;
#endif
}

static inline unsigned int std_hashmap_match_empty(const uint8_t *ctrl, int pos) {
#if defined(STD_HASHMAP_USE_NEON)
  uint8x8_t data = vld1_u8(ctrl + pos);
  uint8x8_t empty = vdup_n_u8(STD_HASHMAP_TAG_EMPTY);
  uint8x8_t cmp = vceq_u8(data, empty);
  uint8_t temp[STD_HASHMAP_GROUP_WIDTH];
  unsigned int mask = 0;
  vst1_u8(temp, cmp);
  for (int i = 0; i < STD_HASHMAP_GROUP_WIDTH; i++) {
    mask |= (temp[i] & 0x80) ? (1u << i) : 0u;
  }
  return mask;
#else
  unsigned int mask = 0;
  for (int i = 0; i < STD_HASHMAP_GROUP_WIDTH; i++) {
    if (ctrl[pos + i] == STD_HASHMAP_TAG_EMPTY) {
      mask |= 1u << i;
    }
  }
  return mask;
#endif
}

static inline unsigned int std_hashmap_match_empty_or_deleted(const uint8_t *ctrl, int pos) {
  unsigned int mask = 0;
  for (int i = 0; i < STD_HASHMAP_GROUP_WIDTH; i++) {
    mask |= ((ctrl[pos + i] >> 7) & 1u) << i;
  }
  return mask;
}

CAMLprim value std_hashmap_find_insert_slot(value ctrl_bytes, value hash_val, value bucket_mask_val) {
  CAMLparam3(ctrl_bytes, hash_val, bucket_mask_val);

  const uint8_t *ctrl = (const uint8_t *)Bytes_val(ctrl_bytes);
  intnat hash = Long_val(hash_val);
  intnat bucket_mask = Long_val(bucket_mask_val);
  intnat max_probes = ((bucket_mask + 1) / STD_HASHMAP_GROUP_WIDTH) + 1;
  intnat pos = hash & bucket_mask;
  intnat stride = 0;

  for (intnat probes = 0; probes < max_probes; probes++) {
    intnat next_stride = stride + STD_HASHMAP_GROUP_WIDTH;
    intnat next_pos = (pos + next_stride + STD_HASHMAP_GROUP_WIDTH) & bucket_mask;
    STD_HASHMAP_PREFETCH_READ(&ctrl[next_pos]);

    unsigned int matches = std_hashmap_match_empty_or_deleted(ctrl, (int)pos);
    if (matches != 0) {
      int offset = std_hashmap_ctz(matches);
      CAMLreturn(Val_long((pos + offset) & bucket_mask));
    }

    stride += STD_HASHMAP_GROUP_WIDTH;
    pos = (pos + stride) & bucket_mask;
  }

  CAMLreturn(Val_long(-1));
}

CAMLprim value std_hashmap_find_candidates(
  value ctrl_bytes,
  value hash_val,
  value h2_val,
  value bucket_mask_val
) {
  CAMLparam4(ctrl_bytes, hash_val, h2_val, bucket_mask_val);
  CAMLlocal2(result, cons);

  const uint8_t *ctrl = (const uint8_t *)Bytes_val(ctrl_bytes);
  intnat hash = Long_val(hash_val);
  int h2 = Int_val(h2_val);
  intnat bucket_mask = Long_val(bucket_mask_val);
  intnat max_probes = ((bucket_mask + 1) / STD_HASHMAP_GROUP_WIDTH) + 1;
  intnat pos = hash & bucket_mask;
  intnat stride = 0;

  result = Val_emptylist;

  for (intnat probes = 0; probes < max_probes; probes++) {
    intnat next_stride = stride + STD_HASHMAP_GROUP_WIDTH;
    intnat next_pos = (pos + next_stride + STD_HASHMAP_GROUP_WIDTH) & bucket_mask;
    STD_HASHMAP_PREFETCH_READ(&ctrl[next_pos]);

    unsigned int matches = std_hashmap_match_tag(ctrl, (int)pos, (uint8_t)h2);
    while (matches != 0) {
      int offset = std_hashmap_ctz(matches);
      intnat index = (pos + offset) & bucket_mask;

      cons = caml_alloc(2, 0);
      Store_field(cons, 0, Val_long(index));
      Store_field(cons, 1, result);
      result = cons;

      matches &= matches - 1;
    }

    if (std_hashmap_match_empty(ctrl, (int)pos) != 0) {
      break;
    }

    stride += STD_HASHMAP_GROUP_WIDTH;
    pos = (pos + stride) & bucket_mask;
  }

  CAMLreturn(result);
}
