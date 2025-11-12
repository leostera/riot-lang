/* UUID generation using platform libraries (libuuid/macOS uuid) */

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <string.h>
#include <sys/time.h>
#include <uuid/uuid.h>

#ifdef __APPLE__
  #include <Security/SecRandom.h>
  #define SECURE_RANDOM(buf, len) (SecRandomCopyBytes(kSecRandomDefault, len, buf) == 0)
#elif defined(__linux__)
  #include <sys/random.h>
  #define SECURE_RANDOM(buf, len) (getrandom(buf, len, 0) == (ssize_t)len)
#else
  #define SECURE_RANDOM(buf, len) 0
#endif

/* UUID v4 - Random UUIDs using platform's crypto RNG */
CAMLprim value kernel_uuid_v4(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  
  uuid_t uuid;
  uuid_generate_random(uuid);
  
  result = caml_alloc_string(16);
  memcpy(Bytes_val(result), uuid, 16);
  
  CAMLreturn(result);
}

/* UUID v7 - Timestamp-ordered UUIDs (RFC 9562) */
CAMLprim value kernel_uuid_v7(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  
  uint8_t uuid[16];
  
  /* Get current time in milliseconds */
  struct timeval tv;
  gettimeofday(&tv, NULL);
  uint64_t time_ms = (uint64_t)tv.tv_sec * 1000ULL + tv.tv_usec / 1000;
  
  /* First 48 bits: timestamp (big-endian) */
  uuid[0] = (time_ms >> 40) & 0xFF;
  uuid[1] = (time_ms >> 32) & 0xFF;
  uuid[2] = (time_ms >> 24) & 0xFF;
  uuid[3] = (time_ms >> 16) & 0xFF;
  uuid[4] = (time_ms >> 8) & 0xFF;
  uuid[5] = time_ms & 0xFF;
  
  /* Remaining 10 bytes: secure random data */
  if (!SECURE_RANDOM(&uuid[6], 10)) {
    uuid_t temp;
    uuid_generate_random(temp);
    memcpy(&uuid[6], &temp[6], 10);
  }
  
  /* Set version 7 */
  uuid[6] = (uuid[6] & 0x0F) | 0x70;
  
  /* Set variant 0b10 */
  uuid[8] = (uuid[8] & 0x3F) | 0x80;
  
  result = caml_alloc_string(16);
  memcpy(Bytes_val(result), uuid, 16);
  
  CAMLreturn(result);
}

/* Convert UUID bytes to string */
CAMLprim value kernel_uuid_to_string(value uuid_bytes) {
  CAMLparam1(uuid_bytes);
  CAMLlocal1(result);
  
  uuid_t uuid;
  memcpy(uuid, String_val(uuid_bytes), 16);
  
  uuid_string_t str;
  uuid_unparse_lower(uuid, str);
  
  result = caml_copy_string(str);
  CAMLreturn(result);
}

/* Parse UUID string to bytes */
CAMLprim value kernel_uuid_of_string(value uuid_str) {
  CAMLparam1(uuid_str);
  CAMLlocal1(result);
  
  uuid_t uuid;
  if (uuid_parse(String_val(uuid_str), uuid) != 0) {
    caml_invalid_argument("Invalid UUID format");
  }
  
  result = caml_alloc_string(16);
  memcpy(Bytes_val(result), uuid, 16);
  
  CAMLreturn(result);
}

/* Compare two UUIDs */
CAMLprim value kernel_uuid_compare(value uuid1, value uuid2) {
  CAMLparam2(uuid1, uuid2);
  
  uuid_t u1, u2;
  memcpy(u1, String_val(uuid1), 16);
  memcpy(u2, String_val(uuid2), 16);
  
  int cmp = uuid_compare(u1, u2);
  CAMLreturn(Val_int(cmp));
}

/* Check if UUID is nil */
CAMLprim value kernel_uuid_is_nil(value uuid_bytes) {
  CAMLparam1(uuid_bytes);
  
  uuid_t uuid;
  memcpy(uuid, String_val(uuid_bytes), 16);
  
  int is_nil = uuid_is_null(uuid);
  CAMLreturn(Val_bool(is_nil));
}
