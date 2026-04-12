/* UUID generation using platform libraries
 * - Linux: system libuuid
 * - macOS: system UUID library
 */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <string.h>
#include <sys/time.h>

#include <uuid/uuid.h>

#ifndef uuid_string_t
typedef char uuid_string_t[37];
#endif

#ifdef __APPLE__
  #include <stdlib.h>
  #define STD_UUID_SECURE_RANDOM(buf, len) (arc4random_buf((buf), (len)), 1)
#elif defined(__linux__)
  #include <sys/random.h>
  #define STD_UUID_SECURE_RANDOM(buf, len) (getrandom(buf, len, 0) == (ssize_t) len)
#else
  #define STD_UUID_SECURE_RANDOM(buf, len) 0
#endif

CAMLprim value std_uuid_v4(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);

  uuid_t uuid;
  uuid_generate_random(uuid);

  result = caml_alloc_string(16);
  memcpy(Bytes_val(result), uuid, 16);

  CAMLreturn(result);
}

CAMLprim value std_uuid_v7(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);

  uint8_t uuid[16];
  struct timeval tv;
  uint64_t time_ms;

  gettimeofday(&tv, NULL);
  time_ms = (uint64_t) tv.tv_sec * 1000ULL + tv.tv_usec / 1000ULL;

  uuid[0] = (time_ms >> 40) & 0xFF;
  uuid[1] = (time_ms >> 32) & 0xFF;
  uuid[2] = (time_ms >> 24) & 0xFF;
  uuid[3] = (time_ms >> 16) & 0xFF;
  uuid[4] = (time_ms >> 8) & 0xFF;
  uuid[5] = time_ms & 0xFF;

  if (!STD_UUID_SECURE_RANDOM(&uuid[6], 10)) {
    uuid_t fallback;
    uuid_generate_random(fallback);
    memcpy(&uuid[6], &fallback[6], 10);
  }

  uuid[6] = (uuid[6] & 0x0F) | 0x70;
  uuid[8] = (uuid[8] & 0x3F) | 0x80;

  result = caml_alloc_string(16);
  memcpy(Bytes_val(result), uuid, 16);

  CAMLreturn(result);
}

CAMLprim value std_uuid_to_string(value uuid_bytes) {
  CAMLparam1(uuid_bytes);
  CAMLlocal1(result);

  uuid_t uuid;
  uuid_string_t out;

  memcpy(uuid, String_val(uuid_bytes), 16);
  uuid_unparse_lower(uuid, out);

  result = caml_copy_string(out);
  CAMLreturn(result);
}

CAMLprim value std_uuid_of_string(value uuid_string) {
  CAMLparam1(uuid_string);
  CAMLlocal1(result);

  uuid_t uuid;
  if (uuid_parse(String_val(uuid_string), uuid) != 0) {
    caml_invalid_argument("Invalid UUID format");
  }

  result = caml_alloc_string(16);
  memcpy(Bytes_val(result), uuid, 16);

  CAMLreturn(result);
}

CAMLprim value std_uuid_compare(value left_uuid, value right_uuid) {
  CAMLparam2(left_uuid, right_uuid);

  uuid_t left;
  uuid_t right;

  memcpy(left, String_val(left_uuid), 16);
  memcpy(right, String_val(right_uuid), 16);

  CAMLreturn(Val_int(uuid_compare(left, right)));
}

CAMLprim value std_uuid_is_nil(value uuid_bytes) {
  CAMLparam1(uuid_bytes);

  uuid_t uuid;
  memcpy(uuid, String_val(uuid_bytes), 16);

  CAMLreturn(Val_bool(uuid_is_null(uuid)));
}
