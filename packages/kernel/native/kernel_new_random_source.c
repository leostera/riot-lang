#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>
#include <stddef.h>
#include "kernel_new_errors.h"

#if defined(__APPLE__)
#include <CommonCrypto/CommonRandom.h>
#elif defined(__linux__)
#include <sys/random.h>
#include <unistd.h>
#endif

CAMLprim value kernel_new_random_source_fill(value bytes_val) {
  CAMLparam1(bytes_val);

  unsigned char *buffer = Bytes_val(bytes_val);
  size_t length = caml_string_length(bytes_val);

#if defined(__APPLE__)
  if (CCRandomGenerateBytes(buffer, length) != kCCSuccess) {
    errno = EIO;
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
#elif defined(__linux__)
  size_t offset = 0;

  while (offset < length) {
    ssize_t written = getrandom(buffer + offset, length - offset, 0);

    if (written == -1) {
      if (errno == EINTR) {
        continue;
      }

      CAMLreturn(kernel_new_result_errno());
    }

    offset += (size_t)written;
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
#else
  errno = ENOSYS;
  CAMLreturn(kernel_new_result_errno());
#endif
}
