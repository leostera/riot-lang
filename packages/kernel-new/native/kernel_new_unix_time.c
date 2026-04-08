#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>
#include <time.h>
#include "kernel_new_errors.h"

CAMLprim value kernel_new_time_system_time_now(value unit_val) {
  CAMLparam1(unit_val);
  CAMLlocal2(pair, result);

  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, Val_long(ts.tv_sec));
  Store_field(pair, 1, Val_int(ts.tv_nsec));
  result = kernel_new_result_ok(pair);
  CAMLreturn(result);
}
