#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/printexc.h>

CAMLprim value kernel_new_exception_to_string(value exn_val) {
  CAMLparam1(exn_val);
  char *formatted = caml_format_exception(exn_val);
  value out;

  if (formatted == NULL) {
    caml_raise_out_of_memory();
  }

  out = caml_copy_string(formatted);
  caml_stat_free(formatted);
  CAMLreturn(out);
}
