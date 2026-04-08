#include <caml/fail.h>
#include <caml/mlvalues.h>

CAMLprim value kernel_new_panic(value message_val) {
  caml_failwith(String_val(message_val));
}
