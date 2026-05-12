#include <stdio.h>

#define CAML_NAME_SPACE
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/osdeps.h>

int main(int argc, char **argv) {
  value result;
  long output = -1;

  caml_startup((char_os **)argv);

  const value *named = caml_named_value("zort_e2e_noalloc");
  if (named == NULL) {
    fprintf(stderr, "missing named callback zort_e2e_noalloc\n");
    caml_shutdown();
    return 1;
  }

  result = caml_callback_exn(*named, Val_long(40));
  if (Is_exception_result(result)) {
    fprintf(stderr, "unexpected exception from noalloc callback\n");
    caml_shutdown();
    return 1;
  }

  output = Long_val(result);
  printf("output=%ld\n", output);

  caml_shutdown();
  return output == 42 ? 0 : 1;
}
