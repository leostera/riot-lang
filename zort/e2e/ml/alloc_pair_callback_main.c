#include <stdio.h>

#define CAML_NAME_SPACE
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/osdeps.h>

int main(int argc, char **argv) {
  value result;
  long first = -1;
  long second = -1;

  caml_startup((char_os **)argv);

  const value *named = caml_named_value("zort_e2e_alloc_pair");
  if (named == NULL) {
    fprintf(stderr, "missing named callback zort_e2e_alloc_pair\n");
    caml_shutdown();
    return 1;
  }

  result = caml_callback_exn(*named, Val_long(10));
  if (Is_exception_result(result)) {
    fprintf(stderr, "unexpected exception from alloc pair callback\n");
    caml_shutdown();
    return 1;
  }

  first = Long_val(Field(result, 0));
  second = Long_val(Field(result, 1));
  printf("output=(%ld,%ld)\n", first, second);

  caml_shutdown();
  return first == 10 && second == 11 ? 0 : 1;
}
