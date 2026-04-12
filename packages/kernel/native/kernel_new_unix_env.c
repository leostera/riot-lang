#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "kernel_new_errors.h"

extern char **environ;

CAMLprim value kernel_new_env_get(value name_val) {
  CAMLparam1(name_val);
  CAMLlocal2(copy, result);

  const char *raw = getenv(String_val(name_val));
  if (raw == NULL) {
    CAMLreturn(Val_int(0));
  }

  copy = caml_copy_string(raw);
  result = caml_alloc(1, 0);
  Store_field(result, 0, copy);
  CAMLreturn(result);
}

CAMLprim value kernel_new_env_set_var(value name_val, value value_val) {
  CAMLparam2(name_val, value_val);

  if (setenv(String_val(name_val), String_val(value_val), 1) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_env_remove_var(value name_val) {
  CAMLparam1(name_val);

  if (unsetenv(String_val(name_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_env_vars(value unit_val) {
  CAMLparam1(unit_val);
  CAMLlocal4(result, pair, name_val, value_val);

  int count = 0;
  while (environ[count] != NULL) {
    count++;
  }

  result = caml_alloc(count, 0);
  for (int index = 0; index < count; index++) {
    const char *entry = environ[index];
    const char *separator = strchr(entry, '=');
    if (separator == NULL) {
      name_val = caml_copy_string(entry);
      value_val = caml_copy_string("");
    } else {
      size_t name_length = (size_t)(separator - entry);
      char *name = malloc(name_length + 1);
      if (name == NULL) {
        caml_raise_out_of_memory();
      }
      memcpy(name, entry, name_length);
      name[name_length] = '\0';
      name_val = caml_copy_string(name);
      value_val = caml_copy_string(separator + 1);
      free(name);
    }

    pair = caml_alloc_tuple(2);
    Store_field(pair, 0, name_val);
    Store_field(pair, 1, value_val);
    Store_field(result, index, pair);
  }

  CAMLreturn(result);
}

CAMLprim value kernel_new_env_current_dir(value unit_val) {
  CAMLparam1(unit_val);
  CAMLlocal1(copy);

  long size = 256;
  char *buffer = NULL;

  for (;;) {
    buffer = malloc((size_t)size);
    if (buffer == NULL) {
      caml_raise_out_of_memory();
    }

    if (getcwd(buffer, (size_t)size) != NULL) {
      copy = caml_copy_string(buffer);
      free(buffer);
      CAMLreturn(kernel_new_result_ok(copy));
    }

    if (errno != ERANGE) {
      int saved_errno = errno;
      free(buffer);
      errno = saved_errno;
      CAMLreturn(kernel_new_result_errno());
    }

    free(buffer);
    size = size * 2;
  }
}

CAMLprim value kernel_new_env_set_current_dir(value path_val) {
  CAMLparam1(path_val);

  if (chdir(String_val(path_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}
