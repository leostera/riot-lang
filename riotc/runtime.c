#define _POSIX_C_SOURCE 200809L

#include "runtime.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static int rt_argc;
static char **rt_argv;
static char *rt_runtime_directory;

_Noreturn static void rt_fail(const char *message) {
  fprintf(stderr, "riotml runtime: %s\n", message);
  abort();
}

static RtValue rt_value(int tag) {
  RtValue value;
  value.tag = tag;
  value.as.integer = 0;
  return value;
}

RtValue rt_int(int64_t integer) {
  RtValue value = rt_value(RT_INT);
  value.as.integer = integer;
  return value;
}

RtValue rt_bool(int boolean) {
  RtValue value = rt_value(RT_BOOL);
  value.as.integer = boolean != 0;
  return value;
}

RtValue rt_string(const char *string) {
  RtValue value = rt_value(RT_STRING);
  size_t length = strlen(string);
  value.as.string = malloc(length + 1);
  if (value.as.string == NULL) rt_fail("out of memory");
  memcpy(value.as.string, string, length + 1);
  return value;
}

RtValue rt_unit(void) { return rt_value(RT_UNIT); }

static char *rt_copy_string(const char *source) {
  size_t length = strlen(source);
  char *copy = malloc(length + 1);
  if (copy == NULL) rt_fail("out of memory");
  memcpy(copy, source, length + 1);
  return copy;
}

static RtValue rt_boxed(int value_tag, int constructor_tag, size_t count,
                        const char *constructor_name,
                        const char *const *field_names,
                        const RtValue *fields) {
  RtValue value = rt_value(value_tag);
  RtBox *box = malloc(sizeof(*box));
  if (box == NULL) rt_fail("out of memory");
  box->tag = constructor_tag;
  box->count = count;
  box->fields = count == 0 ? NULL : malloc(count * sizeof(*box->fields));
  box->constructor_name = constructor_name == NULL
                              ? NULL
                              : rt_copy_string(constructor_name);
  box->field_names = count == 0 || field_names == NULL
                         ? NULL
                         : malloc(count * sizeof(*box->field_names));
  if (count != 0 && box->fields == NULL) rt_fail("out of memory");
  if (count != 0 && field_names != NULL && box->field_names == NULL)
    rt_fail("out of memory");
  if (count != 0) memcpy(box->fields, fields, count * sizeof(*fields));
  if (box->field_names != NULL) {
    for (size_t index = 0; index < count; index += 1)
      box->field_names[index] = rt_copy_string(field_names[index]);
  }
  value.as.box = box;
  return value;
}

RtValue rt_tuple(size_t count, const RtValue *fields) {
  return rt_boxed(RT_TUPLE, 0, count, NULL, NULL, fields);
}

RtValue rt_variant(int tag, size_t count, const RtValue *fields) {
  return rt_boxed(RT_VARIANT, tag, count, NULL, NULL, fields);
}

RtValue rt_variant_named(const char *name, size_t count,
                         const RtValue *fields) {
  return rt_boxed(RT_VARIANT, 0, count, name, NULL, fields);
}

RtValue rt_record_named(const char *constructor_name, size_t count,
                        const char *const *field_names,
                        const RtValue *fields) {
  return rt_boxed(RT_VARIANT, 0, count, constructor_name, field_names, fields);
}

RtValue rt_field(RtValue value, size_t index) {
  if ((value.tag != RT_TUPLE && value.tag != RT_VARIANT) ||
      index >= value.as.box->count) {
    rt_fail("invalid tuple or constructor field access");
  }
  return value.as.box->fields[index];
}

int rt_has_tag(RtValue value, int tag) {
  const char *name = NULL;
  switch (tag) {
  case RT_NIL: name = "Nil"; break;
  case RT_CONS: name = "Cons"; break;
  case RT_NONE: name = "None"; break;
  case RT_SOME: name = "Some"; break;
  case RT_OK: name = "Ok"; break;
  case RT_ERR: name = "Err"; break;
  default: return 0;
  }
  return value.tag == RT_VARIANT && value.as.box->constructor_name != NULL &&
         strcmp(value.as.box->constructor_name, name) == 0;
}

int rt_has_variant_name(RtValue value, const char *name) {
  return value.tag == RT_VARIANT && value.as.box->constructor_name != NULL &&
         strcmp(value.as.box->constructor_name, name) == 0;
}

int rt_has_variant_count(RtValue value, size_t count) {
  return value.tag == RT_VARIANT && value.as.box->count == count;
}

int rt_has_tuple_count(RtValue value, size_t count) {
  return value.tag == RT_TUPLE && value.as.box->count == count;
}

int rt_has_named_field(RtValue value, const char *field_name) {
  if (value.tag != RT_VARIANT || value.as.box->field_names == NULL) return 0;
  for (size_t index = 0; index < value.as.box->count; index += 1) {
    if (strcmp(value.as.box->field_names[index], field_name) == 0) return 1;
  }
  return 0;
}

RtValue rt_field_named(RtValue value, const char *field_name) {
  if (value.tag != RT_VARIANT || value.as.box->field_names == NULL)
    rt_fail("record field access on a non-record value");
  for (size_t index = 0; index < value.as.box->count; index += 1) {
    if (strcmp(value.as.box->field_names[index], field_name) == 0)
      return value.as.box->fields[index];
  }
  rt_fail("record field does not exist");
}

int rt_is_int(RtValue value) { return value.tag == RT_INT; }
int rt_is_bool(RtValue value) { return value.tag == RT_BOOL; }
int rt_is_string(RtValue value) { return value.tag == RT_STRING; }
int rt_is_unit(RtValue value) { return value.tag == RT_UNIT; }

RtValue rt_environment_root(void) {
  RtValue value = rt_value(RT_ENVIRONMENT);
  RtEnvironment *environment = calloc(1, sizeof(*environment));
  if (environment == NULL) rt_fail("out of memory");
  value.as.environment = environment;
  return value;
}

RtValue rt_environment_child(RtValue parent) {
  if (parent.tag != RT_ENVIRONMENT)
    rt_fail("closure environment is not an environment");
  RtValue value = rt_value(RT_ENVIRONMENT);
  RtEnvironment *environment = calloc(1, sizeof(*environment));
  if (environment == NULL) rt_fail("out of memory");
  environment->parent = parent.as.environment;
  value.as.environment = environment;
  return value;
}

void rt_environment_bind(RtValue value, const char *name, RtValue bound_value) {
  if (value.tag != RT_ENVIRONMENT)
    rt_fail("cannot bind a name outside an environment");
  RtEnvironment *environment = value.as.environment;
  for (size_t index = 0; index < environment->count; index += 1) {
    if (strcmp(environment->bindings[index].name, name) == 0) {
      environment->bindings[index].value = bound_value;
      return;
    }
  }
  RtBinding *bindings = realloc(environment->bindings,
                                (environment->count + 1) * sizeof(*bindings));
  if (bindings == NULL) rt_fail("out of memory");
  environment->bindings = bindings;
  environment->bindings[environment->count].name = rt_copy_string(name);
  environment->bindings[environment->count].value = bound_value;
  environment->count += 1;
}

RtValue rt_environment_get(RtValue value, const char *name) {
  if (value.tag != RT_ENVIRONMENT)
    rt_fail("cannot resolve a name outside an environment");
  for (RtEnvironment *environment = value.as.environment; environment != NULL;
       environment = environment->parent) {
    for (size_t index = environment->count; index > 0; index -= 1) {
      RtBinding binding = environment->bindings[index - 1];
      if (strcmp(binding.name, name) == 0) return binding.value;
    }
  }
  fprintf(stderr, "riotml runtime: unbound RiotML value name `%s`\n", name);
  abort();
}

RtValue rt_string_equal_to(RtValue value, const char *expected) {
  return rt_bool(strcmp(rt_as_string(value), expected) == 0);
}

void rt_pattern_fail(void) { rt_fail("RiotML pattern match failed"); }

RtValue rt_function(RtFunction function, RtValue environment) {
  RtValue value = rt_value(RT_FUNCTION);
  RtClosure *closure = malloc(sizeof(*closure));
  if (closure == NULL) rt_fail("out of memory");
  closure->function = function;
  closure->environment = environment;
  value.as.closure = closure;
  return value;
}

RtValue rt_apply(RtValue function, RtValue argument) {
  if (function.tag != RT_FUNCTION) rt_fail("attempted to call a non-function");
  return function.as.closure->function(function.as.closure->environment,
                                       argument);
}

int64_t rt_as_int(RtValue value) {
  if (value.tag != RT_INT) rt_fail("expected an integer");
  return value.as.integer;
}

int rt_as_bool(RtValue value) {
  if (value.tag != RT_BOOL) rt_fail("expected a boolean");
  return value.as.integer != 0;
}

char *rt_as_string(RtValue value) {
  if (value.tag != RT_STRING) rt_fail("expected a string");
  return value.as.string;
}

RtValue rt_add(RtValue left, RtValue right) {
  if (left.tag == RT_STRING && right.tag == RT_STRING)
    return rt_concat(left, right);
  uint32_t result = (uint32_t)rt_as_int(left) + (uint32_t)rt_as_int(right);
  return rt_int((int32_t)result);
}

RtValue rt_i32_add(RtValue left, RtValue right) {
  uint32_t result = (uint32_t)rt_as_int(left) + (uint32_t)rt_as_int(right);
  return rt_int((int32_t)result);
}

RtValue rt_i32_sub(RtValue left, RtValue right) {
  uint32_t result = (uint32_t)rt_as_int(left) - (uint32_t)rt_as_int(right);
  return rt_int((int32_t)result);
}

RtValue rt_i32_mul(RtValue left, RtValue right) {
  uint32_t result = (uint32_t)rt_as_int(left) * (uint32_t)rt_as_int(right);
  return rt_int((int32_t)result);
}

RtValue rt_i32_div(RtValue left, RtValue right) {
  int32_t numerator = (int32_t)rt_as_int(left);
  int32_t denominator = (int32_t)rt_as_int(right);
  if (denominator == 0) rt_fail("i32 division by zero");
  if (numerator == INT32_MIN && denominator == -1)
    return rt_int(INT32_MIN);
  return rt_int(numerator / denominator);
}

RtValue rt_i32_rem(RtValue left, RtValue right) {
  int32_t numerator = (int32_t)rt_as_int(left);
  int32_t denominator = (int32_t)rt_as_int(right);
  if (denominator == 0) rt_fail("i32 remainder by zero");
  if (numerator == INT32_MIN && denominator == -1) return rt_int(0);
  return rt_int(numerator % denominator);
}

RtValue rt_i32_equal(RtValue left, RtValue right) {
  return rt_bool((int32_t)rt_as_int(left) == (int32_t)rt_as_int(right));
}

RtValue rt_i32_not_equal(RtValue left, RtValue right) {
  return rt_bool((int32_t)rt_as_int(left) != (int32_t)rt_as_int(right));
}

RtValue rt_i32_less(RtValue left, RtValue right) {
  return rt_bool((int32_t)rt_as_int(left) < (int32_t)rt_as_int(right));
}

RtValue rt_i32_less_equal(RtValue left, RtValue right) {
  return rt_bool((int32_t)rt_as_int(left) <= (int32_t)rt_as_int(right));
}

RtValue rt_i32_greater(RtValue left, RtValue right) {
  return rt_bool((int32_t)rt_as_int(left) > (int32_t)rt_as_int(right));
}

RtValue rt_i32_greater_equal(RtValue left, RtValue right) {
  return rt_bool((int32_t)rt_as_int(left) >= (int32_t)rt_as_int(right));
}

RtValue rt_i32_neg(RtValue value) {
  uint32_t result = 0u - (uint32_t)rt_as_int(value);
  return rt_int((int32_t)result);
}

RtValue rt_sub(RtValue left, RtValue right) {
  uint32_t result = (uint32_t)rt_as_int(left) - (uint32_t)rt_as_int(right);
  return rt_int((int32_t)result);
}

RtValue rt_mul(RtValue left, RtValue right) {
  uint32_t result = (uint32_t)rt_as_int(left) * (uint32_t)rt_as_int(right);
  return rt_int((int32_t)result);
}

RtValue rt_div(RtValue left, RtValue right) {
  int32_t numerator = (int32_t)rt_as_int(left);
  int32_t denominator = (int32_t)rt_as_int(right);
  if (denominator == 0) rt_fail("division by zero");
  if (numerator == INT32_MIN && denominator == -1)
    return rt_int(INT32_MIN);
  return rt_int(numerator / denominator);
}

RtValue rt_rem(RtValue left, RtValue right) {
  int32_t numerator = (int32_t)rt_as_int(left);
  int32_t denominator = (int32_t)rt_as_int(right);
  if (denominator == 0) rt_fail("remainder by zero");
  if (numerator == INT32_MIN && denominator == -1) return rt_int(0);
  return rt_int(numerator % denominator);
}

RtValue rt_equal(RtValue left, RtValue right) {
  if (left.tag != right.tag) return rt_bool(0);
  switch (left.tag) {
  case RT_INT:
  case RT_BOOL:
    return rt_bool(left.as.integer == right.as.integer);
  case RT_STRING:
    return rt_bool(strcmp(left.as.string, right.as.string) == 0);
  case RT_UNIT:
    return rt_bool(1);
  default:
    return rt_bool(left.as.box == right.as.box);
  }
}

RtValue rt_less(RtValue left, RtValue right) {
  return rt_bool(rt_as_int(left) < rt_as_int(right));
}

RtValue rt_greater(RtValue left, RtValue right) {
  return rt_bool(rt_as_int(left) > rt_as_int(right));
}

RtValue rt_concat(RtValue left, RtValue right) {
  size_t left_length = strlen(rt_as_string(left));
  size_t right_length = strlen(rt_as_string(right));
  char *joined = malloc(left_length + right_length + 1);
  if (joined == NULL) rt_fail("out of memory");
  memcpy(joined, left.as.string, left_length);
  memcpy(joined + left_length, right.as.string, right_length + 1);
  RtValue result = rt_value(RT_STRING);
  result.as.string = joined;
  return result;
}

static RtValue rt_nil(void) { return rt_variant_named("Nil", 0, NULL); }
static RtValue rt_cons_tail(RtValue head, RtValue tail) {
  RtValue fields[] = {head, tail};
  return rt_variant_named("Cons", 2, fields);
}
static RtValue rt_cons_first(RtValue ignored, RtValue head) {
  (void)ignored;
  return rt_function(rt_cons_tail, head);
}
static RtValue rt_cons(RtValue ignored, RtValue unit) {
  (void)ignored;
  (void)unit;
  return rt_function(rt_cons_first, rt_unit());
}
static RtValue rt_none(void) { return rt_variant_named("None", 0, NULL); }
static RtValue rt_some(RtValue ignored, RtValue value) {
  (void)ignored;
  return rt_variant_named("Some", 1, &value);
}
static RtValue rt_ok(RtValue ignored, RtValue value) {
  (void)ignored;
  return rt_variant_named("Ok", 1, &value);
}
static RtValue rt_err(RtValue ignored, RtValue value) {
  (void)ignored;
  return rt_variant_named("Err", 1, &value);
}

void rt_set_args(int argc, char **argv) {
  rt_argc = argc;
  rt_argv = argv;
}

void rt_set_runtime_directory(const char *directory) {
  rt_runtime_directory = rt_copy_string(directory);
}

RtValue rt_args(void) {
  RtValue values = rt_nil();
  for (int index = rt_argc - 1; index >= 1; --index) {
    RtValue text = rt_string(rt_argv[index]);
    values = rt_cons_tail(text, values);
  }
  return values;
}

RtValue rt_host_args(RtValue ignored, RtValue unit) {
  (void)ignored;
  (void)unit;
  return rt_args();
}

RtValue rt_host_current_directory(RtValue ignored, RtValue unit) {
  (void)ignored;
  (void)unit;
  char directory[4096];
  if (getcwd(directory, sizeof(directory)) == NULL)
    rt_fail("cannot discover the current working directory");
  return rt_string(directory);
}

RtValue rt_host_runtime_directory(RtValue ignored, RtValue unit) {
  (void)ignored;
  (void)unit;
  return rt_string(rt_runtime_directory == NULL ? "" : rt_runtime_directory);
}

int rt_utf8_valid(const char *bytes, size_t length) {
  size_t index = 0;
  while (index < length) {
    unsigned char first = (unsigned char)bytes[index];
    size_t width;
    uint32_t codepoint;
    if (first < 0x80) {
      width = 1;
      codepoint = first;
    } else if ((first & 0xe0) == 0xc0) {
      width = 2;
      codepoint = first & 0x1f;
      if (codepoint < 2) return 0;
    } else if ((first & 0xf0) == 0xe0) {
      width = 3;
      codepoint = first & 0x0f;
    } else if ((first & 0xf8) == 0xf0) {
      width = 4;
      codepoint = first & 0x07;
      if (codepoint > 4) return 0;
    } else {
      return 0;
    }
    if (index + width > length) return 0;
    for (size_t offset = 1; offset < width; ++offset) {
      unsigned char next = (unsigned char)bytes[index + offset];
      if ((next & 0xc0) != 0x80) return 0;
      codepoint = (codepoint << 6) | (next & 0x3f);
    }
    if ((width == 3 && codepoint < 0x800) ||
        (width == 4 && codepoint < 0x10000) ||
        (codepoint >= 0xd800 && codepoint <= 0xdfff) || codepoint > 0x10ffff)
      return 0;
    index += width;
  }
  return 1;
}

RtValue rt_read_file(RtValue ignored, RtValue path) {
  (void)ignored;
  FILE *file = fopen(rt_as_string(path), "rb");
  if (file == NULL) return rt_err(rt_unit(), rt_string(strerror(errno)));
  if (fseek(file, 0, SEEK_END) != 0) {
    int error = errno;
    fclose(file);
    return rt_err(rt_unit(), rt_string(strerror(error)));
  }
  long size = ftell(file);
  if (size < 0 || fseek(file, 0, SEEK_SET) != 0) {
    int error = errno;
    fclose(file);
    return rt_err(rt_unit(), rt_string(strerror(error)));
  }
  char *contents = malloc((size_t)size + 1);
  if (contents == NULL) rt_fail("out of memory");
  size_t read_count = fread(contents, 1, (size_t)size, file);
  int read_error = ferror(file);
  fclose(file);
  if (read_error || read_count != (size_t)size) {
    free(contents);
    return rt_err(rt_unit(), rt_string("failed to read file"));
  }
  if (!rt_utf8_valid(contents, read_count)) {
    free(contents);
    return rt_err(rt_unit(), rt_string("file is not valid UTF-8"));
  }
  contents[read_count] = '\0';
  RtValue text = rt_value(RT_STRING);
  text.as.string = contents;
  return rt_ok(rt_unit(), text);
}

static RtValue rt_write_file_contents(RtValue path, RtValue contents) {
  FILE *file = fopen(rt_as_string(path), "wb");
  if (file == NULL) return rt_err(rt_unit(), rt_string(strerror(errno)));
  char *text = rt_as_string(contents);
  size_t length = strlen(text);
  size_t written = fwrite(text, 1, length, file);
  int write_failed = ferror(file) || written != length;
  int close_failed = fclose(file) != 0;
  if (write_failed || close_failed)
    return rt_err(rt_unit(), rt_string("failed to write file"));
  return rt_ok(rt_unit(), rt_unit());
}

static RtValue rt_write_file_contents_fn(RtValue path, RtValue contents) {
  return rt_write_file_contents(path, contents);
}

RtValue rt_write_file(RtValue ignored, RtValue path) {
  (void)ignored;
  return rt_function(rt_write_file_contents_fn, path);
}

RtValue rt_write_stdout(RtValue ignored, RtValue message) {
  (void)ignored;
  char *text = rt_as_string(message);
  fwrite(text, 1, strlen(text), stdout);
  return rt_unit();
}

RtValue rt_write_stderr(RtValue ignored, RtValue message) {
  (void)ignored;
  char *text = rt_as_string(message);
  fwrite(text, 1, strlen(text), stderr);
  return rt_unit();
}

static RtValue rt_process_args(RtValue program, RtValue list) {
  size_t count = 0;
  RtValue cursor = list;
  while (rt_has_tag(cursor, RT_CONS)) {
    ++count;
    cursor = rt_field(cursor, 1);
  }
  if (!rt_has_tag(cursor, RT_NIL))
    return rt_err(rt_unit(), rt_string("Host.run_process expects a List<string>"));
  char **argv = calloc(count + 2, sizeof(*argv));
  if (argv == NULL) rt_fail("out of memory");
  argv[0] = rt_as_string(program);
  cursor = list;
  for (size_t index = 0; index < count; ++index) {
    argv[index + 1] = rt_as_string(rt_field(cursor, 0));
    cursor = rt_field(cursor, 1);
  }
  pid_t child = fork();
  if (child == -1) {
    free(argv);
    return rt_err(rt_unit(), rt_string(strerror(errno)));
  }
  if (child == 0) {
    execvp(argv[0], argv);
    fprintf(stderr, "riotml: cannot execute %s: %s\n", argv[0], strerror(errno));
    _exit(127);
  }
  int status;
  while (waitpid(child, &status, 0) == -1) {
    if (errno == EINTR) continue;
    int error = errno;
    free(argv);
    return rt_err(rt_unit(), rt_string(strerror(error)));
  }
  free(argv);
  if (WIFEXITED(status)) return rt_ok(rt_unit(), rt_int(WEXITSTATUS(status)));
  return rt_err(rt_unit(), rt_string("child process terminated by signal"));
}

static RtValue rt_run_process_args_fn(RtValue program, RtValue arguments) {
  return rt_process_args(program, arguments);
}

RtValue rt_run_process(RtValue ignored, RtValue program) {
  (void)ignored;
  return rt_function(rt_run_process_args_fn, program);
}

static RtValue rt_string_concat_right(RtValue left, RtValue right) {
  return rt_concat(left, right);
}

RtValue rt_string_concat(RtValue ignored, RtValue left) {
  (void)ignored;
  return rt_function(rt_string_concat_right, left);
}

RtValue rt_string_length(RtValue ignored, RtValue value) {
  (void)ignored;
  const unsigned char *text = (const unsigned char *)rt_as_string(value);
  size_t length = 0;
  for (size_t index = 0; text[index] != 0; ++length) {
    unsigned char first = text[index];
    index += first < 0x80 ? 1 : (first & 0xe0) == 0xc0 ? 2 :
             (first & 0xf0) == 0xe0 ? 3 : 4;
  }
  return rt_int((int64_t)length);
}

static uint32_t rt_decode_codepoint(const unsigned char *text, size_t *width) {
  unsigned char first = text[0];
  if (first < 0x80) {
    *width = 1;
    return first;
  }
  if ((first & 0xe0) == 0xc0) {
    *width = 2;
    return ((uint32_t)(first & 0x1f) << 6) | (text[1] & 0x3f);
  }
  if ((first & 0xf0) == 0xe0) {
    *width = 3;
    return ((uint32_t)(first & 0x0f) << 12) |
           ((uint32_t)(text[1] & 0x3f) << 6) | (text[2] & 0x3f);
  }
  *width = 4;
  return ((uint32_t)(first & 0x07) << 18) |
         ((uint32_t)(text[1] & 0x3f) << 12) |
         ((uint32_t)(text[2] & 0x3f) << 6) | (text[3] & 0x3f);
}

static RtValue rt_string_get_index(RtValue text, RtValue index) {
  int64_t wanted = rt_as_int(index);
  if (wanted < 0) return rt_none();
  const unsigned char *bytes = (const unsigned char *)rt_as_string(text);
  size_t offset = 0;
  int64_t position = 0;
  while (bytes[offset] != 0) {
    size_t width;
    uint32_t codepoint = rt_decode_codepoint(bytes + offset, &width);
    if (position == wanted) return rt_some(rt_unit(), rt_int((int64_t)codepoint));
    offset += width;
    ++position;
  }
  return rt_none();
}

static RtValue rt_string_get_second(RtValue text, RtValue index) {
  return rt_string_get_index(text, index);
}

RtValue rt_string_get(RtValue ignored, RtValue text) {
  (void)ignored;
  return rt_function(rt_string_get_second, text);
}

static size_t rt_byte_offset(const char *text, int64_t character_index) {
  if (character_index <= 0) return 0;
  const unsigned char *bytes = (const unsigned char *)text;
  size_t offset = 0;
  int64_t position = 0;
  while (bytes[offset] != 0 && position < character_index) {
    unsigned char first = bytes[offset];
    offset += first < 0x80 ? 1 : (first & 0xe0) == 0xc0 ? 2 :
             (first & 0xf0) == 0xe0 ? 3 : 4;
    ++position;
  }
  return offset;
}

static RtValue rt_string_slice_end(RtValue state, RtValue end_value) {
  RtValue text = rt_field(state, 0);
  int64_t start = rt_as_int(rt_field(state, 1));
  int64_t end = rt_as_int(end_value);
  int64_t length = rt_as_int(rt_string_length(rt_unit(), text));
  if (start < 0) start = 0;
  if (start > length) start = length;
  if (end < start) end = start;
  if (end > length) end = length;
  size_t first = rt_byte_offset(rt_as_string(text), start);
  size_t last = rt_byte_offset(rt_as_string(text), end);
  char *slice = malloc(last - first + 1);
  if (slice == NULL) rt_fail("out of memory");
  memcpy(slice, rt_as_string(text) + first, last - first);
  slice[last - first] = '\0';
  RtValue result = rt_value(RT_STRING);
  result.as.string = slice;
  return result;
}

static RtValue rt_string_slice_start(RtValue text, RtValue start) {
  RtValue fields[] = {text, start};
  return rt_function(rt_string_slice_end, rt_tuple(2, fields));
}

static RtValue rt_string_slice_second(RtValue text, RtValue start) {
  return rt_string_slice_start(text, start);
}

RtValue rt_string_slice(RtValue ignored, RtValue text) {
  (void)ignored;
  return rt_function(rt_string_slice_second, text);
}

static RtValue rt_string_equal_right(RtValue left, RtValue right) {
  return rt_bool(strcmp(rt_as_string(left), rt_as_string(right)) == 0);
}

RtValue rt_string_equal(RtValue ignored, RtValue left) {
  (void)ignored;
  return rt_function(rt_string_equal_right, left);
}

/* Constructor values are exposed to generated C through these functions. */
RtValue rt_constructor_nil(RtValue ignored, RtValue unit) {
  (void)ignored;
  (void)unit;
  return rt_nil();
}
RtValue rt_constructor_none(RtValue ignored, RtValue unit) {
  (void)ignored;
  (void)unit;
  return rt_none();
}
RtValue rt_constructor_cons(RtValue ignored, RtValue unit) {
  return rt_cons(ignored, unit);
}
RtValue rt_constructor_some(RtValue ignored, RtValue value) {
  return rt_some(ignored, value);
}
RtValue rt_constructor_ok(RtValue ignored, RtValue value) {
  return rt_ok(ignored, value);
}
RtValue rt_constructor_err(RtValue ignored, RtValue value) {
  return rt_err(ignored, value);
}
