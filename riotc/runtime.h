#ifndef RIOTML_RUNTIME_H
#define RIOTML_RUNTIME_H

#include <stdint.h>
#include <stddef.h>

typedef struct RtValue RtValue;
typedef struct RtBox RtBox;
typedef struct RtClosure RtClosure;
typedef RtValue (*RtFunction)(RtValue environment, RtValue argument);

struct RtValue {
  int tag;
  union {
    int64_t integer;
    char *string;
    RtBox *box;
    RtClosure *closure;
  } as;
};

struct RtBox {
  int tag;
  size_t count;
  RtValue *fields;
};

struct RtClosure {
  RtFunction function;
  RtValue environment;
};

enum {
  RT_INT = 1,
  RT_BOOL,
  RT_STRING,
  RT_UNIT,
  RT_TUPLE,
  RT_VARIANT,
  RT_FUNCTION
};

enum {
  RT_NIL = 1,
  RT_CONS,
  RT_NONE,
  RT_SOME,
  RT_OK,
  RT_ERR
};

RtValue rt_int(int64_t value);
RtValue rt_bool(int value);
RtValue rt_string(const char *value);
RtValue rt_unit(void);
RtValue rt_tuple(size_t count, const RtValue *fields);
RtValue rt_variant(int tag, size_t count, const RtValue *fields);
RtValue rt_field(RtValue value, size_t index);
int rt_has_tag(RtValue value, int tag);
RtValue rt_function(RtFunction function, RtValue environment);
RtValue rt_apply(RtValue function, RtValue argument);
int64_t rt_as_int(RtValue value);
int rt_as_bool(RtValue value);
char *rt_as_string(RtValue value);

RtValue rt_add(RtValue left, RtValue right);
RtValue rt_i32_add(RtValue left, RtValue right);
RtValue rt_i32_sub(RtValue left, RtValue right);
RtValue rt_i32_mul(RtValue left, RtValue right);
RtValue rt_i32_div(RtValue left, RtValue right);
RtValue rt_i32_rem(RtValue left, RtValue right);
RtValue rt_i32_equal(RtValue left, RtValue right);
RtValue rt_i32_not_equal(RtValue left, RtValue right);
RtValue rt_i32_less(RtValue left, RtValue right);
RtValue rt_i32_less_equal(RtValue left, RtValue right);
RtValue rt_i32_greater(RtValue left, RtValue right);
RtValue rt_i32_greater_equal(RtValue left, RtValue right);
RtValue rt_i32_neg(RtValue value);
RtValue rt_sub(RtValue left, RtValue right);
RtValue rt_mul(RtValue left, RtValue right);
RtValue rt_div(RtValue left, RtValue right);
RtValue rt_rem(RtValue left, RtValue right);
RtValue rt_equal(RtValue left, RtValue right);
RtValue rt_less(RtValue left, RtValue right);
RtValue rt_greater(RtValue left, RtValue right);
RtValue rt_concat(RtValue left, RtValue right);

RtValue rt_args(void);
RtValue rt_read_file(RtValue ignored, RtValue path);
RtValue rt_write_file(RtValue environment, RtValue path);
RtValue rt_write_stdout(RtValue ignored, RtValue message);
RtValue rt_write_stderr(RtValue ignored, RtValue message);
RtValue rt_run_process(RtValue environment, RtValue program);

RtValue rt_string_concat(RtValue environment, RtValue left);
RtValue rt_string_length(RtValue ignored, RtValue value);
RtValue rt_string_get(RtValue environment, RtValue value);
RtValue rt_string_slice(RtValue environment, RtValue value);
RtValue rt_string_equal(RtValue environment, RtValue left);

RtValue rt_constructor_nil(RtValue ignored, RtValue unit);
RtValue rt_constructor_cons(RtValue ignored, RtValue unit);
RtValue rt_constructor_none(RtValue ignored, RtValue unit);
RtValue rt_constructor_some(RtValue ignored, RtValue value);
RtValue rt_constructor_ok(RtValue ignored, RtValue value);
RtValue rt_constructor_err(RtValue ignored, RtValue value);

void rt_set_args(int argc, char **argv);
int rt_utf8_valid(const char *bytes, size_t length);

#endif
