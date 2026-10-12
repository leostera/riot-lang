#include "runtime.h"

#include <string.h>

int main(int argc, char **argv) {
  rt_set_args(argc, argv);

  RtValue arguments = rt_unit();
  arguments.tag = RT_VARIANT;
  arguments.as.box = (RtBox *)rt_native_host_args();
  if (!rt_has_variant_name(arguments, "Cons") ||
      !rt_has_variant_count(arguments, 2))
    return 1;

  RtValue first_argument = rt_field(arguments, 0);
  RtValue tail = rt_field(arguments, 1);
  if (!rt_is_string(first_argument) ||
      strcmp(rt_as_string(first_argument), "alpha") != 0)
    return 2;
  if (!rt_has_variant_name(tail, "Cons") || !rt_has_variant_count(tail, 2))
    return 3;

  RtValue second_argument = rt_field(tail, 0);
  RtValue rest = rt_field(tail, 1);
  if (!rt_is_string(second_argument) ||
      strcmp(rt_as_string(second_argument), "beta") != 0)
    return 4;
  if (!rt_has_variant_name(rest, "Nil") || !rt_has_variant_count(rest, 0))
    return 5;

  return 0;
}
