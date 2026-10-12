#include "runtime.h"

_Static_assert(sizeof(RtValue) == 16, "unexpected RtValue size on target");
_Static_assert(_Alignof(RtValue) == 8, "unexpected RtValue alignment on target");
_Static_assert(offsetof(RtValue, as) == 8, "unexpected RtValue payload offset");

RtValue riotml_function_probe(RtValue environment, RtValue argument) {
  (void)environment;
  return argument;
}

RtValue riotml_call_probe(RtFunction function, RtValue environment,
                          RtValue argument) {
  return function(environment, argument);
}

int8_t riotml_i8_probe(void *environment, int8_t argument) {
  (void)environment;
  return argument;
}

uint16_t riotml_u16_probe(void *environment, uint16_t argument) {
  (void)environment;
  return argument;
}
