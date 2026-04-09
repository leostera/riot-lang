#include <stdio.h>

extern void caml_startup(void *argv);
extern void caml_shutdown(void);
extern long long zort_last_emitted_int;

int main(int argc, char **argv) {
  (void)argc;

  zort_last_emitted_int = -1;
  caml_startup((void *)argv);

  printf("output=%lld\n", zort_last_emitted_int);

  caml_shutdown();
  return zort_last_emitted_int == 42 ? 0 : 1;
}
