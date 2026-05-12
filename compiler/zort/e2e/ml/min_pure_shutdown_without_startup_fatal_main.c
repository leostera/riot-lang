#include <stdio.h>

extern void caml_shutdown(void);
extern unsigned long long caml_globals_inited;
extern unsigned long long zort_startup_calls;
extern unsigned long long zort_shutdown_calls;
extern unsigned long long zort_start_program_calls;
extern unsigned long long zort_last_start_program_result;
extern unsigned long long zort_startup_depth;
extern unsigned long long zort_shutdown_happened;
extern unsigned long long zort_metadata_registered;

int main(int argc, char **argv) {
  (void)argc;
  (void)argv;

  caml_globals_inited = 0;

  printf("output=fatal_shutdown_without_startup\n");
  printf(
      "trace startup_calls_before_shutdown=%llu "
      "shutdown_calls_before_shutdown=%llu "
      "start_program_calls_before_shutdown=%llu "
      "startup_depth_before_shutdown=%llu globals_before_shutdown=%llu "
      "metadata_before_shutdown=%llu shutdown_happened_before_shutdown=%llu "
      "result_raw_before_shutdown=%llu\n",
      zort_startup_calls,
      zort_shutdown_calls,
      zort_start_program_calls,
      zort_startup_depth,
      caml_globals_inited,
      zort_metadata_registered,
      zort_shutdown_happened,
      zort_last_start_program_result);
  fflush(stdout);

  caml_shutdown();
  return 1;
}
