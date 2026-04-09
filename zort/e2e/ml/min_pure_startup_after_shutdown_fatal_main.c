#include <stdio.h>

extern void caml_startup(void *argv);
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

  caml_globals_inited = 0;

  caml_startup((void *)argv);
  caml_shutdown();

  printf("output=fatal_restart_after_shutdown\n");
  printf(
      "trace startup_calls_before_restart=%llu "
      "shutdown_calls_before_restart=%llu "
      "start_program_calls_before_restart=%llu "
      "startup_depth_before_restart=%llu globals_after_shutdown=%llu "
      "metadata_after_shutdown=%llu shutdown_happened_before_restart=%llu "
      "result_raw_before_restart=%llu\n",
      zort_startup_calls,
      zort_shutdown_calls,
      zort_start_program_calls,
      zort_startup_depth,
      caml_globals_inited,
      zort_metadata_registered,
      zort_shutdown_happened,
      zort_last_start_program_result);
  fflush(stdout);

  caml_startup((void *)argv);
  return 1;
}
