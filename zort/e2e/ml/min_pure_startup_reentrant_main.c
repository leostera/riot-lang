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
extern unsigned long long zort_metadata_registration_calls;
extern unsigned long long zort_metadata_registered;

int main(int argc, char **argv) {
  (void)argc;

  caml_globals_inited = 0;

  caml_startup((void *)argv);
  caml_startup((void *)argv);

  unsigned long long depth_after_second_startup = zort_startup_depth;
  unsigned long long globals_after_second_startup = caml_globals_inited;
  unsigned long long metadata_after_second_startup = zort_metadata_registered;

  caml_shutdown();

  unsigned long long depth_after_first_shutdown = zort_startup_depth;
  unsigned long long globals_after_first_shutdown = caml_globals_inited;
  unsigned long long metadata_after_first_shutdown = zort_metadata_registered;

  caml_shutdown();

  printf("output=unit\n");
  printf(
      "trace startup_calls=%llu shutdown_calls=%llu start_program_calls=%llu "
      "startup_depth_after_second_startup=%llu "
      "startup_depth_after_first_shutdown=%llu "
      "startup_depth_after_second_shutdown=%llu "
      "globals_after_second_startup=%llu globals_after_first_shutdown=%llu "
      "globals_after_second_shutdown=%llu metadata_registration_calls=%llu "
      "metadata_after_second_startup=%llu metadata_after_first_shutdown=%llu "
      "metadata_after_second_shutdown=%llu shutdown_happened=%llu "
      "result_raw=%llu\n",
      zort_startup_calls,
      zort_shutdown_calls,
      zort_start_program_calls,
      depth_after_second_startup,
      depth_after_first_shutdown,
      zort_startup_depth,
      globals_after_second_startup,
      globals_after_first_shutdown,
      caml_globals_inited,
      zort_metadata_registration_calls,
      metadata_after_second_startup,
      metadata_after_first_shutdown,
      zort_metadata_registered,
      zort_shutdown_happened,
      zort_last_start_program_result);

  int ok = zort_startup_calls == 2 &&
           zort_shutdown_calls == 2 &&
           zort_start_program_calls == 1 &&
           depth_after_second_startup == 2 &&
           depth_after_first_shutdown == 1 &&
           zort_startup_depth == 0 &&
           globals_after_second_startup == 1 &&
           globals_after_first_shutdown == 1 &&
           caml_globals_inited == 0 &&
           zort_metadata_registration_calls == 1 &&
           metadata_after_second_startup == 1 &&
           metadata_after_first_shutdown == 1 &&
           zort_metadata_registered == 0 &&
           zort_shutdown_happened == 1 &&
           zort_last_start_program_result == 1;
  return ok ? 0 : 1;
}
