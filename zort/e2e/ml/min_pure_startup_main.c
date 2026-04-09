#include <stdio.h>

extern void caml_startup(void *argv);
extern void caml_shutdown(void);
extern unsigned long long caml_globals_inited;
extern unsigned long long zort_startup_calls;
extern unsigned long long zort_start_program_calls;
extern unsigned long long zort_last_start_program_result;
extern unsigned long long zort_metadata_frametables;
extern unsigned long long zort_metadata_frame_descriptors;
extern unsigned long long zort_metadata_gc_root_tables;
extern unsigned long long zort_metadata_gc_root_entries;
extern unsigned long long zort_metadata_code_segments;
extern unsigned long long zort_metadata_data_segments;

int main(int argc, char **argv) {
  (void)argc;

  caml_globals_inited = 0;
  caml_startup((void *)argv);

  printf("output=unit\n");
  printf(
      "trace startup_calls=%llu start_program_calls=%llu globals_inited=%llu "
      "frametables=%llu frame_descriptors=%llu gc_root_tables=%llu "
      "gc_root_entries=%llu code_segments=%llu data_segments=%llu "
      "result_raw=%llu\n",
      zort_startup_calls,
      zort_start_program_calls,
      caml_globals_inited,
      zort_metadata_frametables,
      zort_metadata_frame_descriptors,
      zort_metadata_gc_root_tables,
      zort_metadata_gc_root_entries,
      zort_metadata_code_segments,
      zort_metadata_data_segments,
      zort_last_start_program_result);

  int ok = caml_globals_inited == 1 && zort_last_start_program_result == 1;
  caml_shutdown();
  return ok ? 0 : 1;
}
