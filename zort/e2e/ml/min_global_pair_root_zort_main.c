#include <stdio.h>

extern void caml_startup(void *argv);
extern void caml_shutdown(void);
extern unsigned long long caml_globals_inited;
extern unsigned long long zort_startup_calls;
extern unsigned long long zort_start_program_calls;
extern unsigned long long zort_last_start_program_result;
extern unsigned long long zort_metadata_registered;
extern unsigned long long zort_metadata_frametables;
extern unsigned long long zort_metadata_frame_descriptors;
extern unsigned long long zort_metadata_gc_root_tables;
extern unsigned long long zort_metadata_gc_root_entries;
extern unsigned long long zort_metadata_gc_root_blocks;
extern unsigned long long zort_metadata_gc_root_block_fields;
extern unsigned long long zort_metadata_code_segments;
extern unsigned long long zort_metadata_data_segments;
extern unsigned long long zort_metadata_program_fragment_registered;
extern unsigned long long zort_gc_root_block_field_slot_count(void);
extern unsigned long long *zort_gc_root_block_field_slot_at(unsigned long long index);

int main(int argc, char **argv) {
  (void)argc;

  caml_globals_inited = 0;
  caml_startup((void *)argv);

  unsigned long long root_slot_count = zort_gc_root_block_field_slot_count();
  unsigned long long *first_root_slot = zort_gc_root_block_field_slot_at(0);
  unsigned long long *second_root_slot = zort_gc_root_block_field_slot_at(1);
  unsigned long long first_root_slot_present = first_root_slot != NULL;
  unsigned long long first_root_slot_points_to_block =
      first_root_slot != NULL && *first_root_slot != 0 &&
      ((*first_root_slot & 1ULL) == 0);
  unsigned long long second_root_slot_present = second_root_slot != NULL;

  printf("output=unit\n");
  printf(
      "trace startup_calls=%llu start_program_calls=%llu globals_inited=%llu "
      "metadata_registered=%llu frametables=%llu frame_descriptors=%llu "
      "gc_root_tables=%llu gc_root_entries=%llu gc_root_blocks=%llu "
      "gc_root_block_fields=%llu gc_root_block_field_slots=%llu "
      "first_gc_root_block_field_slot_present=%llu "
      "first_gc_root_block_field_points_to_block=%llu "
      "second_gc_root_block_field_slot_present=%llu "
      "code_segments=%llu data_segments=%llu "
      "program_fragment_registered=%llu result_raw=%llu\n",
      zort_startup_calls,
      zort_start_program_calls,
      caml_globals_inited,
      zort_metadata_registered,
      zort_metadata_frametables,
      zort_metadata_frame_descriptors,
      zort_metadata_gc_root_tables,
      zort_metadata_gc_root_entries,
      zort_metadata_gc_root_blocks,
      zort_metadata_gc_root_block_fields,
      root_slot_count,
      first_root_slot_present,
      first_root_slot_points_to_block,
      second_root_slot_present,
      zort_metadata_code_segments,
      zort_metadata_data_segments,
      zort_metadata_program_fragment_registered,
      zort_last_start_program_result);

  int ok = caml_globals_inited == 1 &&
           zort_metadata_registered == 1 &&
           zort_metadata_gc_root_block_fields == 1 &&
           root_slot_count == 1 &&
           first_root_slot_present == 1 &&
           first_root_slot_points_to_block == 1 &&
           second_root_slot_present == 0 &&
           zort_metadata_program_fragment_registered == 1 &&
           zort_last_start_program_result == 1;
  caml_shutdown();
  return ok ? 0 : 1;
}
