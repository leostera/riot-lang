#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <dirent.h>
#include <errno.h>

typedef struct {
  DIR *dir;
} kernel_fs_read_dir_t;

static kernel_fs_read_dir_t *kernel_fs_read_dir_data(value v_dir) {
  return (kernel_fs_read_dir_t *) Data_custom_val(v_dir);
}

static void kernel_fs_read_dir_finalize(value v_dir) {
  kernel_fs_read_dir_t *dir = kernel_fs_read_dir_data(v_dir);
  if (dir->dir != NULL) {
    closedir(dir->dir);
    dir->dir = NULL;
  }
}

static struct custom_operations kernel_fs_read_dir_ops = {
  "riot.kernel.fs.read_dir",
  kernel_fs_read_dir_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value kernel_fs_read_dir_kind(unsigned char d_type) {
  switch (d_type) {
#ifdef DT_REG
    case DT_REG: return Val_int(1);
#endif
#ifdef DT_DIR
    case DT_DIR: return Val_int(2);
#endif
#ifdef DT_LNK
    case DT_LNK: return Val_int(3);
#endif
#ifdef DT_BLK
    case DT_BLK: return Val_int(4);
#endif
#ifdef DT_CHR
    case DT_CHR: return Val_int(5);
#endif
#ifdef DT_FIFO
    case DT_FIFO: return Val_int(6);
#endif
#ifdef DT_SOCK
    case DT_SOCK: return Val_int(7);
#endif
    default: return Val_int(0);
  }
}

CAMLprim value kernel_fs_read_dir_open(value v_path) {
  CAMLparam1(v_path);
  CAMLlocal1(v_dir);

  DIR *dir = opendir(String_val(v_path));
  if (dir == NULL) {
    caml_uerror("opendir", v_path);
  }

  v_dir = caml_alloc_custom(&kernel_fs_read_dir_ops, sizeof(kernel_fs_read_dir_t), 0, 1);
  kernel_fs_read_dir_data(v_dir)->dir = dir;
  CAMLreturn(v_dir);
}

CAMLprim value kernel_fs_read_dir_read_entry(value v_dir) {
  CAMLparam1(v_dir);
  CAMLlocal2(v_entry, v_name);

  kernel_fs_read_dir_t *dir = kernel_fs_read_dir_data(v_dir);
  if (dir->dir == NULL) {
    caml_raise_end_of_file();
  }

  errno = 0;
  struct dirent *entry = readdir(dir->dir);
  if (entry == NULL) {
    if (errno != 0) {
      caml_uerror("readdir", Nothing);
    }
    caml_raise_end_of_file();
  }

  v_name = caml_copy_string(entry->d_name);
  v_entry = caml_alloc_tuple(2);
  Store_field(v_entry, 0, v_name);
  Store_field(v_entry, 1, kernel_fs_read_dir_kind(entry->d_type));
  CAMLreturn(v_entry);
}

CAMLprim value kernel_fs_read_dir_close(value v_dir) {
  CAMLparam1(v_dir);

  kernel_fs_read_dir_t *dir = kernel_fs_read_dir_data(v_dir);
  if (dir->dir != NULL) {
    if (closedir(dir->dir) == -1) {
      caml_uerror("closedir", Nothing);
    }
    dir->dir = NULL;
  }

  CAMLreturn(Val_unit);
}
