#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <fcntl.h>
#include <dirent.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include "kernel_new_errors.h"

#if defined(__APPLE__)
#include <sys/clonefile.h>
#endif

#define KERNEL_NEW_FILE_FLAG_READ_ONLY 1
#define KERNEL_NEW_FILE_FLAG_WRITE_ONLY (1 << 1)
#define KERNEL_NEW_FILE_FLAG_READ_WRITE (1 << 2)
#define KERNEL_NEW_FILE_FLAG_CREATE (1 << 3)
#define KERNEL_NEW_FILE_FLAG_TRUNCATE (1 << 4)
#define KERNEL_NEW_FILE_FLAG_APPEND (1 << 5)
#define KERNEL_NEW_FILE_FLAG_EXCLUSIVE (1 << 6)

#define KERNEL_NEW_FILE_TYPE_REGULAR 0
#define KERNEL_NEW_FILE_TYPE_DIRECTORY 1
#define KERNEL_NEW_FILE_TYPE_SYMLINK 2
#define KERNEL_NEW_FILE_TYPE_CHARACTER 3
#define KERNEL_NEW_FILE_TYPE_BLOCK 4
#define KERNEL_NEW_FILE_TYPE_FIFO 5
#define KERNEL_NEW_FILE_TYPE_SOCKET 6
#define KERNEL_NEW_FILE_TYPE_UNKNOWN 7

static int kernel_new_file_type_of_mode(mode_t mode) {
  if (S_ISREG(mode)) return KERNEL_NEW_FILE_TYPE_REGULAR;
  if (S_ISDIR(mode)) return KERNEL_NEW_FILE_TYPE_DIRECTORY;
  if (S_ISLNK(mode)) return KERNEL_NEW_FILE_TYPE_SYMLINK;
  if (S_ISCHR(mode)) return KERNEL_NEW_FILE_TYPE_CHARACTER;
  if (S_ISBLK(mode)) return KERNEL_NEW_FILE_TYPE_BLOCK;
  if (S_ISFIFO(mode)) return KERNEL_NEW_FILE_TYPE_FIFO;
  if (S_ISSOCK(mode)) return KERNEL_NEW_FILE_TYPE_SOCKET;
  return KERNEL_NEW_FILE_TYPE_UNKNOWN;
}

#if defined(__APPLE__)
#define KERNEL_NEW_STAT_ATIME_SEC(st) ((st).st_atimespec.tv_sec)
#define KERNEL_NEW_STAT_ATIME_NSEC(st) ((st).st_atimespec.tv_nsec)
#define KERNEL_NEW_STAT_MTIME_SEC(st) ((st).st_mtimespec.tv_sec)
#define KERNEL_NEW_STAT_MTIME_NSEC(st) ((st).st_mtimespec.tv_nsec)
#define KERNEL_NEW_STAT_CTIME_SEC(st) ((st).st_ctimespec.tv_sec)
#define KERNEL_NEW_STAT_CTIME_NSEC(st) ((st).st_ctimespec.tv_nsec)
#else
#define KERNEL_NEW_STAT_ATIME_SEC(st) ((st).st_atim.tv_sec)
#define KERNEL_NEW_STAT_ATIME_NSEC(st) ((st).st_atim.tv_nsec)
#define KERNEL_NEW_STAT_MTIME_SEC(st) ((st).st_mtim.tv_sec)
#define KERNEL_NEW_STAT_MTIME_NSEC(st) ((st).st_mtim.tv_nsec)
#define KERNEL_NEW_STAT_CTIME_SEC(st) ((st).st_ctim.tv_sec)
#define KERNEL_NEW_STAT_CTIME_NSEC(st) ((st).st_ctim.tv_nsec)
#endif

static int64_t kernel_new_timespec_to_ns(int64_t sec, int64_t nsec) {
  return (sec * 1000000000LL) + nsec;
}

static value kernel_new_metadata_of_stat(struct stat *st) {
  CAMLparam0();
  CAMLlocal1(tuple);

  tuple = caml_alloc_tuple(12);
  Store_field(tuple, 0, Val_int(kernel_new_file_type_of_mode(st->st_mode)));
  Store_field(tuple, 1, Val_int((int)(st->st_mode & 07777)));
  Store_field(tuple, 2, caml_copy_int64((int64_t)st->st_size));
  Store_field(tuple, 3, Val_int((int)st->st_nlink));
  Store_field(tuple, 4, Val_int((int)st->st_uid));
  Store_field(tuple, 5, Val_int((int)st->st_gid));
  Store_field(tuple, 6, Val_int((int)st->st_dev));
  Store_field(tuple, 7, Val_int((int)st->st_ino));
  Store_field(tuple, 8, Val_int((int)st->st_rdev));
  Store_field(tuple, 9,
    caml_copy_int64(kernel_new_timespec_to_ns(
      (int64_t)KERNEL_NEW_STAT_ATIME_SEC((*st)),
      (int64_t)KERNEL_NEW_STAT_ATIME_NSEC((*st)))));
  Store_field(tuple, 10,
    caml_copy_int64(kernel_new_timespec_to_ns(
      (int64_t)KERNEL_NEW_STAT_MTIME_SEC((*st)),
      (int64_t)KERNEL_NEW_STAT_MTIME_NSEC((*st)))));
  Store_field(tuple, 11,
    caml_copy_int64(kernel_new_timespec_to_ns(
      (int64_t)KERNEL_NEW_STAT_CTIME_SEC((*st)),
      (int64_t)KERNEL_NEW_STAT_CTIME_NSEC((*st)))));

  CAMLreturn(tuple);
}

static int kernel_new_file_configure_fd(int fd) {
  if (fcntl(fd, F_SETFD, FD_CLOEXEC) == -1) {
    return -1;
  }

  int current_flags = fcntl(fd, F_GETFL, 0);
  if (current_flags == -1) {
    return -1;
  }

  if (fcntl(fd, F_SETFL, current_flags | O_NONBLOCK) == -1) {
    return -1;
  }

  return 0;
}

static int kernel_new_file_open_flags(int flags_mask) {
  int flags = 0;

  if ((flags_mask & KERNEL_NEW_FILE_FLAG_READ_WRITE) != 0) {
    flags |= O_RDWR;
  } else if ((flags_mask & KERNEL_NEW_FILE_FLAG_WRITE_ONLY) != 0) {
    flags |= O_WRONLY;
  } else {
    flags |= O_RDONLY;
  }

  if ((flags_mask & KERNEL_NEW_FILE_FLAG_CREATE) != 0) flags |= O_CREAT;
  if ((flags_mask & KERNEL_NEW_FILE_FLAG_TRUNCATE) != 0) flags |= O_TRUNC;
  if ((flags_mask & KERNEL_NEW_FILE_FLAG_APPEND) != 0) flags |= O_APPEND;
  if ((flags_mask & KERNEL_NEW_FILE_FLAG_EXCLUSIVE) != 0) flags |= O_EXCL;

  return flags;
}

static struct iovec *kernel_new_file_build_iovecs(value segments_val, int *count_out) {
  int count = Wosize_val(segments_val);
  if (count == 0) {
    *count_out = 0;
    return NULL;
  }

  struct iovec *iovecs = malloc(sizeof(struct iovec) * count);
  if (iovecs == NULL) {
    caml_raise_out_of_memory();
  }

  for (int index = 0; index < count; index++) {
    value segment_val = Field(segments_val, index);
    int length = (int)Caml_ba_array_val(segment_val)->dim[0];
    iovecs[index].iov_base = (void *)Caml_ba_data_val(segment_val);
    iovecs[index].iov_len = (size_t)length;
  }

  *count_out = count;
  return iovecs;
}

static value kernel_new_copy_string_ok(const char *text) {
  CAMLparam0();
  CAMLlocal1(payload);
  payload = caml_copy_string(text);
  CAMLreturn(kernel_new_result_ok(payload));
}

static char *kernel_new_copy_ocaml_string_bytes(value string_val, mlsize_t *len_out) {
  mlsize_t len = caml_string_length(string_val);
  char *copy = NULL;

  if (len > 0) {
    copy = malloc((size_t)len);
    if (copy == NULL) {
      caml_raise_out_of_memory();
    }

    memcpy(copy, String_val(string_val), (size_t)len);
  }

  *len_out = len;
  return copy;
}

#if defined(__APPLE__)
static char *kernel_new_copy_ocaml_cstring(value string_val) {
  mlsize_t len = caml_string_length(string_val);
  char *copy = malloc((size_t)len + 1);

  if (copy == NULL) {
    caml_raise_out_of_memory();
  }

  memcpy(copy, String_val(string_val), (size_t)len);
  copy[len] = '\0';
  return copy;
}
#endif

static char *kernel_new_copy_ocaml_bytes_slice(value bytes_val, int pos, int len) {
  char *copy = NULL;

  if (len > 0) {
    copy = malloc((size_t)len);
    if (copy == NULL) {
      caml_raise_out_of_memory();
    }

    memcpy(copy, Bytes_val(bytes_val) + pos, (size_t)len);
  }

  return copy;
}

static ssize_t kernel_new_file_read_into_heap_bytes(int fd, value buffer_val, int pos, int len) {
  char *copy = NULL;
  ssize_t result;

  if (len > 0) {
    copy = malloc((size_t)len);
    if (copy == NULL) {
      caml_raise_out_of_memory();
    }
  }

  caml_enter_blocking_section();
  result = read(fd, copy, (size_t)len);
  caml_leave_blocking_section();

  if (result > 0) {
    memcpy(Bytes_val(buffer_val) + pos, copy, (size_t)result);
  }

  int saved_errno = errno;
  free(copy);
  errno = saved_errno;
  return result;
}

static ssize_t kernel_new_file_write_from_heap_bytes(int fd, value buffer_val, int pos, int len) {
  char *copy = kernel_new_copy_ocaml_bytes_slice(buffer_val, pos, len);
  ssize_t result;

  caml_enter_blocking_section();
  result = write(fd, copy, (size_t)len);
  caml_leave_blocking_section();

  int saved_errno = errno;
  free(copy);
  errno = saved_errno;
  return result;
}

CAMLprim value kernel_new_iovec_slice_create(value vlength) {
  CAMLparam1(vlength);
  intnat length = Long_val(vlength);

  if (length < 0) {
    caml_invalid_argument("Kernel.IO.Iovec.IoSlice.create");
  }

  CAMLreturn(
    caml_ba_alloc_dims(CAML_BA_CHAR | CAML_BA_C_LAYOUT | CAML_BA_MANAGED, 1, NULL, length));
}

CAMLprim value kernel_new_iovec_slice_blit(
  value src_val,
  value src_offset_val,
  value dst_val,
  value dst_offset_val,
  value len_val) {
  memmove(
    (char *)Caml_ba_data_val(dst_val) + Long_val(dst_offset_val),
    (char *)Caml_ba_data_val(src_val) + Long_val(src_offset_val),
    (size_t)Long_val(len_val));
  return Val_unit;
}

CAMLprim value kernel_new_iovec_slice_blit_from_bytes(
  value src_val,
  value src_offset_val,
  value dst_val,
  value dst_offset_val,
  value len_val) {
  memcpy(
    (char *)Caml_ba_data_val(dst_val) + Long_val(dst_offset_val),
    Bytes_val(src_val) + Long_val(src_offset_val),
    (size_t)Long_val(len_val));
  return Val_unit;
}

CAMLprim value kernel_new_iovec_slice_blit_from_string(
  value src_val,
  value src_offset_val,
  value dst_val,
  value dst_offset_val,
  value len_val) {
  memcpy(
    (char *)Caml_ba_data_val(dst_val) + Long_val(dst_offset_val),
    String_val(src_val) + Long_val(src_offset_val),
    (size_t)Long_val(len_val));
  return Val_unit;
}

CAMLprim value kernel_new_iovec_slice_blit_to_bytes(
  value src_val,
  value src_offset_val,
  value dst_val,
  value dst_offset_val,
  value len_val) {
  memcpy(
    Bytes_val(dst_val) + Long_val(dst_offset_val),
    (char *)Caml_ba_data_val(src_val) + Long_val(src_offset_val),
    (size_t)Long_val(len_val));
  return Val_unit;
}

static int kernel_new_write_all_bytes(int fd, const char *buffer, size_t len) {
  size_t offset = 0;

  while (offset < len) {
    ssize_t result;

    caml_enter_blocking_section();
    result = write(fd, buffer + offset, len - offset);
    caml_leave_blocking_section();

    if (result == -1) {
      return -errno;
    }

    if (result == 0) {
      return -EIO;
    }

    offset += (size_t)result;
  }

  return 0;
}

static int kernel_new_writev_all_2(
  int fd,
  const char *left_buffer,
  size_t left_len,
  const char *right_buffer,
  size_t right_len) {
  size_t left_offset = 0;
  size_t right_offset = 0;

  while ((left_offset < left_len) || (right_offset < right_len)) {
    struct iovec iovecs[2];
    int iovecs_len = 0;
    ssize_t result;

    if (left_offset < left_len) {
      iovecs[iovecs_len].iov_base = (void *)(left_buffer + left_offset);
      iovecs[iovecs_len].iov_len = left_len - left_offset;
      iovecs_len += 1;
    }

    if (right_offset < right_len) {
      iovecs[iovecs_len].iov_base = (void *)(right_buffer + right_offset);
      iovecs[iovecs_len].iov_len = right_len - right_offset;
      iovecs_len += 1;
    }

    caml_enter_blocking_section();
    result = writev(fd, iovecs, iovecs_len);
    caml_leave_blocking_section();

    if (result == -1) {
      return -errno;
    }

    if (result == 0) {
      return -EIO;
    }

    if ((size_t)result < (left_len - left_offset)) {
      left_offset += (size_t)result;
    } else {
      size_t left_remaining = left_len - left_offset;
      size_t right_written = (size_t)result - left_remaining;
      left_offset = left_len;
      right_offset += right_written;
    }
  }

  return 0;
}

CAMLprim value kernel_new_fs_file_open(value path_val, value flags_mask_val, value perm_val) {
  CAMLparam3(path_val, flags_mask_val, perm_val);

  int fd = open(String_val(path_val), kernel_new_file_open_flags(Int_val(flags_mask_val)), Int_val(perm_val));
  if (fd == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_file_configure_fd(fd) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(fd)));
}

CAMLprim value kernel_new_fs_file_close(value fd_val) {
  CAMLparam1(fd_val);

  if (close(Int_val(fd_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_try_lock_exclusive(value fd_val) {
  CAMLparam1(fd_val);

  if (flock(Int_val(fd_val), LOCK_EX | LOCK_NB) == -1) {
    if (errno == EWOULDBLOCK || errno == EAGAIN) {
      CAMLreturn(kernel_new_result_ok(Val_bool(0)));
    }

    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_bool(1)));
}

CAMLprim value kernel_new_fs_file_unlock(value fd_val) {
  CAMLparam1(fd_val);

  if (flock(Int_val(fd_val), LOCK_UN) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_read(value fd_val, value buffer_val, value pos_val, value len_val) {
  CAMLparam4(fd_val, buffer_val, pos_val, len_val);

  ssize_t result = kernel_new_file_read_into_heap_bytes(
    Int_val(fd_val),
    buffer_val,
    Int_val(pos_val),
    Int_val(len_val));

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_fs_file_write(value fd_val, value buffer_val, value pos_val, value len_val) {
  CAMLparam4(fd_val, buffer_val, pos_val, len_val);

  ssize_t result = kernel_new_file_write_from_heap_bytes(
    Int_val(fd_val),
    buffer_val,
    Int_val(pos_val),
    Int_val(len_val));

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_stdio_print(value fd_val, value message_val) {
  CAMLparam2(fd_val, message_val);
  mlsize_t message_len;
  char *message = kernel_new_copy_ocaml_string_bytes(message_val, &message_len);

  int error = kernel_new_write_all_bytes(
    Int_val(fd_val),
    message,
    (size_t)message_len);

  free(message);

  if (error < 0) {
    errno = -error;
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_stdio_println(value fd_val, value message_val) {
  CAMLparam2(fd_val, message_val);

  static const char newline[] = "\n";
  mlsize_t message_len;
  char *message = kernel_new_copy_ocaml_string_bytes(message_val, &message_len);
  int error = kernel_new_writev_all_2(
    Int_val(fd_val),
    message,
    (size_t)message_len,
    newline,
    1);

  free(message);

  if (error < 0) {
    errno = -error;
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_write_raw(value fd_val, value buffer_val, value pos_val, value len_val) {
  CAMLparam4(fd_val, buffer_val, pos_val, len_val);

  ssize_t result = kernel_new_file_write_from_heap_bytes(
    Int_val(fd_val),
    buffer_val,
    Int_val(pos_val),
    Int_val(len_val));

  if (result == -1) {
    CAMLreturn(Val_int(-errno));
  }

  CAMLreturn(Val_int((int)result));
}

CAMLprim value kernel_new_fs_file_write_all_raw(
  value fd_val,
  value buffer_val,
  value pos_val,
  value len_val) {
  CAMLparam4(fd_val, buffer_val, pos_val, len_val);

  int fd = Int_val(fd_val);
  int pos = Int_val(pos_val);
  int remaining = Int_val(len_val);
  char *copy = kernel_new_copy_ocaml_bytes_slice(buffer_val, pos, remaining);
  char *cursor = copy;

  while (remaining > 0) {
    ssize_t result;

    caml_enter_blocking_section();
    result = write(fd, cursor, (size_t)remaining);
    caml_leave_blocking_section();

    if (result == -1) {
      free(copy);
      CAMLreturn(Val_int(-errno));
    }

    if (result == 0) {
      free(copy);
      CAMLreturn(Val_int(0));
    }

    cursor += (int)result;
    remaining -= (int)result;
  }

  free(copy);
  CAMLreturn(len_val);
}

CAMLprim value kernel_new_fs_file_readv(value fd_val, value segments_val) {
  CAMLparam2(fd_val, segments_val);

  int count = 0;
  struct iovec *iovecs = kernel_new_file_build_iovecs(segments_val, &count);
  ssize_t result;

  if (count == 0) {
    CAMLreturn(kernel_new_result_ok(Val_int(0)));
  }

  caml_enter_blocking_section();
  result = readv(Int_val(fd_val), iovecs, count);
  caml_leave_blocking_section();

  free(iovecs);

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_fs_file_writev(value fd_val, value segments_val) {
  CAMLparam2(fd_val, segments_val);

  int count = 0;
  struct iovec *iovecs = kernel_new_file_build_iovecs(segments_val, &count);
  ssize_t result;

  if (count == 0) {
    CAMLreturn(kernel_new_result_ok(Val_int(0)));
  }

  caml_enter_blocking_section();
  result = writev(Int_val(fd_val), iovecs, count);
  caml_leave_blocking_section();

  free(iovecs);

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_fs_file_pipe(value unit_val) {
  CAMLparam1(unit_val);
  CAMLlocal2(pair, result);

  int fds[2];
  if (pipe(fds) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_file_configure_fd(fds[0]) == -1) {
    int saved_errno = errno;
    close(fds[0]);
    close(fds[1]);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_file_configure_fd(fds[1]) == -1) {
    int saved_errno = errno;
    close(fds[0]);
    close(fds[1]);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, Val_int(fds[0]));
  Store_field(pair, 1, Val_int(fds[1]));
  result = kernel_new_result_ok(pair);
  CAMLreturn(result);
}

CAMLprim value kernel_new_fs_file_mkdir(value path_val, value perm_val) {
  CAMLparam2(path_val, perm_val);

  if (mkdir(String_val(path_val), (mode_t)Int_val(perm_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_chmod(value path_val, value perm_val) {
  CAMLparam2(path_val, perm_val);

  if (chmod(String_val(path_val), (mode_t)Int_val(perm_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_rmdir(value path_val) {
  CAMLparam1(path_val);

  if (rmdir(String_val(path_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_remove(value path_val) {
  CAMLparam1(path_val);

  if (unlink(String_val(path_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_rename(value src_val, value dst_val) {
  CAMLparam2(src_val, dst_val);

  if (rename(String_val(src_val), String_val(dst_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_link(value src_val, value dst_val) {
  CAMLparam2(src_val, dst_val);

  if (link(String_val(src_val), String_val(dst_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_clone(value src_val, value dst_val) {
  CAMLparam2(src_val, dst_val);

#if defined(__APPLE__)
  char *src = kernel_new_copy_ocaml_cstring(src_val);
  char *dst = kernel_new_copy_ocaml_cstring(dst_val);
  int result;
  int saved_errno = 0;

  caml_enter_blocking_section();
  result = clonefile(src, dst, 0);
  if (result == -1) {
    saved_errno = errno;
  }
  caml_leave_blocking_section();

  free(src);
  free(dst);
  errno = saved_errno;

  if (result == -1) {
#ifdef EXDEV
    if (saved_errno == EXDEV) {
      CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
    }
#endif
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
#else
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
#endif
}

CAMLprim value kernel_new_fs_file_symlink(value src_val, value dst_val) {
  CAMLparam2(src_val, dst_val);

  if (symlink(String_val(src_val), String_val(dst_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_readlink(value path_val) {
  CAMLparam1(path_val);

  char buffer[PATH_MAX];
  ssize_t len = readlink(String_val(path_val), buffer, sizeof(buffer) - 1);
  if (len == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  buffer[len] = '\0';
  CAMLreturn(kernel_new_copy_string_ok(buffer));
}

CAMLprim value kernel_new_fs_file_realpath(value path_val) {
  CAMLparam1(path_val);

  char *resolved = realpath(String_val(path_val), NULL);
  if (resolved == NULL) {
    CAMLreturn(kernel_new_result_errno());
  }

  value result = kernel_new_copy_string_ok(resolved);
  free(resolved);
  CAMLreturn(result);
}

CAMLprim value kernel_new_fs_file_stat(value path_val) {
  CAMLparam1(path_val);
  struct stat st;

  if (stat(String_val(path_val), &st) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(kernel_new_metadata_of_stat(&st)));
}

CAMLprim value kernel_new_fs_file_lstat(value path_val) {
  CAMLparam1(path_val);
  struct stat st;

  if (lstat(String_val(path_val), &st) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(kernel_new_metadata_of_stat(&st)));
}

CAMLprim value kernel_new_fs_file_fstat(value fd_val) {
  CAMLparam1(fd_val);
  struct stat st;

  if (fstat(Int_val(fd_val), &st) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(kernel_new_metadata_of_stat(&st)));
}

CAMLprim value kernel_new_fs_file_readdir(value path_val) {
  CAMLparam1(path_val);
  CAMLlocal2(entries, item);

  DIR *dir = opendir(String_val(path_val));
  if (dir == NULL) {
    CAMLreturn(kernel_new_result_errno());
  }

  int count = 0;
  struct dirent *entry;
  errno = 0;
  while ((entry = readdir(dir)) != NULL) {
    if (strcmp(entry->d_name, ".") != 0 && strcmp(entry->d_name, "..") != 0) {
      count++;
    }
  }

  if (errno != 0) {
    int saved_errno = errno;
    closedir(dir);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  rewinddir(dir);
  entries = caml_alloc(count, 0);

  int index = 0;
  errno = 0;
  while ((entry = readdir(dir)) != NULL) {
    if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
      continue;
    }
    item = caml_copy_string(entry->d_name);
    Store_field(entries, index, item);
    index++;
  }

  if (errno != 0) {
    int saved_errno = errno;
    closedir(dir);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (closedir(dir) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(entries));
}

CAMLprim value kernel_new_fs_file_getcwd(value unit_val) {
  CAMLparam1(unit_val);

  char buffer[PATH_MAX];
  if (getcwd(buffer, sizeof(buffer)) == NULL) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_copy_string_ok(buffer));
}

CAMLprim value kernel_new_fs_file_chdir(value path_val) {
  CAMLparam1(path_val);

  if (chdir(String_val(path_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_file_isatty(value fd_val) {
  CAMLparam1(fd_val);
  CAMLreturn(Val_bool(isatty(Int_val(fd_val)) == 1));
}
