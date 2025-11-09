#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <time.h>

#if defined(__linux__) && !defined(__GLIBC__)

#include <sys/sendfile.h>
#include <sys/stat.h>
#include <unistd.h>
#include "syscall.h"

#elif defined(__linux__) && defined(__GLIBC__)

#include <sys/sendfile.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#elif __APPLE__

#include <copyfile.h>
#include <unistd.h>

#endif

CAMLprim value kernel_unix_sendfile(value v_fd, value v_s, value v_offset, value v_len) {
  CAMLparam4(v_fd, v_s, v_offset, v_len);
  int fd = Int_val(v_fd);
  int s = Int_val(v_s);
  off_t offset = Int_val(v_offset);

#ifdef __APPLE__
   off_t len = Int_val(v_len);
   int ret = sendfile(fd, s, offset, &len, NULL, 0);
#else
   size_t len = Int_val(v_len);
   int ret = sendfile(fd, s, &offset, len);
#endif

  if (ret == -1) uerror("sendfile", Nothing);

  CAMLreturn(Val_int(len));
}

/*

This code taken from Eio's eio_posix_stub.c, licensed as:

Copyright (C) 2021 Anil Madhavapeddy  
Copyright (C) 2022 Thomas Leonard  

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

*/

/* Fill [iov] with pointers to the cstructs in the array [v_bufs]. */
static void fill_iov(struct iovec *iov, value v_bufs) {
  int n_bufs = Wosize_val(v_bufs);
  for (int i = 0; i < n_bufs; i++) {
    value v_cs = Field(v_bufs, i);
    value v_ba = Field(v_cs, 0);
    value v_off = Field(v_cs, 1);
    value v_len = Field(v_cs, 2);
    iov[i].iov_base = Bytes_val(v_ba) + Long_val(v_off);
    iov[i].iov_len = Long_val(v_len);
  }
}

CAMLprim value kernel_unix_readv(value v_fd, value v_bufs) {
  CAMLparam1(v_bufs);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec iov[n_bufs];

  fill_iov(iov, v_bufs);

  caml_enter_blocking_section();
  r = readv(Int_val(v_fd), iov, n_bufs);
  caml_leave_blocking_section();
  if (r < 0) uerror("readv", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value kernel_unix_writev(value v_fd, value v_bufs) {
  CAMLparam1(v_bufs);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec iov[n_bufs];

  fill_iov(iov, v_bufs);

  caml_enter_blocking_section();
  r = writev(Int_val(v_fd), iov, n_bufs);
  caml_leave_blocking_section();
  if (r < 0) uerror("writev", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value kernel_unix_copy_file(value v_src_fd, value v_dst_fd) {
  CAMLparam2(v_src_fd, v_dst_fd);
  int src_fd = Int_val(v_src_fd);
  int dst_fd = Int_val(v_dst_fd);
  int ret = -1;

#ifdef __APPLE__
  /* Use fcopyfile on macOS for efficient copying */
  caml_enter_blocking_section();
  ret = fcopyfile(src_fd, dst_fd, NULL, COPYFILE_DATA);
  caml_leave_blocking_section();
  if (ret < 0) uerror("fcopyfile", Nothing);
#else
  /* Use copy_file_range on Linux */
  struct stat st;
  if (fstat(src_fd, &st) < 0) uerror("fstat", Nothing);
  
  off_t offset = 0;
  size_t remaining = st.st_size;
  
  caml_enter_blocking_section();
  while (remaining > 0) {
    ssize_t copied = copy_file_range(src_fd, &offset, dst_fd, NULL, remaining, 0);
    if (copied < 0) {
      caml_leave_blocking_section();
      uerror("copy_file_range", Nothing);
    }
    if (copied == 0) break;  /* EOF */
    remaining -= copied;
  }
  caml_leave_blocking_section();
#endif

  CAMLreturn(Val_unit);
}
