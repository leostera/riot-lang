#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#include "tty_errors.h"

static struct custom_operations tty_termios_ops = {
  "riot.tty.termios",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value tty_alloc_termios(const struct termios *source) {
  value block = caml_alloc_custom(&tty_termios_ops, sizeof(struct termios), 0, 1);
  memcpy(Data_custom_val(block), source, sizeof(struct termios));
  return block;
}

static struct termios *tty_termios_val(value value_termios) {
  return (struct termios *)Data_custom_val(value_termios);
}

CAMLprim value tty_stdin_fd(value unit_val) {
  (void)unit_val;
  return Val_int(STDIN_FILENO);
}

CAMLprim value tty_stdout_fd(value unit_val) {
  (void)unit_val;
  return Val_int(STDOUT_FILENO);
}

CAMLprim value tty_stderr_fd(value unit_val) {
  (void)unit_val;
  return Val_int(STDERR_FILENO);
}

CAMLprim value tty_open_tty(value unit_val) {
  CAMLparam1(unit_val);
  int fd = open("/dev/tty", O_RDWR | O_CLOEXEC);
  if (fd == -1) {
    CAMLreturn(tty_result_errno());
  }
  CAMLreturn(tty_result_ok(Val_int(fd)));
}

CAMLprim value tty_close(value fd_val) {
  CAMLparam1(fd_val);
  if (close(Int_val(fd_val)) == -1) {
    CAMLreturn(tty_result_errno());
  }
  CAMLreturn(tty_result_ok(Val_unit));
}

CAMLprim value tty_is_tty(value fd_val) {
  CAMLparam1(fd_val);
  CAMLreturn(Val_bool(isatty(Int_val(fd_val)) == 1));
}

CAMLprim value tty_get_size(value fd_val) {
  CAMLparam1(fd_val);
  CAMLlocal3(pair, cols, rows);
  struct winsize ws;

  if (ioctl(Int_val(fd_val), TIOCGWINSZ, &ws) == -1) {
    CAMLreturn(tty_result_errno());
  }

  pair = caml_alloc_tuple(2);
  cols = Val_int(ws.ws_col);
  rows = Val_int(ws.ws_row);
  Store_field(pair, 0, cols);
  Store_field(pair, 1, rows);
  CAMLreturn(tty_result_ok(pair));
}

CAMLprim value tty_get_attributes(value fd_val) {
  CAMLparam1(fd_val);
  struct termios attrs;

  if (tcgetattr(Int_val(fd_val), &attrs) == -1) {
    CAMLreturn(tty_result_errno());
  }

  CAMLreturn(tty_result_ok(tty_alloc_termios(&attrs)));
}

CAMLprim value tty_set_attributes(value fd_val, value when_val, value termios_val) {
  CAMLparam3(fd_val, when_val, termios_val);
  int when_to_apply = TCSANOW;

  switch (Int_val(when_val)) {
    case 0: when_to_apply = TCSANOW; break;
    case 1: when_to_apply = TCSADRAIN; break;
    case 2: when_to_apply = TCSAFLUSH; break;
    default:
      errno = EINVAL;
      CAMLreturn(tty_result_errno());
  }

  if (tcsetattr(Int_val(fd_val), when_to_apply, tty_termios_val(termios_val)) == -1) {
    CAMLreturn(tty_result_errno());
  }

  CAMLreturn(tty_result_ok(Val_unit));
}

CAMLprim value tty_make_raw_mode(value termios_val) {
  CAMLparam1(termios_val);
  struct termios copy = *tty_termios_val(termios_val);
  copy.c_lflag &= (tcflag_t)~(ECHO | ICANON);
  copy.c_iflag &= (tcflag_t)~ICRNL;
  CAMLreturn(tty_alloc_termios(&copy));
}

CAMLprim value tty_default_termios(value unit_val) {
  CAMLparam1(unit_val);
  struct termios attrs;
  memset(&attrs, 0, sizeof(struct termios));
  CAMLreturn(tty_alloc_termios(&attrs));
}

CAMLprim value tty_read(value fd_val, value bytes_val, value offset_val, value len_val) {
  CAMLparam4(fd_val, bytes_val, offset_val, len_val);
  ssize_t read_count = read(
    Int_val(fd_val),
    &Byte_u(bytes_val, Int_val(offset_val)),
    (size_t)Int_val(len_val));

  if (read_count == -1) {
    CAMLreturn(tty_result_errno());
  }

  CAMLreturn(tty_result_ok(Val_int(read_count)));
}

CAMLprim value tty_write(value fd_val, value bytes_val, value offset_val, value len_val) {
  CAMLparam4(fd_val, bytes_val, offset_val, len_val);
  ssize_t written_count = write(
    Int_val(fd_val),
    &Byte_u(bytes_val, Int_val(offset_val)),
    (size_t)Int_val(len_val));

  if (written_count == -1) {
    CAMLreturn(tty_result_errno());
  }

  CAMLreturn(tty_result_ok(Val_int(written_count)));
}
