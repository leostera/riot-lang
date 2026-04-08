#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <errno.h>
#include <fcntl.h>
#include <spawn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
#include "kernel_new_errors.h"

#define KERNEL_NEW_PROCESS_STDIO_NULL 0
#define KERNEL_NEW_PROCESS_STDIO_PIPE 1
#define KERNEL_NEW_PROCESS_STDIO_INHERIT 2
#define KERNEL_NEW_PROCESS_STDIO_FILE 3
#define KERNEL_NEW_PROCESS_STDIO_REDIRECT_TO_STDOUT 4

#define KERNEL_NEW_PROCESS_STATUS_EXITED 0
#define KERNEL_NEW_PROCESS_STATUS_SIGNALED 1
#define KERNEL_NEW_PROCESS_STATUS_STOPPED 2

extern char **environ;

static int kernel_new_process_set_cloexec(int fd) {
  int flags = fcntl(fd, F_GETFD, 0);
  if (flags == -1) {
    return -1;
  }

  if (fcntl(fd, F_SETFD, flags | FD_CLOEXEC) == -1) {
    return -1;
  }

  return 0;
}

static int kernel_new_process_set_nonblocking(int fd) {
  int flags = fcntl(fd, F_GETFL, 0);
  if (flags == -1) {
    return -1;
  }

  if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
    return -1;
  }

  return 0;
}

static void kernel_new_process_close_if_open(int fd) {
  if (fd >= 0) {
    close(fd);
  }
}

static value kernel_new_process_some_int(int raw) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_int(raw));
  CAMLreturn(result);
}

static value kernel_new_process_some_status(int tag, int code) {
  CAMLparam0();
  CAMLlocal2(tuple, result);
  tuple = caml_alloc_tuple(2);
  Store_field(tuple, 0, Val_int(tag));
  Store_field(tuple, 1, Val_int(code));
  result = caml_alloc(1, 0);
  Store_field(result, 0, tuple);
  CAMLreturn(result);
}

static int kernel_new_process_apply_env(value env_val) {
  int count = Wosize_val(env_val);
  for (int index = 0; index < count; index++) {
    value entry = Field(env_val, index);
    if (setenv(String_val(Field(entry, 0)), String_val(Field(entry, 1)), 1) == -1) {
      return -1;
    }
  }
  return 0;
}

static int kernel_new_process_open_dev_null(int flags) {
  int fd = open("/dev/null", flags);
  return fd;
}

static int kernel_new_process_dup_to(int source_fd, int target_fd) {
  if (source_fd == target_fd) {
    return 0;
  }

  if (dup2(source_fd, target_fd) == -1) {
    return -1;
  }

  return 0;
}

static int kernel_new_process_setup_stdio(
  int stdin_mode,
  int stdin_file,
  int stdin_read_end,
  int stdout_mode,
  int stdout_file,
  int stdout_write_end,
  int stderr_mode,
  int stderr_file,
  int stderr_write_end)
{
  int dev_null = -1;

  switch (stdin_mode) {
    case KERNEL_NEW_PROCESS_STDIO_NULL:
      dev_null = kernel_new_process_open_dev_null(O_RDONLY);
      if (dev_null == -1) {
        return -1;
      }
      if (kernel_new_process_dup_to(dev_null, STDIN_FILENO) == -1) {
        close(dev_null);
        return -1;
      }
      close(dev_null);
      break;
    case KERNEL_NEW_PROCESS_STDIO_PIPE:
      if (kernel_new_process_dup_to(stdin_read_end, STDIN_FILENO) == -1) {
        return -1;
      }
      break;
    case KERNEL_NEW_PROCESS_STDIO_FILE:
      if (kernel_new_process_dup_to(stdin_file, STDIN_FILENO) == -1) {
        return -1;
      }
      break;
    case KERNEL_NEW_PROCESS_STDIO_INHERIT:
      break;
    default:
      errno = EINVAL;
      return -1;
  }

  switch (stdout_mode) {
    case KERNEL_NEW_PROCESS_STDIO_NULL:
      dev_null = kernel_new_process_open_dev_null(O_WRONLY);
      if (dev_null == -1) {
        return -1;
      }
      if (kernel_new_process_dup_to(dev_null, STDOUT_FILENO) == -1) {
        close(dev_null);
        return -1;
      }
      close(dev_null);
      break;
    case KERNEL_NEW_PROCESS_STDIO_PIPE:
      if (kernel_new_process_dup_to(stdout_write_end, STDOUT_FILENO) == -1) {
        return -1;
      }
      break;
    case KERNEL_NEW_PROCESS_STDIO_FILE:
      if (kernel_new_process_dup_to(stdout_file, STDOUT_FILENO) == -1) {
        return -1;
      }
      break;
    case KERNEL_NEW_PROCESS_STDIO_INHERIT:
      break;
    default:
      errno = EINVAL;
      return -1;
  }

  switch (stderr_mode) {
    case KERNEL_NEW_PROCESS_STDIO_NULL:
      dev_null = kernel_new_process_open_dev_null(O_WRONLY);
      if (dev_null == -1) {
        return -1;
      }
      if (kernel_new_process_dup_to(dev_null, STDERR_FILENO) == -1) {
        close(dev_null);
        return -1;
      }
      close(dev_null);
      break;
    case KERNEL_NEW_PROCESS_STDIO_PIPE:
      if (kernel_new_process_dup_to(stderr_write_end, STDERR_FILENO) == -1) {
        return -1;
      }
      break;
    case KERNEL_NEW_PROCESS_STDIO_FILE:
      if (kernel_new_process_dup_to(stderr_file, STDERR_FILENO) == -1) {
        return -1;
      }
      break;
    case KERNEL_NEW_PROCESS_STDIO_INHERIT:
      break;
    case KERNEL_NEW_PROCESS_STDIO_REDIRECT_TO_STDOUT:
      if (kernel_new_process_dup_to(STDOUT_FILENO, STDERR_FILENO) == -1) {
        return -1;
      }
      break;
    default:
      errno = EINVAL;
      return -1;
  }

  return 0;
}

CAMLprim value kernel_new_process_spawn(
  value program_val,
  value args_val,
  value env_val,
  value current_dir_val,
  value stdio_val)
{
  CAMLparam5(program_val, args_val, env_val, current_dir_val, stdio_val);
  CAMLlocal5(result, tuple, stdin_val, stdout_val, stderr_val);

  const char *program = String_val(program_val);
  int arg_count = Wosize_val(args_val);
  char **argv = calloc((size_t)arg_count + 2, sizeof(char *));
  int error_pipe[2] = { -1, -1 };
  int stdin_pipe[2] = { -1, -1 };
  int stdout_pipe[2] = { -1, -1 };
  int stderr_pipe[2] = { -1, -1 };
  int stdin_read_end = -1;
  int stdin_write_end = -1;
  int stdout_read_end = -1;
  int stdout_write_end = -1;
  int stderr_read_end = -1;
  int stderr_write_end = -1;
  int stdin_mode = Int_val(Field(stdio_val, 0));
  int stdin_file = Int_val(Field(stdio_val, 1));
  int stdout_mode = Int_val(Field(stdio_val, 2));
  int stdout_file = Int_val(Field(stdio_val, 3));
  int stderr_mode = Int_val(Field(stdio_val, 4));
  int stderr_file = Int_val(Field(stdio_val, 5));

  if (argv == NULL) {
    caml_raise_out_of_memory();
  }

  argv[0] = (char *)program;
  for (int index = 0; index < arg_count; index++) {
    argv[index + 1] = (char *)String_val(Field(args_val, index));
  }
  argv[arg_count + 1] = NULL;

  if (pipe(error_pipe) == -1) {
    free(argv);
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_process_set_cloexec(error_pipe[1]) == -1) {
    int saved_errno = errno;
    kernel_new_process_close_if_open(error_pipe[0]);
    kernel_new_process_close_if_open(error_pipe[1]);
    free(argv);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (stdin_mode == KERNEL_NEW_PROCESS_STDIO_PIPE) {
    if (pipe(stdin_pipe) == -1) {
      int saved_errno = errno;
      kernel_new_process_close_if_open(error_pipe[0]);
      kernel_new_process_close_if_open(error_pipe[1]);
      free(argv);
      errno = saved_errno;
      CAMLreturn(kernel_new_result_errno());
    }
    stdin_read_end = stdin_pipe[0];
    stdin_write_end = stdin_pipe[1];
    if (kernel_new_process_set_nonblocking(stdin_write_end) == -1) {
      int saved_errno = errno;
      kernel_new_process_close_if_open(stdin_read_end);
      kernel_new_process_close_if_open(stdin_write_end);
      kernel_new_process_close_if_open(error_pipe[0]);
      kernel_new_process_close_if_open(error_pipe[1]);
      free(argv);
      errno = saved_errno;
      CAMLreturn(kernel_new_result_errno());
    }
  }

  if (stdout_mode == KERNEL_NEW_PROCESS_STDIO_PIPE) {
    if (pipe(stdout_pipe) == -1) {
      int saved_errno = errno;
      kernel_new_process_close_if_open(stdin_read_end);
      kernel_new_process_close_if_open(stdin_write_end);
      kernel_new_process_close_if_open(error_pipe[0]);
      kernel_new_process_close_if_open(error_pipe[1]);
      free(argv);
      errno = saved_errno;
      CAMLreturn(kernel_new_result_errno());
    }
    stdout_read_end = stdout_pipe[0];
    stdout_write_end = stdout_pipe[1];
    if (kernel_new_process_set_nonblocking(stdout_read_end) == -1) {
      int saved_errno = errno;
      kernel_new_process_close_if_open(stdout_read_end);
      kernel_new_process_close_if_open(stdout_write_end);
      kernel_new_process_close_if_open(stdin_read_end);
      kernel_new_process_close_if_open(stdin_write_end);
      kernel_new_process_close_if_open(error_pipe[0]);
      kernel_new_process_close_if_open(error_pipe[1]);
      free(argv);
      errno = saved_errno;
      CAMLreturn(kernel_new_result_errno());
    }
  }

  if (stderr_mode == KERNEL_NEW_PROCESS_STDIO_PIPE) {
    if (pipe(stderr_pipe) == -1) {
      int saved_errno = errno;
      kernel_new_process_close_if_open(stdout_read_end);
      kernel_new_process_close_if_open(stdout_write_end);
      kernel_new_process_close_if_open(stdin_read_end);
      kernel_new_process_close_if_open(stdin_write_end);
      kernel_new_process_close_if_open(error_pipe[0]);
      kernel_new_process_close_if_open(error_pipe[1]);
      free(argv);
      errno = saved_errno;
      CAMLreturn(kernel_new_result_errno());
    }
    stderr_read_end = stderr_pipe[0];
    stderr_write_end = stderr_pipe[1];
    if (kernel_new_process_set_nonblocking(stderr_read_end) == -1) {
      int saved_errno = errno;
      kernel_new_process_close_if_open(stderr_read_end);
      kernel_new_process_close_if_open(stderr_write_end);
      kernel_new_process_close_if_open(stdout_read_end);
      kernel_new_process_close_if_open(stdout_write_end);
      kernel_new_process_close_if_open(stdin_read_end);
      kernel_new_process_close_if_open(stdin_write_end);
      kernel_new_process_close_if_open(error_pipe[0]);
      kernel_new_process_close_if_open(error_pipe[1]);
      free(argv);
      errno = saved_errno;
      CAMLreturn(kernel_new_result_errno());
    }
  }

  pid_t child_pid = fork();
  if (child_pid == -1) {
    int saved_errno = errno;
    kernel_new_process_close_if_open(stderr_read_end);
    kernel_new_process_close_if_open(stderr_write_end);
    kernel_new_process_close_if_open(stdout_read_end);
    kernel_new_process_close_if_open(stdout_write_end);
    kernel_new_process_close_if_open(stdin_read_end);
    kernel_new_process_close_if_open(stdin_write_end);
    kernel_new_process_close_if_open(error_pipe[0]);
    kernel_new_process_close_if_open(error_pipe[1]);
    free(argv);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (child_pid == 0) {
    int child_errno = 0;

    kernel_new_process_close_if_open(error_pipe[0]);
    kernel_new_process_close_if_open(stdin_write_end);
    kernel_new_process_close_if_open(stdout_read_end);
    kernel_new_process_close_if_open(stderr_read_end);

    if (kernel_new_process_setup_stdio(
          stdin_mode,
          stdin_file,
          stdin_read_end,
          stdout_mode,
          stdout_file,
          stdout_write_end,
          stderr_mode,
          stderr_file,
          stderr_write_end) == -1) {
      child_errno = errno;
      write(error_pipe[1], &child_errno, sizeof(child_errno));
      _exit(127);
    }

    kernel_new_process_close_if_open(stdin_read_end);
    kernel_new_process_close_if_open(stdout_write_end);
    kernel_new_process_close_if_open(stderr_write_end);

    if (Is_block(current_dir_val)) {
      if (chdir(String_val(Field(current_dir_val, 0))) == -1) {
        child_errno = errno;
        write(error_pipe[1], &child_errno, sizeof(child_errno));
        _exit(127);
      }
    }

    if (kernel_new_process_apply_env(env_val) == -1) {
      child_errno = errno;
      write(error_pipe[1], &child_errno, sizeof(child_errno));
      _exit(127);
    }

    execvp(program, argv);
    child_errno = errno;
    write(error_pipe[1], &child_errno, sizeof(child_errno));
    _exit(127);
  }

  kernel_new_process_close_if_open(error_pipe[1]);
  kernel_new_process_close_if_open(stdin_read_end);
  kernel_new_process_close_if_open(stdout_write_end);
  kernel_new_process_close_if_open(stderr_write_end);

  int child_errno = 0;
  ssize_t error_bytes;
  caml_enter_blocking_section();
  error_bytes = read(error_pipe[0], &child_errno, sizeof(child_errno));
  caml_leave_blocking_section();
  kernel_new_process_close_if_open(error_pipe[0]);
  free(argv);

  if (error_bytes > 0) {
    int wait_status = 0;
    waitpid(child_pid, &wait_status, 0);
    kernel_new_process_close_if_open(stdin_write_end);
    kernel_new_process_close_if_open(stdout_read_end);
    kernel_new_process_close_if_open(stderr_read_end);
    errno = child_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  tuple = caml_alloc_tuple(4);
  Store_field(tuple, 0, Val_int((int)child_pid));

  if (stdin_mode == KERNEL_NEW_PROCESS_STDIO_PIPE) {
    stdin_val = kernel_new_process_some_int(stdin_write_end);
  } else {
    stdin_val = Val_int(0);
  }
  Store_field(tuple, 1, stdin_val);

  if (stdout_mode == KERNEL_NEW_PROCESS_STDIO_PIPE) {
    stdout_val = kernel_new_process_some_int(stdout_read_end);
  } else {
    stdout_val = Val_int(0);
  }
  Store_field(tuple, 2, stdout_val);

  if (stderr_mode == KERNEL_NEW_PROCESS_STDIO_PIPE) {
    stderr_val = kernel_new_process_some_int(stderr_read_end);
  } else {
    stderr_val = Val_int(0);
  }
  Store_field(tuple, 3, stderr_val);

  result = kernel_new_result_ok(tuple);
  CAMLreturn(result);
}

CAMLprim value kernel_new_process_try_wait(value pid_val) {
  CAMLparam1(pid_val);

  int status = 0;
  pid_t result;
  caml_enter_blocking_section();
  result = waitpid(Int_val(pid_val), &status, WNOHANG | WUNTRACED);
  caml_leave_blocking_section();

  if (result == 0) {
    CAMLreturn(kernel_new_result_ok(Val_int(0)));
  }

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (WIFEXITED(status)) {
    CAMLreturn(kernel_new_result_ok(
      kernel_new_process_some_status(KERNEL_NEW_PROCESS_STATUS_EXITED, WEXITSTATUS(status))));
  }

  if (WIFSIGNALED(status)) {
    CAMLreturn(kernel_new_result_ok(
      kernel_new_process_some_status(KERNEL_NEW_PROCESS_STATUS_SIGNALED, WTERMSIG(status))));
  }

  if (WIFSTOPPED(status)) {
    CAMLreturn(kernel_new_result_ok(
      kernel_new_process_some_status(KERNEL_NEW_PROCESS_STATUS_STOPPED, WSTOPSIG(status))));
  }

  CAMLreturn(kernel_new_result_ok(Val_int(0)));
}

CAMLprim value kernel_new_process_wait(value pid_val) {
  CAMLparam1(pid_val);
  CAMLlocal1(tuple);

  int status = 0;
  pid_t result;
  caml_enter_blocking_section();
  result = waitpid(Int_val(pid_val), &status, WUNTRACED);
  caml_leave_blocking_section();

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  tuple = caml_alloc_tuple(2);

  if (WIFEXITED(status)) {
    Store_field(tuple, 0, Val_int(KERNEL_NEW_PROCESS_STATUS_EXITED));
    Store_field(tuple, 1, Val_int(WEXITSTATUS(status)));
    CAMLreturn(kernel_new_result_ok(tuple));
  }

  if (WIFSIGNALED(status)) {
    Store_field(tuple, 0, Val_int(KERNEL_NEW_PROCESS_STATUS_SIGNALED));
    Store_field(tuple, 1, Val_int(WTERMSIG(status)));
    CAMLreturn(kernel_new_result_ok(tuple));
  }

  if (WIFSTOPPED(status)) {
    Store_field(tuple, 0, Val_int(KERNEL_NEW_PROCESS_STATUS_STOPPED));
    Store_field(tuple, 1, Val_int(WSTOPSIG(status)));
    CAMLreturn(kernel_new_result_ok(tuple));
  }

  errno = EINVAL;
  CAMLreturn(kernel_new_result_errno());
}

CAMLprim value kernel_new_process_kill(value pid_val, value signal_val) {
  CAMLparam2(pid_val, signal_val);

  if (kill(Int_val(pid_val), Int_val(signal_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_process_current_pid(value unit_val) {
  CAMLparam1(unit_val);
  CAMLreturn(Val_int(getpid()));
}
