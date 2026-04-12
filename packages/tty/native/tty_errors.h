#ifndef TTY_ERRORS_H
#define TTY_ERRORS_H

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>

#define TTY_ERR_UNKNOWN_BASE 1024

#define TTY_ERR_END_OF_FILE 1
#define TTY_ERR_PERMISSION_DENIED 2
#define TTY_ERR_NO_SUCH_FILE_OR_DIRECTORY 3
#define TTY_ERR_INTERRUPTED 4
#define TTY_ERR_INPUT_OUTPUT 5
#define TTY_ERR_BAD_FILE_DESCRIPTOR 6
#define TTY_ERR_RESOURCE_BUSY 7
#define TTY_ERR_ALREADY_EXISTS 8
#define TTY_ERR_INVALID_ARGUMENT 9
#define TTY_ERR_NO_SPACE_LEFT 10
#define TTY_ERR_BROKEN_PIPE 11
#define TTY_ERR_WOULD_BLOCK 12
#define TTY_ERR_NOT_DIRECTORY 13
#define TTY_ERR_IS_DIRECTORY 14
#define TTY_ERR_NOT_SUPPORTED 15
#define TTY_ERR_ADDRESS_IN_USE 16
#define TTY_ERR_ADDRESS_NOT_AVAILABLE 17
#define TTY_ERR_CONNECTION_REFUSED 18
#define TTY_ERR_CONNECTION_RESET 19
#define TTY_ERR_TIMED_OUT 20
#define TTY_ERR_NETWORK_UNREACHABLE 21
#define TTY_ERR_DESTINATION_ADDRESS_REQUIRED 22
#define TTY_ERR_NOT_CONNECTED 23
#define TTY_ERR_CONNECTION_ABORTED 24
#define TTY_ERR_MESSAGE_TOO_LONG 25
#define TTY_ERR_NO_SUCH_PROCESS 26
#define TTY_ERR_DIRECTORY_NOT_EMPTY 27

static inline int tty_error_of_errno(int error_number) {
  switch (error_number) {
    case EPERM: return TTY_ERR_PERMISSION_DENIED;
    case EACCES: return TTY_ERR_PERMISSION_DENIED;
    case ENOENT: return TTY_ERR_NO_SUCH_FILE_OR_DIRECTORY;
    case EINTR: return TTY_ERR_INTERRUPTED;
    case EIO: return TTY_ERR_INPUT_OUTPUT;
    case EBADF: return TTY_ERR_BAD_FILE_DESCRIPTOR;
    case EBUSY: return TTY_ERR_RESOURCE_BUSY;
    case EEXIST: return TTY_ERR_ALREADY_EXISTS;
    case EINVAL: return TTY_ERR_INVALID_ARGUMENT;
    case ENOSPC: return TTY_ERR_NO_SPACE_LEFT;
    case EPIPE: return TTY_ERR_BROKEN_PIPE;
    case EAGAIN: return TTY_ERR_WOULD_BLOCK;
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
    case EWOULDBLOCK: return TTY_ERR_WOULD_BLOCK;
#endif
    case ENOTDIR: return TTY_ERR_NOT_DIRECTORY;
    case EISDIR: return TTY_ERR_IS_DIRECTORY;
#ifdef ENOSYS
    case ENOSYS: return TTY_ERR_NOT_SUPPORTED;
#endif
#ifdef EOPNOTSUPP
    case EOPNOTSUPP: return TTY_ERR_NOT_SUPPORTED;
#endif
    case EADDRINUSE: return TTY_ERR_ADDRESS_IN_USE;
    case EADDRNOTAVAIL: return TTY_ERR_ADDRESS_NOT_AVAILABLE;
    case ECONNREFUSED: return TTY_ERR_CONNECTION_REFUSED;
    case ECONNRESET: return TTY_ERR_CONNECTION_RESET;
    case ETIMEDOUT: return TTY_ERR_TIMED_OUT;
    case ENETUNREACH: return TTY_ERR_NETWORK_UNREACHABLE;
#ifdef EDESTADDRREQ
    case EDESTADDRREQ: return TTY_ERR_DESTINATION_ADDRESS_REQUIRED;
#endif
#ifdef ENOTCONN
    case ENOTCONN: return TTY_ERR_NOT_CONNECTED;
#endif
#ifdef ECONNABORTED
    case ECONNABORTED: return TTY_ERR_CONNECTION_ABORTED;
#endif
#ifdef EMSGSIZE
    case EMSGSIZE: return TTY_ERR_MESSAGE_TOO_LONG;
#endif
#ifdef ESRCH
    case ESRCH: return TTY_ERR_NO_SUCH_PROCESS;
#endif
#ifdef ENOTEMPTY
    case ENOTEMPTY: return TTY_ERR_DIRECTORY_NOT_EMPTY;
#endif
    default: return TTY_ERR_UNKNOWN_BASE + error_number;
  }
}

static inline value tty_result_ok(value payload) {
  CAMLparam1(payload);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

static inline value tty_result_error(int code) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, 1);
  Store_field(result, 0, Val_int(code));
  CAMLreturn(result);
}

static inline value tty_result_errno(void) {
  return tty_result_error(tty_error_of_errno(errno));
}

#endif
