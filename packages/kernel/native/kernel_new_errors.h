#ifndef KERNEL_NEW_ERRORS_H
#define KERNEL_NEW_ERRORS_H

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>

#define KERNEL_NEW_ERR_UNKNOWN_BASE 1024

#define KERNEL_NEW_ERR_END_OF_FILE 1
#define KERNEL_NEW_ERR_PERMISSION_DENIED 2
#define KERNEL_NEW_ERR_NO_SUCH_FILE_OR_DIRECTORY 3
#define KERNEL_NEW_ERR_INTERRUPTED 4
#define KERNEL_NEW_ERR_INPUT_OUTPUT 5
#define KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR 6
#define KERNEL_NEW_ERR_RESOURCE_BUSY 7
#define KERNEL_NEW_ERR_ALREADY_EXISTS 8
#define KERNEL_NEW_ERR_INVALID_ARGUMENT 9
#define KERNEL_NEW_ERR_NO_SPACE_LEFT 10
#define KERNEL_NEW_ERR_BROKEN_PIPE 11
#define KERNEL_NEW_ERR_WOULD_BLOCK 12
#define KERNEL_NEW_ERR_NOT_DIRECTORY 13
#define KERNEL_NEW_ERR_IS_DIRECTORY 14
#define KERNEL_NEW_ERR_NOT_SUPPORTED 15
#define KERNEL_NEW_ERR_ADDRESS_IN_USE 16
#define KERNEL_NEW_ERR_ADDRESS_NOT_AVAILABLE 17
#define KERNEL_NEW_ERR_CONNECTION_REFUSED 18
#define KERNEL_NEW_ERR_CONNECTION_RESET 19
#define KERNEL_NEW_ERR_TIMED_OUT 20
#define KERNEL_NEW_ERR_NETWORK_UNREACHABLE 21
#define KERNEL_NEW_ERR_DESTINATION_ADDRESS_REQUIRED 22
#define KERNEL_NEW_ERR_NOT_CONNECTED 23
#define KERNEL_NEW_ERR_CONNECTION_ABORTED 24
#define KERNEL_NEW_ERR_MESSAGE_TOO_LONG 25
#define KERNEL_NEW_ERR_NO_SUCH_PROCESS 26
#define KERNEL_NEW_ERR_DIRECTORY_NOT_EMPTY 27

static inline int kernel_new_error_of_errno(int error_number) {
  switch (error_number) {
    case EPERM: return KERNEL_NEW_ERR_PERMISSION_DENIED;
    case EACCES: return KERNEL_NEW_ERR_PERMISSION_DENIED;
    case ENOENT: return KERNEL_NEW_ERR_NO_SUCH_FILE_OR_DIRECTORY;
    case EINTR: return KERNEL_NEW_ERR_INTERRUPTED;
    case EIO: return KERNEL_NEW_ERR_INPUT_OUTPUT;
    case EBADF: return KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR;
    case EBUSY: return KERNEL_NEW_ERR_RESOURCE_BUSY;
    case EEXIST: return KERNEL_NEW_ERR_ALREADY_EXISTS;
    case EINVAL: return KERNEL_NEW_ERR_INVALID_ARGUMENT;
    case ENOSPC: return KERNEL_NEW_ERR_NO_SPACE_LEFT;
    case EPIPE: return KERNEL_NEW_ERR_BROKEN_PIPE;
    case EAGAIN: return KERNEL_NEW_ERR_WOULD_BLOCK;
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
    case EWOULDBLOCK: return KERNEL_NEW_ERR_WOULD_BLOCK;
#endif
    case ENOTDIR: return KERNEL_NEW_ERR_NOT_DIRECTORY;
    case EISDIR: return KERNEL_NEW_ERR_IS_DIRECTORY;
#ifdef ENOSYS
    case ENOSYS: return KERNEL_NEW_ERR_NOT_SUPPORTED;
#endif
#ifdef EOPNOTSUPP
    case EOPNOTSUPP: return KERNEL_NEW_ERR_NOT_SUPPORTED;
#endif
#if defined(ENOTSUP) && (!defined(EOPNOTSUPP) || ENOTSUP != EOPNOTSUPP)
    case ENOTSUP: return KERNEL_NEW_ERR_NOT_SUPPORTED;
#endif
    case EADDRINUSE: return KERNEL_NEW_ERR_ADDRESS_IN_USE;
    case EADDRNOTAVAIL: return KERNEL_NEW_ERR_ADDRESS_NOT_AVAILABLE;
    case ECONNREFUSED: return KERNEL_NEW_ERR_CONNECTION_REFUSED;
    case ECONNRESET: return KERNEL_NEW_ERR_CONNECTION_RESET;
    case ETIMEDOUT: return KERNEL_NEW_ERR_TIMED_OUT;
    case ENETUNREACH: return KERNEL_NEW_ERR_NETWORK_UNREACHABLE;
#ifdef EDESTADDRREQ
    case EDESTADDRREQ: return KERNEL_NEW_ERR_DESTINATION_ADDRESS_REQUIRED;
#endif
#ifdef ENOTCONN
    case ENOTCONN: return KERNEL_NEW_ERR_NOT_CONNECTED;
#endif
#ifdef ECONNABORTED
    case ECONNABORTED: return KERNEL_NEW_ERR_CONNECTION_ABORTED;
#endif
#ifdef EMSGSIZE
    case EMSGSIZE: return KERNEL_NEW_ERR_MESSAGE_TOO_LONG;
#endif
#ifdef ESRCH
    case ESRCH: return KERNEL_NEW_ERR_NO_SUCH_PROCESS;
#endif
#ifdef ENOTEMPTY
    case ENOTEMPTY: return KERNEL_NEW_ERR_DIRECTORY_NOT_EMPTY;
#endif
    default: return KERNEL_NEW_ERR_UNKNOWN_BASE + error_number;
  }
}

static inline value kernel_new_result_ok(value payload) {
  CAMLparam1(payload);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

static inline value kernel_new_result_error(int code) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, 1);
  Store_field(result, 0, Val_int(code));
  CAMLreturn(result);
}

static inline value kernel_new_result_errno(void) {
  return kernel_new_result_error(kernel_new_error_of_errno(errno));
}

#endif
