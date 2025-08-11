#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/custom.h>
#include <caml/threads.h>
#include <caml/bigarray.h>

#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdio.h>

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#include <sys/event.h>

/* Type definitions for OCaml allocation functions */
typedef value (*ocaml_alloc_first_arg_fn) (char const *);
typedef char const * const * (ocaml_alloc_second_arg);

/* Create a new kqueue */
CAMLprim value gluon_kqueue(value unit) {
    CAMLparam1(unit);
    int kq = kqueue();
    if (kq == -1) {
        uerror("kqueue", Nothing);
    }
    CAMLreturn(Val_int(kq));
}

/* Convert C struct kevent to OCaml kevent record */
static value kevent_to_value(struct kevent *ke) {
    CAMLparam0();
    CAMLlocal1(result);
    
    result = caml_alloc(6, 0);
    Store_field(result, 0, Val_int(ke->ident));
    Store_field(result, 1, Val_int(ke->filter));
    Store_field(result, 2, Val_int(ke->flags));
    Store_field(result, 3, Val_int(ke->fflags));
    Store_field(result, 4, Val_int(ke->data));
    
    /* The udata field contains a pointer to the stored OCaml value */
    if (ke->udata != NULL) {
        value *stored_value = (value *)(intptr_t)ke->udata;
        Store_field(result, 5, *stored_value);
        caml_remove_generational_global_root(stored_value);
        free(stored_value);
    } else {
        Store_field(result, 5, Val_int(0));
    }
    
    CAMLreturn(result);
}

/* Main kevent syscall wrapper */
CAMLprim value gluon_kevent(value v_kq, value v_changelist, value v_nchanges, 
                           value v_eventlist, value v_nevents, value v_timeout_ns) {
    CAMLparam5(v_kq, v_changelist, v_nchanges, v_eventlist, v_nevents);
    CAMLxparam1(v_timeout_ns);
    
    int kq = Int_val(v_kq);
    int nchanges = Int_val(v_nchanges);
    int nevents = Int_val(v_nevents);
    int64_t timeout_ns = Int64_val(v_timeout_ns);
    
    struct kevent *changelist = NULL;
    struct kevent *eventlist = NULL;
    struct timespec ts, *timeout = NULL;
    int result;
    
    /* Convert changelist from OCaml array to C array */
    if (nchanges > 0) {
        changelist = malloc(sizeof(struct kevent) * nchanges);
        if (!changelist) {
            caml_raise_out_of_memory();
        }
        
        for (int i = 0; i < nchanges; i++) {
            value v_change = Field(v_changelist, i);
            int ident = Int_val(Field(v_change, 0));
            int filter = Int_val(Field(v_change, 1));
            int flags = Int_val(Field(v_change, 2));
            int fflags = Int_val(Field(v_change, 3));
            int data = Int_val(Field(v_change, 4));
            
            /* Store OCaml value as udata */
            value token = Field(v_change, 5);
            void *udata = NULL;
            
            /* Only store token for non-delete operations */
            if ((flags & 0x0002) == 0) {
                value *stored_token = malloc(sizeof(value));
                if (!stored_token) {
                    free(changelist);
                    caml_raise_out_of_memory();
                }
                *stored_token = token;
                caml_register_generational_global_root(stored_token);
                udata = stored_token;
            }
            
            EV_SET(&changelist[i], ident, filter, flags, fflags, data, udata);
        }
    }
    
    /* Allocate eventlist if needed */
    if (nevents > 0) {
        eventlist = malloc(sizeof(struct kevent) * nevents);
        if (!eventlist) {
            if (changelist) free(changelist);
            caml_raise_out_of_memory();
        }
    }
    
    /* Setup timeout */
    if (timeout_ns >= 0) {
        ts.tv_sec = timeout_ns / 1000000000LL;
        ts.tv_nsec = timeout_ns % 1000000000LL;
        timeout = &ts;
    }
    
    /* Call kevent, potentially blocking */
    caml_release_runtime_system();
    result = kevent(kq, changelist, nchanges, eventlist, nevents, timeout);
    caml_acquire_runtime_system();
    
    /* Clean up changelist (udata already cleaned up if events are returned) */
    if (changelist) free(changelist);
    
    /* Check for errors */
    if (result == -1) {
        int saved_errno = errno;
        if (eventlist) free(eventlist);
        unix_error(saved_errno, "kevent", Nothing);
    }
    
    /* Convert results back to OCaml array */
    if (result > 0 && eventlist) {
        for (int i = 0; i < result; i++) {
            Store_field(v_eventlist, i, kevent_to_value(&eventlist[i]));
        }
    }
    
    /* Cleanup */
    if (eventlist) free(eventlist);
    
    CAMLreturn(Val_int(result));
}

/* Set file descriptor to non-blocking mode */
CAMLprim value gluon_set_nonblocking(value v_fd) {
    CAMLparam1(v_fd);
    int fd = Int_val(v_fd);
    int flags = fcntl(fd, F_GETFL, 0);
    
    if (flags == -1) {
        uerror("fcntl(F_GETFL)", Nothing);
    }
    
    if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        uerror("fcntl(F_SETFL)", Nothing);
    }
    
    CAMLreturn(Val_unit);
}

/* Read syscall wrapper */
CAMLprim value gluon_read(value v_fd, value v_buf, value v_ofs, value v_len) {
    CAMLparam4(v_fd, v_buf, v_ofs, v_len);
    int fd = Int_val(v_fd);
    int ofs = Int_val(v_ofs);
    int len = Int_val(v_len);
    ssize_t result;
    
    caml_release_runtime_system();
    result = read(fd, &Byte(v_buf, ofs), len);
    caml_acquire_runtime_system();
    
    if (result == -1) {
        uerror("read", Nothing);
    }
    
    CAMLreturn(Val_int(result));
}

/* Write syscall wrapper */
CAMLprim value gluon_write(value v_fd, value v_buf, value v_ofs, value v_len) {
    CAMLparam4(v_fd, v_buf, v_ofs, v_len);
    int fd = Int_val(v_fd);
    int ofs = Int_val(v_ofs);
    int len = Int_val(v_len);
    ssize_t result;
    
    caml_release_runtime_system();
    result = write(fd, &Byte(v_buf, ofs), len);
    caml_acquire_runtime_system();
    
    if (result == -1) {
        uerror("write", Nothing);
    }
    
    CAMLreturn(Val_int(result));
}

/* Fill iov with pointers to the OCaml buffers */
static void fill_iov(struct iovec *iov, value v_bufs) {
    int n_bufs = Wosize_val(v_bufs);
    for (int i = 0; i < n_bufs; i++) {
        value v_iov = Field(v_bufs, i);
        value v_buf = Field(v_iov, 0);
        value v_off = Field(v_iov, 1);
        value v_len = Field(v_iov, 2);
        iov[i].iov_base = Bytes_val(v_buf) + Long_val(v_off);
        iov[i].iov_len = Long_val(v_len);
    }
}

/* Readv syscall wrapper */
CAMLprim value gluon_readv(value v_fd, value v_iovecs) {
    CAMLparam2(v_fd, v_iovecs);
    int fd = Int_val(v_fd);
    int nvecs = Wosize_val(v_iovecs);
    struct iovec iov[nvecs];
    ssize_t result;
    
    fill_iov(iov, v_iovecs);
    
    caml_release_runtime_system();
    result = readv(fd, iov, nvecs);
    caml_acquire_runtime_system();
    
    if (result == -1) {
        uerror("readv", Nothing);
    }
    
    CAMLreturn(Val_int(result));
}

/* Writev syscall wrapper */
CAMLprim value gluon_writev(value v_fd, value v_iovecs) {
    CAMLparam2(v_fd, v_iovecs);
    int fd = Int_val(v_fd);
    int nvecs = Wosize_val(v_iovecs);
    struct iovec iov[nvecs];
    ssize_t result;
    
    fill_iov(iov, v_iovecs);
    
    caml_release_runtime_system();
    result = writev(fd, iov, nvecs);
    caml_acquire_runtime_system();
    
    if (result == -1) {
        uerror("writev", Nothing);
    }
    
    CAMLreturn(Val_int(result));
}

/* Connect syscall wrapper - simple version without get_sockaddr */
CAMLprim value gluon_connect(value v_fd, value v_addr) {
    CAMLparam2(v_fd, v_addr);
    
    /* For now, we'll return an error - the OCaml side should handle socket creation */
    unix_error(ENOTSUP, "gluon_connect", Nothing);
    
    CAMLreturn(Val_int(0));
}

/* Accept syscall wrapper - simple version without alloc_sockaddr */
CAMLprim value gluon_accept(value v_fd) {
    CAMLparam1(v_fd);
    
    /* For now, we'll return an error - the OCaml side should handle accept */
    unix_error(ENOTSUP, "gluon_accept", Nothing);
    
    CAMLreturn(Val_unit);
}

/* Sendfile syscall wrapper (macOS specific) */
CAMLprim value gluon_sendfile(value v_out_fd, value v_in_fd, value v_offset, value v_count) {
    CAMLparam4(v_out_fd, v_in_fd, v_offset, v_count);
    int out_fd = Int_val(v_out_fd);
    int in_fd = Int_val(v_in_fd);
    off_t offset = Int_val(v_offset);
    off_t len = Int_val(v_count);
    int result;
    
#ifdef __APPLE__
    /* macOS sendfile */
    caml_release_runtime_system();
    result = sendfile(in_fd, out_fd, offset, &len, NULL, 0);
    caml_acquire_runtime_system();
    
    if (result == -1 && errno != EAGAIN && errno != EWOULDBLOCK) {
        uerror("sendfile", Nothing);
    }
    
    /* On macOS, len is updated with bytes sent */
    CAMLreturn(Val_int(len));
#else
    /* Not supported on this platform */
    unix_error(ENOSYS, "sendfile", Nothing);
#endif
}

#else /* Not BSD/macOS - provide stub implementations */

/* Stub implementations for non-kqueue platforms */
CAMLprim value gluon_kqueue(value unit) {
    unix_error(ENOSYS, "kqueue", Nothing);
}

CAMLprim value gluon_kevent(value v_kq, value v_changelist, value v_nchanges, 
                           value v_eventlist, value v_nevents, value v_timeout_ns) {
    unix_error(ENOSYS, "kevent", Nothing);
}

CAMLprim value gluon_set_nonblocking(value v_fd) {
    CAMLparam1(v_fd);
    int fd = Int_val(v_fd);
    int flags = fcntl(fd, F_GETFL, 0);
    
    if (flags == -1) {
        uerror("fcntl(F_GETFL)", Nothing);
    }
    
    if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        uerror("fcntl(F_SETFL)", Nothing);
    }
    
    CAMLreturn(Val_unit);
}

CAMLprim value gluon_read(value v_fd, value v_buf, value v_ofs, value v_len) {
    CAMLparam4(v_fd, v_buf, v_ofs, v_len);
    int fd = Int_val(v_fd);
    int ofs = Int_val(v_ofs);
    int len = Int_val(v_len);
    ssize_t result;
    
    caml_release_runtime_system();
    result = read(fd, &Byte(v_buf, ofs), len);
    caml_acquire_runtime_system();
    
    if (result == -1) {
        uerror("read", Nothing);
    }
    
    CAMLreturn(Val_int(result));
}

CAMLprim value gluon_write(value v_fd, value v_buf, value v_ofs, value v_len) {
    CAMLparam4(v_fd, v_buf, v_ofs, v_len);
    int fd = Int_val(v_fd);
    int ofs = Int_val(v_ofs);
    int len = Int_val(v_len);
    ssize_t result;
    
    caml_release_runtime_system();
    result = write(fd, &Byte(v_buf, ofs), len);
    caml_acquire_runtime_system();
    
    if (result == -1) {
        uerror("write", Nothing);
    }
    
    CAMLreturn(Val_int(result));
}

static void fill_iov(struct iovec *iov, value v_bufs) {
    int n_bufs = Wosize_val(v_bufs);
    for (int i = 0; i < n_bufs; i++) {
        value v_iov = Field(v_bufs, i);
        value v_buf = Field(v_iov, 0);
        value v_off = Field(v_iov, 1);
        value v_len = Field(v_iov, 2);
        iov[i].iov_base = Bytes_val(v_buf) + Long_val(v_off);
        iov[i].iov_len = Long_val(v_len);
    }
}

CAMLprim value gluon_readv(value v_fd, value v_iovecs) {
    CAMLparam2(v_fd, v_iovecs);
    int fd = Int_val(v_fd);
    int nvecs = Wosize_val(v_iovecs);
    struct iovec iov[nvecs];
    ssize_t result;
    
    fill_iov(iov, v_iovecs);
    
    caml_release_runtime_system();
    result = readv(fd, iov, nvecs);
    caml_acquire_runtime_system();
    
    if (result == -1) {
        uerror("readv", Nothing);
    }
    
    CAMLreturn(Val_int(result));
}

CAMLprim value gluon_writev(value v_fd, value v_iovecs) {
    CAMLparam2(v_fd, v_iovecs);
    int fd = Int_val(v_fd);
    int nvecs = Wosize_val(v_iovecs);
    struct iovec iov[nvecs];
    ssize_t result;
    
    fill_iov(iov, v_iovecs);
    
    caml_release_runtime_system();
    result = writev(fd, iov, nvecs);
    caml_acquire_runtime_system();
    
    if (result == -1) {
        uerror("writev", Nothing);
    }
    
    CAMLreturn(Val_int(result));
}

CAMLprim value gluon_connect(value v_fd, value v_addr) {
    unix_error(ENOTSUP, "gluon_connect", Nothing);
}

CAMLprim value gluon_accept(value v_fd) {
    unix_error(ENOTSUP, "gluon_accept", Nothing);
}

CAMLprim value gluon_sendfile(value v_out_fd, value v_in_fd, value v_offset, value v_count) {
    unix_error(ENOSYS, "sendfile", Nothing);
}

#endif /* BSD/macOS check */
