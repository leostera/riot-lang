#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/event.h>
#include <unistd.h>
#include "./utils.h"


value kqueue_event_to_record(struct kevent *kevent) {
  CAMLparam0();
  CAMLlocal1(event);
  event = caml_alloc_tuple(4);
  Store_field(event, 0, Val_int(kevent->ident));
  Store_field(event, 1, Val_int(kevent->filter));
  Store_field(event, 2, Val_int(kevent->flags));

  value *stored_value = (value *)(intptr_t)kevent->udata;
  Store_field(event, 3, *stored_value);

  CAMLreturn(event);
}

CAMLprim value kernel_unix_kevent(value max_events_val, value timeout_val, value fd_val) {
    // fprintf(stderr, "waiting events\n");
    CAMLparam3(max_events_val, timeout_val, fd_val);
    CAMLlocal1(event_array);

    int max_events = Int_val(max_events_val);
    int64_t timeout_ns = Int64_val(timeout_val);
    int fd = Long_val(fd_val);

    struct kevent *events = malloc(sizeof(struct kevent) * max_events);
    if (events == NULL) {
        caml_failwith("Memory allocation failed");
    }

    int num_events = -1;

    if (timeout_ns < 0) {
      // fprintf(stderr, "waiting events inifinitely\n");
      caml_enter_blocking_section();
      num_events = kevent(fd, NULL, 0, events, max_events, NULL);
      caml_leave_blocking_section();
    } else {
      // fprintf(stderr, "waiting events with timeout\n");
      struct timespec timeout;
      timeout.tv_sec = timeout_ns / 1000000000;
      timeout.tv_nsec = timeout_ns % 1000000000;
      caml_enter_blocking_section();
      num_events = kevent(fd, NULL, 0, events, max_events, &timeout);
      caml_leave_blocking_section();
    }

    if (num_events == -1) {
      // fprintf(stderr, "error %d\n", errno);
        free(events);
        uerror("kevent", Nothing);
    }

    // fprintf(stderr, "creating event\n");
    struct kevent **event_ptrs = malloc((num_events + 1) * sizeof(struct kevent *));
    for (int i = 0; i < num_events; i++) {
        event_ptrs[i] = &events[i];
    }
    event_ptrs[num_events] = NULL;
    event_array = caml_alloc_array((ocaml_alloc_first_arg_fn)kqueue_event_to_record, (ocaml_alloc_second_arg)event_ptrs);

    free(events);
    CAMLreturn(event_array);
}

CAMLprim value kernel_unix_fcntl(value fd, value cmd, value arg) {
    CAMLparam3(fd, cmd, arg);

    int c_fd = Int_val(fd);
    int c_cmd = Int_val(cmd);
    int c_arg = Int_val(arg);
    int result = fcntl(c_fd, c_cmd, c_arg);

    if (result == -1) uerror("fcntl", Nothing);

    CAMLreturn(Val_int(result));
}

CAMLprim value kernel_unix_kqueue(value unit) {
    CAMLparam1(unit);

    int fd = kqueue();
    if (fd == -1) uerror("kqueue", Nothing);

    CAMLreturn(Val_int(fd));
}

CAMLprim value kernel_unix_kevent_register(value fd_val, value events_val, value ignored_errors_val) {
    CAMLparam3(fd_val, events_val, ignored_errors_val);
    int fd = Int_val(fd_val);
    int num_events = Wosize_val(events_val);
    int num_ignored_errors = Wosize_val(ignored_errors_val);

    struct kevent *changes = (struct kevent *)malloc(sizeof(struct kevent) * num_events);
    if (changes == NULL) {
        caml_failwith("Memory allocation failed");
    }

    // Access events directly from OCaml array
    for (int i = 0; i < num_events; i++) {
        value field = Field(events_val, i);
        struct kevent *kevent = malloc(sizeof(struct kevent));
        if (kevent == NULL) {
            caml_failwith("Memory allocation failed");
        }
        int fd = Int_val(Field(field, 0));
        int filter = Int_val(Field(field, 1));
        int flags = Int_val(Field(field, 2)); 

        value* token = malloc (sizeof (value*));
        *token = Field(field, 3);
        caml_register_generational_global_root(token);

        // fprintf(stderr, "Record %d: ident=%lu, filter=%d, flags=%u, udata=%p\n", i, fd, filter, flags, token);
        EV_SET(&changes[i], fd, filter, flags, 0, 0, (void *)(int64_t)(token));
        // fprintf(stderr, "Event %d: ident=%lu, filter=%d, flags=%u, fflags=%u, data=%ld, udata=%p\n",
        //     i,
        //     changes[i].ident,
        //     changes[i].filter,
        //     changes[i].flags,
        //     changes[i].fflags,
        //     changes[i].data,
        //     changes[i].udata);
    }
    // fprintf(stderr, "events are ok\n");

    caml_enter_blocking_section();
    int result = kevent(fd, changes, num_events, NULL, 0, NULL);
    caml_leave_blocking_section();
    free(changes);

    if (result == -1) {
        // fprintf(stderr, "errno=%d - %s\n", errno, strerror(errno));
        if (errno == EINTR) {
            // According to the manual page of FreeBSD: "When kevent() call fails
            // with EINTR error, all changes in the changelist have been applied",
            // so we can safely ignore it.
            CAMLreturn(Val_unit);
        }
        // fprintf(stderr, "debug: error %d count %d", errno, num_ignored_errors);
        for (int i = 0; i < num_ignored_errors; i++) {
            if (Int_val(Field(ignored_errors_val, i)) == errno) {
                // Ignore this specific error
                CAMLreturn(Val_unit);
            }
        }
        // fprintf(stderr, "debug: error %d", errno);
        uerror("kevent_register", Nothing);
    }

    // fprintf(stderr, "all good\n");

    CAMLreturn(Val_unit);
}

#else
/* Linux - use epoll instead of kqueue */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/epoll.h>
#include <unistd.h>
#include <stdlib.h>
#include "./utils.h"

/* Wrapper structure to store both fd and token since epoll_data is a union */
typedef struct {
    int fd;
    value *token;
} epoll_user_data_t;

/* Convert epoll event to OCaml record matching kqueue event structure */
value epoll_event_to_record(struct epoll_event *ev) {
  CAMLparam0();
  CAMLlocal1(event);
  event = caml_alloc_tuple(4);
  
  /* Extract fd and token from wrapper structure */
  epoll_user_data_t *user_data = (epoll_user_data_t *)ev->data.ptr;
  
  /* Map epoll events to kqueue-like structure:
     field 0: fd (ident)
     field 1: filter (EVFILT_READ=1/EVFILT_WRITE=2)
     field 2: flags (EV_ADD=1/EV_DELETE=2/EV_ENABLE=4/EV_DISABLE=8)
     field 3: token (udata)
  */
  Store_field(event, 0, Val_int(user_data->fd));
  
  /* Map epoll events to kqueue filters */
  int filter = 0;
  if (ev->events & EPOLLIN) filter = -1;  /* EVFILT_READ */
  if (ev->events & EPOLLOUT) filter = -2; /* EVFILT_WRITE */
  Store_field(event, 1, Val_int(filter));
  
  /* EV_ADD flag */
  Store_field(event, 2, Val_int(1));
  
  /* Token from wrapper */
  Store_field(event, 3, user_data->token ? *(user_data->token) : Val_int(0));

  CAMLreturn(event);
}

CAMLprim value kernel_unix_kevent(value max_events_val, value timeout_val, value fd_val) {
    CAMLparam3(max_events_val, timeout_val, fd_val);
    CAMLlocal1(event_array);

    int max_events = Int_val(max_events_val);
    int64_t timeout_ns = Int64_val(timeout_val);
    int epfd = Long_val(fd_val);

    struct epoll_event *events = malloc(sizeof(struct epoll_event) * max_events);
    if (events == NULL) {
        caml_failwith("Memory allocation failed");
    }

    int num_events = -1;
    int timeout_ms;

    if (timeout_ns < 0) {
        timeout_ms = -1; /* Wait indefinitely */
    } else {
        timeout_ms = (int)(timeout_ns / 1000000); /* Convert nanoseconds to milliseconds */
    }

    caml_enter_blocking_section();
    num_events = epoll_wait(epfd, events, max_events, timeout_ms);
    caml_leave_blocking_section();

    if (num_events == -1) {
        free(events);
        uerror("epoll_wait", Nothing);
    }

    event_array = caml_alloc(num_events, 0);
    for (int i = 0; i < num_events; i++) {
        Store_field(event_array, i, epoll_event_to_record(&events[i]));
    }

    free(events);
    CAMLreturn(event_array);
}

CAMLprim value kernel_unix_kqueue(value unit) {
    CAMLparam1(unit);
    
    int epfd = epoll_create1(EPOLL_CLOEXEC);
    if (epfd == -1) {
        uerror("epoll_create1", Nothing);
    }
    
    CAMLreturn(Val_long(epfd));
}

CAMLprim value kernel_unix_kevent_register(value fd_val, value events_val, value ignored_errors_val) {
    CAMLparam3(fd_val, events_val, ignored_errors_val);
    
    int epfd = Long_val(fd_val);
    int num_events = Wosize_val(events_val);
    int num_ignored_errors = Wosize_val(ignored_errors_val);

    for (int i = 0; i < num_events; i++) {
        value field = Field(events_val, i);
        
        /* Extract fields from OCaml record */
        int fd = Int_val(Field(field, 0));
        int filter = Int_val(Field(field, 1));
        int flags = Int_val(Field(field, 2));
        
        /* Allocate wrapper to store both fd and token (since epoll_data is a union) */
        epoll_user_data_t *user_data = malloc(sizeof(epoll_user_data_t));
        if (user_data == NULL) {
            caml_failwith("Memory allocation failed");
        }
        
        user_data->fd = fd;
        user_data->token = malloc(sizeof(value*));
        *(user_data->token) = Field(field, 3);
        caml_register_generational_global_root(user_data->token);
        
        struct epoll_event ev;
        ev.events = 0;
        ev.data.ptr = user_data;
        
        /* Map kqueue filters to epoll events */
        if (filter == -1) ev.events |= EPOLLIN;   /* EVFILT_READ */
        if (filter == -2) ev.events |= EPOLLOUT;  /* EVFILT_WRITE */
        
        /* Map kqueue flags to epoll operations */
        int op;
        if (flags & 0x0001) {  /* EV_ADD */
            op = EPOLL_CTL_ADD;
        } else if (flags & 0x0002) {  /* EV_DELETE */
            op = EPOLL_CTL_DEL;
        } else if (flags & 0x0008) {  /* EV_DISABLE */
            op = EPOLL_CTL_DEL;
        } else {
            op = EPOLL_CTL_MOD;
        }
        
        int result = epoll_ctl(epfd, op, fd, &ev);
        if (result == -1) {
            /* Check if this error should be ignored */
            int should_ignore = 0;
            for (int j = 0; j < num_ignored_errors; j++) {
                if (Int_val(Field(ignored_errors_val, j)) == errno) {
                    should_ignore = 1;
                    break;
                }
            }
            
            if (!should_ignore && errno != EEXIST) {
                /* EEXIST is common when re-adding, try MOD instead */
                if (op == EPOLL_CTL_ADD) {
                    result = epoll_ctl(epfd, EPOLL_CTL_MOD, fd, &ev);
                    if (result == -1) {
                        uerror("epoll_ctl", Nothing);
                    }
                } else {
                    uerror("epoll_ctl", Nothing);
                }
            }
        }
    }

    CAMLreturn(Val_unit);
}

CAMLprim value kernel_unix_fcntl(value fd_val, value cmd_val, value arg_val) {
    CAMLparam3(fd_val, cmd_val, arg_val);
    
    int fd = Long_val(fd_val);
    int cmd = Int_val(cmd_val);
    int arg = Int_val(arg_val);
    
    int result = fcntl(fd, cmd, arg);
    if (result == -1) {
        uerror("fcntl", Nothing);
    }
    
    CAMLreturn(Val_int(result));
}

#endif
