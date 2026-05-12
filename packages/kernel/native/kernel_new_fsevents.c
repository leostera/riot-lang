#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include "kernel_new_errors.h"

#if defined(__APPLE__) || defined(__MACH__)
#include <CoreServices/CoreServices.h>
#include <dispatch/dispatch.h>

typedef struct {
  int read_fd;
  int write_fd;
  int closed;
  int watching;
  int next_watch_id;
  char *pending;
  size_t pending_len;
  size_t pending_cap;
  FSEventStreamRef stream;
  dispatch_queue_t queue;
} kernel_new_fs_events_state_t;

typedef struct {
  kernel_new_fs_events_state_t *state;
} kernel_new_fs_events_t;

typedef struct {
  uint32_t path_len;
  uint32_t flags;
  uint64_t event_id;
} kernel_new_fs_event_header_t;

static kernel_new_fs_events_t *kernel_new_fs_events_data(value watcher_val) {
  return (kernel_new_fs_events_t *)Data_custom_val(watcher_val);
}

static kernel_new_fs_events_state_t *kernel_new_fs_events_state(value watcher_val) {
  return kernel_new_fs_events_data(watcher_val)->state;
}

static void kernel_new_fs_events_stop_stream(kernel_new_fs_events_state_t *watcher) {
  if (watcher->stream != NULL) {
    FSEventStreamStop(watcher->stream);
    FSEventStreamInvalidate(watcher->stream);
    FSEventStreamRelease(watcher->stream);
    watcher->stream = NULL;
  }
  if (watcher->queue != NULL) {
    dispatch_release(watcher->queue);
    watcher->queue = NULL;
  }
  watcher->watching = 0;
}

static void kernel_new_fs_events_close(kernel_new_fs_events_state_t *watcher) {
  if (watcher->closed) {
    return;
  }

  kernel_new_fs_events_stop_stream(watcher);

  if (watcher->read_fd >= 0) {
    close(watcher->read_fd);
    watcher->read_fd = -1;
  }

  if (watcher->write_fd >= 0) {
    close(watcher->write_fd);
    watcher->write_fd = -1;
  }

  if (watcher->pending != NULL) {
    free(watcher->pending);
    watcher->pending = NULL;
  }

  watcher->pending_len = 0;
  watcher->pending_cap = 0;
  watcher->closed = 1;
}

static void kernel_new_fs_events_finalize(value watcher_val) {
  kernel_new_fs_events_t *handle = kernel_new_fs_events_data(watcher_val);
  if (handle->state != NULL) {
    kernel_new_fs_events_close(handle->state);
    free(handle->state);
    handle->state = NULL;
  }
}

static struct custom_operations kernel_new_fs_events_ops = {
  "riot.kernel.fs.events",
  kernel_new_fs_events_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static int kernel_new_fs_events_configure_fd(int fd) {
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

static int kernel_new_fs_events_ensure_capacity(kernel_new_fs_events_state_t *watcher, size_t required) {
  if (required <= watcher->pending_cap) {
    return 0;
  }

  size_t new_capacity = watcher->pending_cap == 0 ? 4096 : watcher->pending_cap;
  while (new_capacity < required) {
    new_capacity *= 2;
  }

  char *new_pending = watcher->pending == NULL
    ? malloc(new_capacity)
    : realloc(watcher->pending, new_capacity);

  if (new_pending == NULL) {
    return -1;
  }

  watcher->pending = new_pending;
  watcher->pending_cap = new_capacity;
  return 0;
}

static void kernel_new_fs_events_callback(
  ConstFSEventStreamRef stream,
  void *client_info,
  size_t num_events,
  void *event_paths,
  const FSEventStreamEventFlags event_flags[],
  const FSEventStreamEventId event_ids[]
) {
  kernel_new_fs_events_state_t *watcher = (kernel_new_fs_events_state_t *)client_info;
  char **paths = (char **)event_paths;

  (void)stream;

  for (size_t index = 0; index < num_events; index++) {
    kernel_new_fs_event_header_t header;
    ssize_t written = 0;

    if (watcher->closed || watcher->write_fd < 0) {
      return;
    }

    header.path_len = (uint32_t)strlen(paths[index]);
    header.flags = (uint32_t)event_flags[index];
    header.event_id = (uint64_t)event_ids[index];

    written = write(watcher->write_fd, &header, sizeof(header));
    if (written == -1) {
      return;
    }

    written = write(watcher->write_fd, paths[index], header.path_len);
    if (written == -1) {
      return;
    }
  }
}

static int kernel_new_fs_events_read_available(kernel_new_fs_events_state_t *watcher) {
  char buffer[4096];

  while (1) {
    ssize_t read_count = read(watcher->read_fd, buffer, sizeof(buffer));
    if (read_count > 0) {
      if (kernel_new_fs_events_ensure_capacity(watcher, watcher->pending_len + (size_t)read_count) == -1) {
        caml_raise_out_of_memory();
      }

      memcpy(watcher->pending + watcher->pending_len, buffer, (size_t)read_count);
      watcher->pending_len += (size_t)read_count;
      continue;
    }

    if (read_count == 0) {
      return KERNEL_NEW_ERR_END_OF_FILE;
    }

    if (errno == EAGAIN
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
        || errno == EWOULDBLOCK
#endif
    ) {
      return 0;
    }

    return kernel_new_error_of_errno(errno);
  }
}

CAMLprim value kernel_new_fs_events_create(value unit_val) {
  CAMLparam1(unit_val);
  CAMLlocal2(watcher_val, result);

  int pipe_fds[2];
  kernel_new_fs_events_t *handle = NULL;
  kernel_new_fs_events_state_t *watcher = NULL;

  signal(SIGPIPE, SIG_IGN);

  watcher_val = caml_alloc_custom(&kernel_new_fs_events_ops, sizeof(kernel_new_fs_events_t), 0, 1);
  handle = kernel_new_fs_events_data(watcher_val);
  handle->state = NULL;

  watcher = malloc(sizeof(kernel_new_fs_events_state_t));
  if (watcher == NULL) {
    caml_raise_out_of_memory();
  }

  if (pipe(pipe_fds) == -1) {
    free(watcher);
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_fs_events_configure_fd(pipe_fds[0]) == -1) {
    int saved_errno = errno;
    close(pipe_fds[0]);
    close(pipe_fds[1]);
    free(watcher);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_fs_events_configure_fd(pipe_fds[1]) == -1) {
    int saved_errno = errno;
    close(pipe_fds[0]);
    close(pipe_fds[1]);
    free(watcher);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  watcher->read_fd = pipe_fds[0];
  watcher->write_fd = pipe_fds[1];
  watcher->closed = 0;
  watcher->watching = 0;
  watcher->next_watch_id = 1;
  watcher->pending = NULL;
  watcher->pending_len = 0;
  watcher->pending_cap = 0;
  watcher->stream = NULL;
  watcher->queue = NULL;
  handle->state = watcher;

  result = kernel_new_result_ok(watcher_val);
  CAMLreturn(result);
}

CAMLprim value kernel_new_fs_events_watch(value watcher_val, value path_val, value latency_val) {
  CAMLparam3(watcher_val, path_val, latency_val);

  kernel_new_fs_events_state_t *watcher = kernel_new_fs_events_state(watcher_val);
  const char *path = String_val(path_val);
  double latency = Double_val(latency_val);
  CFStringRef path_str = NULL;
  CFArrayRef paths = NULL;
  FSEventStreamContext stream_ctx = {0, watcher, NULL, NULL, NULL};

  if (watcher == NULL) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR));
  }

  if (watcher->closed) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR));
  }

  if (watcher->watching) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_INVALID_ARGUMENT));
  }

  path_str = CFStringCreateWithCString(NULL, path, kCFStringEncodingUTF8);
  if (path_str == NULL) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_INVALID_ARGUMENT));
  }

  paths = CFArrayCreate(NULL, (const void **)&path_str, 1, &kCFTypeArrayCallBacks);
  CFRelease(path_str);
  if (paths == NULL) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_INVALID_ARGUMENT));
  }

  watcher->stream = FSEventStreamCreate(
    NULL,
    &kernel_new_fs_events_callback,
    &stream_ctx,
    paths,
    kFSEventStreamEventIdSinceNow,
    latency,
    kFSEventStreamCreateFlagFileEvents | kFSEventStreamCreateFlagNoDefer);
  CFRelease(paths);

  if (watcher->stream == NULL) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_INPUT_OUTPUT));
  }

  watcher->queue = dispatch_queue_create("riot.kernel.fs.events", DISPATCH_QUEUE_SERIAL);
  if (watcher->queue == NULL) {
    kernel_new_fs_events_stop_stream(watcher);
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_INPUT_OUTPUT));
  }

  FSEventStreamSetDispatchQueue(watcher->stream, watcher->queue);
  if (!FSEventStreamStart(watcher->stream)) {
    kernel_new_fs_events_stop_stream(watcher);
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_INPUT_OUTPUT));
  }

  watcher->watching = 1;
  CAMLreturn(kernel_new_result_ok(Val_int(watcher->next_watch_id++)));
}

CAMLprim value kernel_new_fs_events_unwatch(value watcher_val, value watch_id_val) {
  CAMLparam2(watcher_val, watch_id_val);
  kernel_new_fs_events_state_t *watcher = kernel_new_fs_events_state(watcher_val);

  (void)watch_id_val;

  if (watcher == NULL) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR));
  }

  if (watcher->closed) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR));
  }

  if (!watcher->watching) {
    CAMLreturn(kernel_new_result_ok(Val_unit));
  }

  kernel_new_fs_events_stop_stream(watcher);
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_events_poll(value watcher_val) {
  CAMLparam1(watcher_val);
  CAMLlocal4(array_val, event_val, path_val, event_id_val);

  kernel_new_fs_events_state_t *watcher = kernel_new_fs_events_state(watcher_val);
  size_t offset = 0;
  size_t event_count = 0;
  int read_result = 0;

  if (watcher == NULL) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR));
  }

  if (watcher->closed) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR));
  }

  read_result = kernel_new_fs_events_read_available(watcher);
  if (read_result != 0 && read_result != KERNEL_NEW_ERR_END_OF_FILE) {
    CAMLreturn(kernel_new_result_error(read_result));
  }

  while ((offset + sizeof(kernel_new_fs_event_header_t)) <= watcher->pending_len) {
    kernel_new_fs_event_header_t header;
    memcpy(&header, watcher->pending + offset, sizeof(header));
    if ((offset + sizeof(header) + header.path_len) > watcher->pending_len) {
      break;
    }
    event_count += 1;
    offset += sizeof(header) + header.path_len;
  }

  if (event_count == 0) {
    if (watcher->pending_len != 0) {
      CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_WOULD_BLOCK));
    }
    if (read_result == KERNEL_NEW_ERR_END_OF_FILE) {
      CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_END_OF_FILE));
    }
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_WOULD_BLOCK));
  }

  array_val = caml_alloc((mlsize_t)event_count, 0);
  offset = 0;

  for (size_t index = 0; index < event_count; index++) {
    kernel_new_fs_event_header_t header;
    memcpy(&header, watcher->pending + offset, sizeof(header));
    path_val = caml_alloc_initialized_string((mlsize_t)header.path_len, watcher->pending + offset + sizeof(header));
    event_id_val = caml_copy_int64((int64_t)header.event_id);
    event_val = caml_alloc_tuple(3);
    Store_field(event_val, 0, path_val);
    Store_field(event_val, 1, caml_copy_int32((int32_t)header.flags));
    Store_field(event_val, 2, event_id_val);
    Store_field(array_val, index, event_val);
    offset += sizeof(header) + header.path_len;
  }

  if (offset < watcher->pending_len) {
    memmove(watcher->pending, watcher->pending + offset, watcher->pending_len - offset);
  }
  watcher->pending_len -= offset;

  CAMLreturn(kernel_new_result_ok(array_val));
}

CAMLprim value kernel_new_fs_events_stop(value watcher_val) {
  CAMLparam1(watcher_val);
  kernel_new_fs_events_state_t *watcher = kernel_new_fs_events_state(watcher_val);

  if (watcher == NULL) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR));
  }

  if (watcher->closed) {
    CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR));
  }

  kernel_new_fs_events_close(watcher);
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_fs_events_read_fd(value watcher_val) {
  CAMLparam1(watcher_val);
  kernel_new_fs_events_state_t *watcher = kernel_new_fs_events_state(watcher_val);

  if (watcher == NULL) {
    CAMLreturn(Val_int(-1));
  }

  CAMLreturn(Val_int(watcher->read_fd));
}

#else

CAMLprim value kernel_new_fs_events_create(value unit_val) {
  CAMLparam1(unit_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_fs_events_watch(value watcher_val, value path_val, value latency_val) {
  CAMLparam3(watcher_val, path_val, latency_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_fs_events_unwatch(value watcher_val, value watch_id_val) {
  CAMLparam2(watcher_val, watch_id_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_fs_events_poll(value watcher_val) {
  CAMLparam1(watcher_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_fs_events_stop(value watcher_val) {
  CAMLparam1(watcher_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_fs_events_read_fd(value watcher_val) {
  CAMLparam1(watcher_val);
  caml_invalid_argument("fs events unsupported");
}

#endif
