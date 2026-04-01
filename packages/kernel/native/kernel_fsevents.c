#if defined(__APPLE__) || defined(__MACH__)

#include <CoreServices/CoreServices.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <dispatch/dispatch.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

// Event flags we care about
#define kFSEventStreamEventFlagItemCreated      0x00000100
#define kFSEventStreamEventFlagItemRemoved      0x00000200
#define kFSEventStreamEventFlagItemModified     0x00001000
#define kFSEventStreamEventFlagItemRenamed      0x00000800
#define kFSEventStreamEventFlagItemChangeOwner  0x00004000

typedef struct {
    int fd;  // Write end of pipe for sending events to OCaml
    FSEventStreamRef stream;
    dispatch_queue_t queue;
} fsevents_context_t;

void fsevents_callback(
    ConstFSEventStreamRef stream,
    void *client_info,
    size_t num_events,
    void *event_paths,
    const FSEventStreamEventFlags event_flags[],
    const FSEventStreamEventId event_ids[]
) {
    fsevents_context_t *ctx = (fsevents_context_t *)client_info;
    char **paths = (char **)event_paths;
    
    // Write events to pipe (OCaml reads from other end)
    for (size_t i = 0; i < num_events; i++) {
        // Format: path_len (4 bytes) | flags (4 bytes) | event_id (8 bytes) | path (path_len bytes)
        uint32_t path_len = strlen(paths[i]);
        uint32_t flags = (uint32_t)event_flags[i];
        uint64_t event_id = (uint64_t)event_ids[i];
        
        // Check for write errors (pipe might be closed)
        ssize_t r;
        r = write(ctx->fd, &path_len, sizeof(path_len));
        if (r == -1) return;  // Pipe closed, stop writing
        r = write(ctx->fd, &flags, sizeof(flags));
        if (r == -1) return;
        r = write(ctx->fd, &event_id, sizeof(event_id));
        if (r == -1) return;
        r = write(ctx->fd, paths[i], path_len);
        if (r == -1) return;
    }
}

CAMLprim value kernel_fsevents_create(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);
    
    // Ignore SIGPIPE - we'll handle pipe errors via write() return values
    signal(SIGPIPE, SIG_IGN);
    
    fsevents_context_t *ctx = malloc(sizeof(fsevents_context_t));
    if (!ctx) caml_failwith("Failed to allocate fsevents context");
    
    // Create pipe for event communication
    int pipe_fds[2];
    if (pipe(pipe_fds) == -1) {
        free(ctx);
        uerror("pipe", Nothing);
    }
    
    // Make read end non-blocking
    int flags = fcntl(pipe_fds[0], F_GETFL, 0);
    fcntl(pipe_fds[0], F_SETFL, flags | O_NONBLOCK);
    
    ctx->fd = pipe_fds[1];  // Write end
    ctx->stream = NULL;
    ctx->queue = NULL;
    
    // Return tuple: (context_ptr, read_fd)
    result = caml_alloc_tuple(2);
    Store_field(result, 0, Val_long((long)ctx));
    Store_field(result, 1, Val_int(pipe_fds[0]));
    
    CAMLreturn(result);
}

CAMLprim value kernel_fsevents_watch(value ctx_val, value path_val, value latency_val) {
    CAMLparam3(ctx_val, path_val, latency_val);
    
    fsevents_context_t *ctx = (fsevents_context_t *)Long_val(ctx_val);
    const char *path = String_val(path_val);
    double latency = Double_val(latency_val);
    
    CFStringRef path_str = CFStringCreateWithCString(NULL, path, kCFStringEncodingUTF8);
    CFArrayRef paths = CFArrayCreate(NULL, (const void **)&path_str, 1, NULL);
    
    FSEventStreamContext stream_ctx = {0, ctx, NULL, NULL, NULL};
    ctx->stream = FSEventStreamCreate(
        NULL,
        &fsevents_callback,
        &stream_ctx,
        paths,
        kFSEventStreamEventIdSinceNow,
        latency,
        kFSEventStreamCreateFlagFileEvents | kFSEventStreamCreateFlagNoDefer
    );
    
    CFRelease(paths);
    CFRelease(path_str);
    
    if (!ctx->stream) caml_failwith("Failed to create FSEventStream");

    ctx->queue = dispatch_queue_create("riot.kernel.fsevents", DISPATCH_QUEUE_SERIAL);
    if (!ctx->queue) {
        FSEventStreamRelease(ctx->stream);
        ctx->stream = NULL;
        caml_failwith("Failed to create fsevents dispatch queue");
    }

    FSEventStreamSetDispatchQueue(ctx->stream, ctx->queue);
    FSEventStreamStart(ctx->stream);
    
    CAMLreturn(Val_unit);
}

CAMLprim value kernel_fsevents_stop(value ctx_val) {
    CAMLparam1(ctx_val);
    
    fsevents_context_t *ctx = (fsevents_context_t *)Long_val(ctx_val);
    
    if (ctx->stream) {
        FSEventStreamStop(ctx->stream);
        FSEventStreamInvalidate(ctx->stream);
        FSEventStreamRelease(ctx->stream);
    }

    if (ctx->queue) {
        dispatch_release(ctx->queue);
    }
    
    close(ctx->fd);
    free(ctx);
    
    CAMLreturn(Val_unit);
}

#else
/* Linux/other platforms - provide stub implementations */
/* TODO: Implement using inotify on Linux */

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>

CAMLprim value kernel_fsevents_create(value paths_val, value fd_val) {
    CAMLparam2(paths_val, fd_val);
    caml_failwith("fsevents not supported on this platform - use inotify");
    CAMLreturn(Val_long(0));
}

CAMLprim value kernel_fsevents_watch(value ctx_val) {
    CAMLparam1(ctx_val);
    caml_failwith("fsevents not supported on this platform - use inotify");
    CAMLreturn(Val_unit);
}

CAMLprim value kernel_fsevents_stop(value ctx_val) {
    CAMLparam1(ctx_val);
    caml_failwith("fsevents not supported on this platform - use inotify");
    CAMLreturn(Val_unit);
}

#endif
