(** Generic worker pool for concurrent task processing

    (** Type of a generic worker pool *) type ('ctx, 'task, 'result) t

    (** Type of a worker function that processes a task in a context *) type
    ('ctx, 'task, 'result) worker_fn = 'ctx -> 'task -> ('result, string) result

    (** Type of a task provider that yields tasks to be processed *) type ('ctx,
    'task) task_provider = 'ctx -> 'task option

    (** Create and start a generic worker pool *) val start : workers:int ->
    ctx:'ctx -> worker_fn:('ctx, 'task, 'result) worker_fn ->
    task_provider:('ctx, 'task) task_provider -> ('ctx, 'task, 'result) t

    (** Wait for all tasks to complete and get results *) val await_completion :
    ('ctx, 'task, 'result) t -> (('task * 'result) list * ('task * string) list)

    (** Shut down the worker pool *) val shutdown : ('ctx, 'task, 'result) t ->
    unit *)
