(**
   Generic worker pool for controlled parallel execution

   Two modes of operation:
   - DynamicWorkerPool: Manual task assignment via WorkerReady messages
   - SimpleWorkerPool: Automatic task distribution from pre-queued tasks 
*)
module DynamicWorkerPool = Dynamic

module SimpleWorkerPool = Simple
