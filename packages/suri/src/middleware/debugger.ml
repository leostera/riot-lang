(**
   The visual debugger is temporarily disabled while Suri removes its dependency
   on Riot's build model. Keep the middleware in place as a pass-through so
   existing applications and examples keep compiling.
*)
let debugger = fun ~conn ~next -> next conn
