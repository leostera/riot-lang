open Std

let command =
  let open ArgParser in
  let open Arg in
  command "new"
  |> about "Create a new database"
  |> arg (positional "path" |> help "Path for new database directory")

let run matches =
  let open ArgParser in
  let db_path = get_one matches "path" |> Option.expect ~msg:"path required" in
  
  (* Create directory *)
  (match Fs.create_dir_all (Path.v db_path) with
   | Error _ ->
       println ("Error: Failed to create directory: " ^ db_path);
       Error (Failure ("Failed to create directory: " ^ db_path))
   | Ok () ->
       (* Initialize database with LSM backend *)
       let db = Graph_store.open_exclusive ~data_dir:db_path ()
         |> Result.expect ~msg:"Failed to create database" in
       
       (* Close to persist *)
       Graph_store.close db;
       
       println ("Created database: " ^ db_path);
       println "Backend: LSM";
       println ("Location: " ^ db_path ^ "/");
       Ok ()
  )
