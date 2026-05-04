let make : type socket err. reader:(socket, err) reader -> writer:(socket, err) writer -> from_io_error:(err -> error) -> uri:uri -> t = fun ~reader ~writer ~from_io_error ~uri -> make_conn reader writer from_io_error uri
let perform : type a b. (a, b) step_callback = fun k eff -> k eff

type 'a t = 'a list

let map (type a b) (iter : a t) ~(fn : a -> b) : b t = failwith "todo"
