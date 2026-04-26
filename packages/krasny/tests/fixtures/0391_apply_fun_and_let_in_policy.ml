let make_iterator client connection=Iter.MutIterator.make (module Iterator) {Iterator.client=client;connection;buffer="";done_=false}

let configure observed=let config=H.Config.make ~connection_policy:H.Config.ReuseConnection ~transport:(fun _request->Ok(H.Response.make ~status:200 ~body:"ok"())) ~telemetry:(fun telemetry->observed:=Some telemetry) () in config

let header ()=let line=very_long_header_prefix^very_long_header_name^very_long_header_value^very_long_header_suffix^"\r\n" in line
