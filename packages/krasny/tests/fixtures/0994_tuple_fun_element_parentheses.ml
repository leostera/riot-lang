let make_capture_writer=fun ()->let chunks=ref [] in((fun chunk->chunks:=chunk::!chunks),fun ()->!chunks|>List.reverse|>String.concat "")
