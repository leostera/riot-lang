type ('src, 'err) t = (('src, 'err) Reader.buffered, 'err) Reader.t

let of_reader = Reader.buffered

let to_reader value = value

let read = Reader.read

let read_vectored = Reader.read_vectored

let read_char = Reader.read_char

let read_line = Reader.read_line

let read_to_string = Reader.read_to_string
