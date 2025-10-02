type error = Unix.error

type file_kind =
  | Regular
  | Directory
  | Symlink
  | Block
  | Character
  | Fifo
  | Socket

let file_kind_of_unix = function
  | Unix.S_REG -> Regular
  | Unix.S_DIR -> Directory
  | Unix.S_LNK -> Symlink
  | Unix.S_BLK -> Block
  | Unix.S_CHR -> Character
  | Unix.S_FIFO -> Fifo
  | Unix.S_SOCK -> Socket

let file_kind_to_unix = function
  | Regular -> Unix.S_REG
  | Directory -> Unix.S_DIR
  | Symlink -> Unix.S_LNK
  | Block -> Unix.S_BLK
  | Character -> Unix.S_CHR
  | Fifo -> Unix.S_FIFO
  | Socket -> Unix.S_SOCK

let unix_error_message = Unix.error_message
