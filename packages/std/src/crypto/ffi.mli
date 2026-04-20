val md5: string -> Hash.t

val md5_iovec: Kernel.IO.IoVec.t -> Hash.t

val sha1: string -> Hash.t

val sha1_iovec: Kernel.IO.IoVec.t -> Hash.t

val sha256: string -> Hash.t

val sha256_iovec: Kernel.IO.IoVec.t -> Hash.t

val sha512: string -> Hash.t

val sha512_iovec: Kernel.IO.IoVec.t -> Hash.t

val hmac_sha256: key:string -> data:string -> bytes

val default_hash: string -> Hash.t

val default_hash_iovec: Kernel.IO.IoVec.t -> Hash.t
