# Unix.* things used in packages/std/src/fs/

## Types
- Unix.dir_handle
- Unix.error
- Unix.file_descr
- Unix.open_flag
- Unix.stats
- Unix.Unix_error

## Functions
- Unix.dup
- Unix.error_message
- Unix.fchmod
- Unix.fstat
- Unix.fsync
- Unix.LargeFile.lseek
- Unix.LargeFile.fstat
- Unix.LargeFile.ftruncate
- Unix.lockf
- Unix.lstat

## Error Constants
- Unix.EACCES
- Unix.EAGAIN
- Unix.EEXIST

## File Open Flags
- Unix.O_APPEND
- Unix.O_CREAT
- Unix.O_EXCL
- Unix.O_RDONLY
- Unix.O_RDWR
- Unix.O_TRUNC
- Unix.O_WRONLY

## Lock Commands
- Unix.F_LOCK
- Unix.F_RLOCK
- Unix.F_TLOCK
- Unix.F_TRLOCK
- Unix.F_ULOCK

## Seek Commands
- Unix.SEEK_CUR
- Unix.SEEK_END
- Unix.SEEK_SET

## File Type Constants
- Unix.S_BLK
- Unix.S_CHR
- Unix.S_DIR
- Unix.S_FIFO
- Unix.S_LNK
- Unix.S_REG
- Unix.S_SOCK

## Unix.stats Fields
- Unix.st_atime
- Unix.st_dev
- Unix.st_gid
- Unix.st_ino
- Unix.st_kind
- Unix.st_mtime
- Unix.st_nlink
- Unix.st_perm
- Unix.st_rdev
- Unix.st_size
- Unix.st_uid