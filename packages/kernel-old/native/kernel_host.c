/* Host triplet detection using preprocessor directives */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* Detect architecture */
#if defined(__x86_64__) || defined(_M_X64)
  #define ARCH "x86_64"
#elif defined(__aarch64__) || defined(_M_ARM64)
  #define ARCH "aarch64"
#elif defined(__arm__) || defined(_M_ARM)
  #define ARCH "arm"
#elif defined(__i386__) || defined(_M_IX86)
  #define ARCH "i686"
#elif defined(__riscv) && (__riscv_xlen == 64)
  #define ARCH "riscv64"
#else
  #define ARCH "unknown"
#endif

/* Detect vendor */
#if defined(__APPLE__)
  #define VENDOR "apple"
#elif defined(_WIN32)
  #define VENDOR "pc"
#else
  #define VENDOR "unknown"
#endif

/* Detect OS */
#if defined(__linux__)
  #define OS "linux"
#elif defined(__APPLE__)
  #define OS "darwin"
#elif defined(_WIN32)
  #define OS "windows"
#elif defined(__FreeBSD__)
  #define OS "freebsd"
#elif defined(__OpenBSD__)
  #define OS "openbsd"
#elif defined(__NetBSD__)
  #define OS "netbsd"
#else
  #define OS "unknown"
#endif

/* Detect ABI */
#if defined(__linux__)
  #if defined(__GLIBC__)
    #define ABI "gnu"
  #elif defined(__MUSL__)
    #define ABI "musl"
  #else
    #define ABI ""
  #endif
#elif defined(_WIN32)
  #if defined(_MSC_VER)
    #define ABI "msvc"
  #elif defined(__MINGW32__)
    #define ABI "mingw"
  #else
    #define ABI ""
  #endif
#else
  #define ABI ""
#endif

/* Return architecture string */
CAMLprim value kernel_host_arch(value unit) {
    CAMLparam1(unit);
    CAMLreturn(caml_copy_string(ARCH));
}

/* Return vendor string */
CAMLprim value kernel_host_vendor(value unit) {
    CAMLparam1(unit);
    CAMLreturn(caml_copy_string(VENDOR));
}

/* Return OS string */
CAMLprim value kernel_host_os(value unit) {
    CAMLparam1(unit);
    CAMLreturn(caml_copy_string(OS));
}

/* Return ABI string (or empty string if none) */
CAMLprim value kernel_host_abi(value unit) {
    CAMLparam1(unit);
    CAMLreturn(caml_copy_string(ABI));
}
