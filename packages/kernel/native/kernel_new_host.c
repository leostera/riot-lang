#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if defined(__x86_64__) || defined(_M_X64)
#define KERNEL_HOST_ARCH "x86_64"
#elif defined(__aarch64__) || defined(_M_ARM64)
#define KERNEL_HOST_ARCH "aarch64"
#elif defined(__arm__) || defined(_M_ARM)
#define KERNEL_HOST_ARCH "arm"
#elif defined(__i386__) || defined(_M_IX86)
#define KERNEL_HOST_ARCH "i686"
#elif defined(__riscv) && (__riscv_xlen == 64)
#define KERNEL_HOST_ARCH "riscv64"
#else
#define KERNEL_HOST_ARCH "unknown"
#endif

#if defined(__APPLE__)
#define KERNEL_HOST_VENDOR "apple"
#elif defined(_WIN32)
#define KERNEL_HOST_VENDOR "pc"
#else
#define KERNEL_HOST_VENDOR "unknown"
#endif

#if defined(__linux__)
#define KERNEL_HOST_OS "linux"
#elif defined(__APPLE__)
#define KERNEL_HOST_OS "darwin"
#elif defined(_WIN32)
#define KERNEL_HOST_OS "windows"
#elif defined(__FreeBSD__)
#define KERNEL_HOST_OS "freebsd"
#elif defined(__OpenBSD__)
#define KERNEL_HOST_OS "openbsd"
#elif defined(__NetBSD__)
#define KERNEL_HOST_OS "netbsd"
#elif defined(__CYGWIN__)
#define KERNEL_HOST_OS "cygwin"
#else
#define KERNEL_HOST_OS "unknown"
#endif

#if defined(__linux__)
#if defined(__GLIBC__)
#define KERNEL_HOST_ABI "gnu"
#elif defined(__MUSL__)
#define KERNEL_HOST_ABI "musl"
#else
#define KERNEL_HOST_ABI ""
#endif
#elif defined(_WIN32)
#if defined(_MSC_VER)
#define KERNEL_HOST_ABI "msvc"
#elif defined(__MINGW32__)
#define KERNEL_HOST_ABI "mingw"
#else
#define KERNEL_HOST_ABI ""
#endif
#else
#define KERNEL_HOST_ABI ""
#endif

CAMLprim value kernel_new_host_arch(value unit_val) {
  CAMLparam1(unit_val);
  CAMLreturn(caml_copy_string(KERNEL_HOST_ARCH));
}

CAMLprim value kernel_new_host_vendor(value unit_val) {
  CAMLparam1(unit_val);
  CAMLreturn(caml_copy_string(KERNEL_HOST_VENDOR));
}

CAMLprim value kernel_new_host_os(value unit_val) {
  CAMLparam1(unit_val);
  CAMLreturn(caml_copy_string(KERNEL_HOST_OS));
}

CAMLprim value kernel_new_host_abi(value unit_val) {
  CAMLparam1(unit_val);
  CAMLreturn(caml_copy_string(KERNEL_HOST_ABI));
}
