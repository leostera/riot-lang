#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/uio.h>
#include <unistd.h>
#include "kernel_new_errors.h"

#define KERNEL_NEW_NET_CONNECT_CONNECTED 0
#define KERNEL_NEW_NET_CONNECT_IN_PROGRESS 1
#define KERNEL_NEW_NET_ADDR_KIND_STREAM 0
#define KERNEL_NEW_NET_ADDR_KIND_DATAGRAM 1
#define KERNEL_NEW_NET_RESOLVER_ERR_BASE 4096
#define KERNEL_NEW_NET_RESOLVER_ERR_HOST_NOT_FOUND (KERNEL_NEW_NET_RESOLVER_ERR_BASE + 1)
#define KERNEL_NEW_NET_RESOLVER_ERR_TEMPORARY_FAILURE (KERNEL_NEW_NET_RESOLVER_ERR_BASE + 2)
#define KERNEL_NEW_NET_RESOLVER_ERR_RESOLUTION_FAILED (KERNEL_NEW_NET_RESOLVER_ERR_BASE + 3)

static int kernel_new_net_configure_socket(int fd) {
  if (fcntl(fd, F_SETFD, FD_CLOEXEC) == -1) {
    return -1;
  }

  int current_flags = fcntl(fd, F_GETFL, 0);
  if (current_flags == -1) {
    return -1;
  }

  if (fcntl(fd, F_SETFL, current_flags | O_NONBLOCK) == -1) {
    return -1;
  }

#ifdef SO_NOSIGPIPE
  int no_sigpipe = 1;
  if (setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &no_sigpipe, sizeof(no_sigpipe)) == -1) {
    return -1;
  }
#endif

  return 0;
}

static char *kernel_new_net_copy_ocaml_bytes_slice(value bytes_val, int pos, int len) {
  char *copy = NULL;

  if (len > 0) {
    copy = malloc((size_t)len);
    if (copy == NULL) {
      caml_raise_out_of_memory();
    }

    memcpy(copy, Bytes_val(bytes_val) + pos, (size_t)len);
  }

  return copy;
}

static ssize_t kernel_new_net_recv_into_heap_bytes(int fd, value buffer_val, int pos, int len) {
  char *copy = NULL;
  ssize_t result;

  if (len > 0) {
    copy = malloc((size_t)len);
    if (copy == NULL) {
      caml_raise_out_of_memory();
    }
  }

  caml_enter_blocking_section();
  result = recv(fd, copy, (size_t)len, 0);
  caml_leave_blocking_section();

  if (result > 0) {
    memcpy(Bytes_val(buffer_val) + pos, copy, (size_t)result);
  }

  int saved_errno = errno;
  free(copy);
  errno = saved_errno;
  return result;
}

static ssize_t kernel_new_net_send_from_heap_bytes(int fd, value buffer_val, int pos, int len) {
  char *copy = kernel_new_net_copy_ocaml_bytes_slice(buffer_val, pos, len);
  ssize_t result;

  caml_enter_blocking_section();
  result = send(fd, copy, (size_t)len, 0);
  caml_leave_blocking_section();

  int saved_errno = errno;
  free(copy);
  errno = saved_errno;
  return result;
}

static ssize_t kernel_new_net_recvfrom_into_heap_bytes(
  int fd,
  value buffer_val,
  int pos,
  int len,
  struct sockaddr *addr,
  socklen_t *addr_len) {
  char *copy = NULL;
  ssize_t result;

  if (len > 0) {
    copy = malloc((size_t)len);
    if (copy == NULL) {
      caml_raise_out_of_memory();
    }
  }

  caml_enter_blocking_section();
  result = recvfrom(fd, copy, (size_t)len, 0, addr, addr_len);
  caml_leave_blocking_section();

  if (result > 0) {
    memcpy(Bytes_val(buffer_val) + pos, copy, (size_t)result);
  }

  int saved_errno = errno;
  free(copy);
  errno = saved_errno;
  return result;
}

static ssize_t kernel_new_net_sendto_from_heap_bytes(
  int fd,
  value buffer_val,
  int pos,
  int len,
  const struct sockaddr *addr,
  socklen_t addr_len) {
  char *copy = kernel_new_net_copy_ocaml_bytes_slice(buffer_val, pos, len);
  ssize_t result;

  caml_enter_blocking_section();
  result = sendto(fd, copy, (size_t)len, 0, addr, addr_len);
  caml_leave_blocking_section();

  int saved_errno = errno;
  free(copy);
  errno = saved_errno;
  return result;
}

static int kernel_new_net_sockaddr_of_parts(
  const char *ip,
  int port,
  struct sockaddr_storage *storage,
  socklen_t *addr_len_out)
{
  if (port < 0 || port > 65535) {
    errno = EINVAL;
    return -1;
  }

  memset(storage, 0, sizeof(*storage));

  struct sockaddr_in *ipv4 = (struct sockaddr_in *)storage;
  if (inet_pton(AF_INET, ip, &ipv4->sin_addr) == 1) {
    ipv4->sin_family = AF_INET;
    ipv4->sin_port = htons((uint16_t)port);
    *addr_len_out = sizeof(struct sockaddr_in);
    return AF_INET;
  }

  struct sockaddr_in6 *ipv6 = (struct sockaddr_in6 *)storage;
  if (inet_pton(AF_INET6, ip, &ipv6->sin6_addr) == 1) {
    ipv6->sin6_family = AF_INET6;
    ipv6->sin6_port = htons((uint16_t)port);
    *addr_len_out = sizeof(struct sockaddr_in6);
    return AF_INET6;
  }

  errno = EINVAL;
  return -1;
}

static int kernel_new_net_socket_addr_components(
  const struct sockaddr *addr,
  socklen_t addr_len,
  char *ip_buffer,
  size_t ip_buffer_len,
  int *port_out)
{
  (void)addr_len;

  switch (addr->sa_family) {
    case AF_INET: {
      const struct sockaddr_in *ipv4 = (const struct sockaddr_in *)addr;
      if (inet_ntop(AF_INET, &ipv4->sin_addr, ip_buffer, (socklen_t)ip_buffer_len) == NULL) {
        return -1;
      }
      *port_out = (int)ntohs(ipv4->sin_port);
      return 0;
    }
    case AF_INET6: {
      const struct sockaddr_in6 *ipv6 = (const struct sockaddr_in6 *)addr;
      if (inet_ntop(AF_INET6, &ipv6->sin6_addr, ip_buffer, (socklen_t)ip_buffer_len) == NULL) {
        return -1;
      }
      *port_out = (int)ntohs(ipv6->sin6_port);
      return 0;
    }
    default:
      errno = EAFNOSUPPORT;
      return -1;
  }
}

static value kernel_new_net_copy_socket_addr(const struct sockaddr *addr, socklen_t addr_len) {
  CAMLparam0();
  CAMLlocal3(tuple, ip_val, port_val);

  char ip_buffer[INET6_ADDRSTRLEN];
  int port = 0;
  if (kernel_new_net_socket_addr_components(addr, addr_len, ip_buffer, sizeof(ip_buffer), &port) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  ip_val = caml_copy_string(ip_buffer);
  port_val = Val_int(port);
  tuple = caml_alloc_tuple(2);
  Store_field(tuple, 0, ip_val);
  Store_field(tuple, 1, port_val);
  CAMLreturn(kernel_new_result_ok(tuple));
}

static struct iovec *kernel_new_net_build_iovecs(value segments_val, int *count_out) {
  int count = Wosize_val(segments_val);
  if (count == 0) {
    *count_out = 0;
    return NULL;
  }

  struct iovec *iovecs = malloc(sizeof(struct iovec) * count);
  if (iovecs == NULL) {
    caml_raise_out_of_memory();
  }

  for (int index = 0; index < count; index++) {
    value segment_val = Field(segments_val, index);
    int length = (int)Caml_ba_array_val(segment_val)->dim[0];
    iovecs[index].iov_base = (void *)Caml_ba_data_val(segment_val);
    iovecs[index].iov_len = (size_t)length;
  }

  *count_out = count;
  return iovecs;
}

static int kernel_new_net_set_reuse_options(int fd, int reuse_addr, int reuse_port) {
  int enabled = 1;

  if (reuse_addr != 0) {
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &enabled, sizeof(enabled)) == -1) {
      return -1;
    }
  }

  if (reuse_port != 0) {
#ifdef SO_REUSEPORT
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &enabled, sizeof(enabled)) == -1) {
      return -1;
    }
#else
    errno = ENOTSUP;
    return -1;
#endif
  }

  return 0;
}

static int kernel_new_net_resolver_error(int status) {
  switch (status) {
#ifdef EAI_NONAME
    case EAI_NONAME:
      return KERNEL_NEW_NET_RESOLVER_ERR_HOST_NOT_FOUND;
#endif
#ifdef EAI_NODATA
    case EAI_NODATA:
      return KERNEL_NEW_NET_RESOLVER_ERR_HOST_NOT_FOUND;
#endif
#ifdef EAI_AGAIN
    case EAI_AGAIN:
      return KERNEL_NEW_NET_RESOLVER_ERR_TEMPORARY_FAILURE;
#endif
#ifdef EAI_SYSTEM
    case EAI_SYSTEM:
      return kernel_new_error_of_errno(errno);
#endif
    default:
      return KERNEL_NEW_NET_RESOLVER_ERR_RESOLUTION_FAILED;
  }
}

CAMLprim value kernel_new_net_ip_addr_is_valid(value ip_val) {
  CAMLparam1(ip_val);

  struct sockaddr_storage storage;
  socklen_t addr_len = 0;
  int family = kernel_new_net_sockaddr_of_parts(String_val(ip_val), 0, &storage, &addr_len);
  CAMLreturn(Val_bool(family != -1));
}

CAMLprim value kernel_new_net_addr_resolve(value host_val, value port_val, value kind_val) {
  CAMLparam3(host_val, port_val, kind_val);
  CAMLlocal4(entries, item, tuple, ip_val);

  int port = Int_val(port_val);
  if (port < 0 || port > 65535) {
    errno = EINVAL;
    CAMLreturn(kernel_new_result_errno());
  }

  struct addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  switch (Int_val(kind_val)) {
    case KERNEL_NEW_NET_ADDR_KIND_STREAM:
      hints.ai_socktype = SOCK_STREAM;
      break;
    case KERNEL_NEW_NET_ADDR_KIND_DATAGRAM:
      hints.ai_socktype = SOCK_DGRAM;
      break;
    default:
      errno = EINVAL;
      CAMLreturn(kernel_new_result_errno());
  }

  char service_buffer[16];
  snprintf(service_buffer, sizeof(service_buffer), "%d", port);

  struct addrinfo *results = NULL;
  int status = getaddrinfo(String_val(host_val), service_buffer, &hints, &results);
  if (status != 0) {
    CAMLreturn(kernel_new_result_error(kernel_new_net_resolver_error(status)));
  }

  int count = 0;
  for (struct addrinfo *current = results; current != NULL; current = current->ai_next) {
    if (current->ai_family == AF_INET || current->ai_family == AF_INET6) {
      count++;
    }
  }

  entries = caml_alloc(count, 0);
  int index = 0;
  for (struct addrinfo *current = results; current != NULL; current = current->ai_next) {
    if (current->ai_family != AF_INET && current->ai_family != AF_INET6) {
      continue;
    }

    char ip_buffer[INET6_ADDRSTRLEN];
    int resolved_port = 0;
    if (kernel_new_net_socket_addr_components(
          current->ai_addr,
          current->ai_addrlen,
          ip_buffer,
          sizeof(ip_buffer),
          &resolved_port) == -1) {
      freeaddrinfo(results);
      CAMLreturn(kernel_new_result_errno());
    }

    ip_val = caml_copy_string(ip_buffer);
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, ip_val);
    Store_field(tuple, 1, Val_int(resolved_port));
    item = tuple;
    Store_field(entries, index, item);
    index++;
  }

  freeaddrinfo(results);
  CAMLreturn(kernel_new_result_ok(entries));
}

CAMLprim value kernel_new_net_socket_close(value fd_val) {
  CAMLparam1(fd_val);

  if (close(Int_val(fd_val)) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_net_tcp_stream_shutdown(value fd_val, value how_val) {
  CAMLparam2(fd_val, how_val);

  int how;
  switch (Int_val(how_val)) {
    case 0:
      how = SHUT_RD;
      break;
    case 1:
      how = SHUT_WR;
      break;
    case 2:
      how = SHUT_RDWR;
      break;
    default:
      errno = EINVAL;
      CAMLreturn(kernel_new_result_errno());
  }

  if (shutdown(Int_val(fd_val), how) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_net_socket_local_addr(value fd_val) {
  CAMLparam1(fd_val);

  struct sockaddr_storage storage;
  socklen_t addr_len = sizeof(storage);
  if (getsockname(Int_val(fd_val), (struct sockaddr *)&storage, &addr_len) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_net_copy_socket_addr((struct sockaddr *)&storage, addr_len));
}

CAMLprim value kernel_new_net_tcp_stream_finish_connect(value fd_val) {
  CAMLparam1(fd_val);

  int fd = Int_val(fd_val);
  int so_error = 0;
  socklen_t so_error_len = sizeof(so_error);
  if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &so_error, &so_error_len) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (so_error != 0) {
    errno = so_error;
    CAMLreturn(kernel_new_result_errno());
  }

  struct sockaddr_storage storage;
  socklen_t addr_len = sizeof(storage);
  if (getpeername(fd, (struct sockaddr *)&storage, &addr_len) == -1) {
    if (errno == ENOTCONN) {
      errno = EAGAIN;
    }
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_net_tcp_stream_connect(value ip_val, value port_val) {
  CAMLparam2(ip_val, port_val);
  CAMLlocal2(tuple, result);

  struct sockaddr_storage storage;
  socklen_t addr_len = 0;
  int family = kernel_new_net_sockaddr_of_parts(String_val(ip_val), Int_val(port_val), &storage, &addr_len);
  if (family == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  int fd = socket(family, SOCK_STREAM, 0);
  if (fd == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_net_configure_socket(fd) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (connect(fd, (struct sockaddr *)&storage, addr_len) == 0) {
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, Val_int(fd));
    Store_field(tuple, 1, Val_int(KERNEL_NEW_NET_CONNECT_CONNECTED));
    result = kernel_new_result_ok(tuple);
    CAMLreturn(result);
  }

  if (errno == EINPROGRESS) {
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, Val_int(fd));
    Store_field(tuple, 1, Val_int(KERNEL_NEW_NET_CONNECT_IN_PROGRESS));
    result = kernel_new_result_ok(tuple);
    CAMLreturn(result);
  }

  {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }
}

CAMLprim value kernel_new_net_unix_stream_connect(value path_val) {
  CAMLparam1(path_val);
  CAMLlocal2(tuple, result);

  const char *path = String_val(path_val);
  struct sockaddr_un addr;
  memset(&addr, 0, sizeof(addr));
  addr.sun_family = AF_UNIX;

  size_t path_len = strlen(path);
  if (path_len >= sizeof(addr.sun_path)) {
    errno = ENAMETOOLONG;
    CAMLreturn(kernel_new_result_errno());
  }
  memcpy(addr.sun_path, path, path_len + 1);

  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_net_configure_socket(fd) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (connect(fd, (struct sockaddr *)&addr, sizeof(addr)) == 0) {
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, Val_int(fd));
    Store_field(tuple, 1, Val_int(KERNEL_NEW_NET_CONNECT_CONNECTED));
    result = kernel_new_result_ok(tuple);
    CAMLreturn(result);
  }

  if (errno == EINPROGRESS) {
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, Val_int(fd));
    Store_field(tuple, 1, Val_int(KERNEL_NEW_NET_CONNECT_IN_PROGRESS));
    result = kernel_new_result_ok(tuple);
    CAMLreturn(result);
  }

  {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }
}

CAMLprim value kernel_new_net_tcp_stream_read(value fd_val, value buffer_val, value pos_val, value len_val) {
  CAMLparam4(fd_val, buffer_val, pos_val, len_val);

  ssize_t result = kernel_new_net_recv_into_heap_bytes(
    Int_val(fd_val),
    buffer_val,
    Int_val(pos_val),
    Int_val(len_val));

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_net_tcp_stream_write(value fd_val, value buffer_val, value pos_val, value len_val) {
  CAMLparam4(fd_val, buffer_val, pos_val, len_val);

  ssize_t result = kernel_new_net_send_from_heap_bytes(
    Int_val(fd_val),
    buffer_val,
    Int_val(pos_val),
    Int_val(len_val));

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_net_tcp_stream_readv(value fd_val, value segments_val) {
  CAMLparam2(fd_val, segments_val);

  int count = 0;
  struct iovec *iovecs = kernel_new_net_build_iovecs(segments_val, &count);
  ssize_t result;

  if (count == 0) {
    CAMLreturn(kernel_new_result_ok(Val_int(0)));
  }

  caml_enter_blocking_section();
  result = readv(Int_val(fd_val), iovecs, count);
  caml_leave_blocking_section();

  free(iovecs);

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_net_tcp_stream_writev(value fd_val, value segments_val) {
  CAMLparam2(fd_val, segments_val);

  int count = 0;
  struct iovec *iovecs = kernel_new_net_build_iovecs(segments_val, &count);
  ssize_t result;

  if (count == 0) {
    CAMLreturn(kernel_new_result_ok(Val_int(0)));
  }

  caml_enter_blocking_section();
  result = writev(Int_val(fd_val), iovecs, count);
  caml_leave_blocking_section();

  free(iovecs);

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_net_tcp_stream_peer_addr(value fd_val) {
  CAMLparam1(fd_val);

  struct sockaddr_storage storage;
  socklen_t addr_len = sizeof(storage);
  if (getpeername(Int_val(fd_val), (struct sockaddr *)&storage, &addr_len) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_net_copy_socket_addr((struct sockaddr *)&storage, addr_len));
}

CAMLprim value kernel_new_net_tcp_listener_bind(
  value ip_val,
  value port_val,
  value reuse_addr_val,
  value reuse_port_val,
  value backlog_val)
{
  CAMLparam5(ip_val, port_val, reuse_addr_val, reuse_port_val, backlog_val);

  struct sockaddr_storage storage;
  socklen_t addr_len = 0;
  int family = kernel_new_net_sockaddr_of_parts(String_val(ip_val), Int_val(port_val), &storage, &addr_len);
  if (family == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  int fd = socket(family, SOCK_STREAM, 0);
  if (fd == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_net_set_reuse_options(fd, Bool_val(reuse_addr_val), Bool_val(reuse_port_val)) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (bind(fd, (struct sockaddr *)&storage, addr_len) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (listen(fd, Int_val(backlog_val)) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_net_configure_socket(fd) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(fd)));
}

CAMLprim value kernel_new_net_tcp_listener_accept(value listener_val) {
  CAMLparam1(listener_val);
  CAMLlocal3(tuple, addr_val, result);

  struct sockaddr_storage storage;
  socklen_t addr_len = sizeof(storage);
  int client_fd = accept(Int_val(listener_val), (struct sockaddr *)&storage, &addr_len);
  if (client_fd == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_net_configure_socket(client_fd) == -1) {
    int saved_errno = errno;
    close(client_fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  {
    value addr_result = kernel_new_net_copy_socket_addr((struct sockaddr *)&storage, addr_len);
    if (Tag_val(addr_result) != 0) {
      int saved_errno = Int_val(Field(addr_result, 0));
      close(client_fd);
      CAMLreturn(kernel_new_result_error(saved_errno));
    }

    addr_val = Field(addr_result, 0);
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, Val_int(client_fd));
    Store_field(tuple, 1, addr_val);
    result = kernel_new_result_ok(tuple);
    CAMLreturn(result);
  }
}

CAMLprim value kernel_new_net_udp_socket_bind(
  value ip_val,
  value port_val,
  value reuse_addr_val,
  value reuse_port_val)
{
  CAMLparam4(ip_val, port_val, reuse_addr_val, reuse_port_val);

  struct sockaddr_storage storage;
  socklen_t addr_len = 0;
  int family = kernel_new_net_sockaddr_of_parts(String_val(ip_val), Int_val(port_val), &storage, &addr_len);
  if (family == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  int fd = socket(family, SOCK_DGRAM, 0);
  if (fd == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_net_set_reuse_options(fd, Bool_val(reuse_addr_val), Bool_val(reuse_port_val)) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (bind(fd, (struct sockaddr *)&storage, addr_len) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  if (kernel_new_net_configure_socket(fd) == -1) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(fd)));
}

CAMLprim value kernel_new_net_udp_socket_connect(value socket_val, value ip_val, value port_val) {
  CAMLparam3(socket_val, ip_val, port_val);

  struct sockaddr_storage storage;
  socklen_t addr_len = 0;
  if (kernel_new_net_sockaddr_of_parts(String_val(ip_val), Int_val(port_val), &storage, &addr_len) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (connect(Int_val(socket_val), (struct sockaddr *)&storage, addr_len) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_net_udp_socket_recv(value socket_val, value buffer_val, value pos_val, value len_val) {
  CAMLparam4(socket_val, buffer_val, pos_val, len_val);

  ssize_t result = kernel_new_net_recv_into_heap_bytes(
    Int_val(socket_val),
    buffer_val,
    Int_val(pos_val),
    Int_val(len_val));

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_net_udp_socket_recv_from(
  value socket_val,
  value buffer_val,
  value pos_val,
  value len_val)
{
  CAMLparam4(socket_val, buffer_val, pos_val, len_val);
  CAMLlocal3(tuple, addr_val, result);

  struct sockaddr_storage storage;
  socklen_t addr_len = sizeof(storage);
  ssize_t bytes_read;

  bytes_read = kernel_new_net_recvfrom_into_heap_bytes(
    Int_val(socket_val),
    buffer_val,
    Int_val(pos_val),
    Int_val(len_val),
    (struct sockaddr *)&storage,
    &addr_len);

  if (bytes_read == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  {
    value addr_result = kernel_new_net_copy_socket_addr((struct sockaddr *)&storage, addr_len);
    if (Tag_val(addr_result) != 0) {
      CAMLreturn(addr_result);
    }

    addr_val = Field(addr_result, 0);
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, Val_int(bytes_read));
    Store_field(tuple, 1, addr_val);
    result = kernel_new_result_ok(tuple);
    CAMLreturn(result);
  }
}

CAMLprim value kernel_new_net_udp_socket_send(value socket_val, value buffer_val, value pos_val, value len_val) {
  CAMLparam4(socket_val, buffer_val, pos_val, len_val);

  ssize_t result = kernel_new_net_send_from_heap_bytes(
    Int_val(socket_val),
    buffer_val,
    Int_val(pos_val),
    Int_val(len_val));

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}

CAMLprim value kernel_new_net_udp_socket_send_to(
  value socket_val,
  value ip_val,
  value port_val,
  value buffer_val,
  value bounds_val)
{
  CAMLparam5(socket_val, ip_val, port_val, buffer_val, bounds_val);

  struct sockaddr_storage storage;
  socklen_t addr_len = 0;
  int pos = Int_val(Field(bounds_val, 0));
  int len = Int_val(Field(bounds_val, 1));
  if (kernel_new_net_sockaddr_of_parts(String_val(ip_val), Int_val(port_val), &storage, &addr_len) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  ssize_t result;
  result = kernel_new_net_sendto_from_heap_bytes(
    Int_val(socket_val),
    buffer_val,
    pos,
    len,
    (struct sockaddr *)&storage,
    addr_len);

  if (result == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(result)));
}
