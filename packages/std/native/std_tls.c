/* TLS implementation using OpenSSL BIO pairs for transport abstraction. */

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#if defined(__APPLE__) || defined(__linux__)
#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/x509.h>

typedef struct {
  SSL_CTX *ctx;
  SSL *ssl;
  BIO *internal_bio;
  BIO *network_bio;
  int is_server;
  int handshake_done;
} std_tls_engine_t;

static const char *std_tls_ssl_error_name(int err) {
  switch (err) {
    case SSL_ERROR_NONE:
      return "SSL_ERROR_NONE";
    case SSL_ERROR_SSL:
      return "SSL_ERROR_SSL";
    case SSL_ERROR_WANT_READ:
      return "SSL_ERROR_WANT_READ";
    case SSL_ERROR_WANT_WRITE:
      return "SSL_ERROR_WANT_WRITE";
    case SSL_ERROR_WANT_X509_LOOKUP:
      return "SSL_ERROR_WANT_X509_LOOKUP";
    case SSL_ERROR_SYSCALL:
      return "SSL_ERROR_SYSCALL";
    case SSL_ERROR_ZERO_RETURN:
      return "SSL_ERROR_ZERO_RETURN";
    case SSL_ERROR_WANT_CONNECT:
      return "SSL_ERROR_WANT_CONNECT";
    case SSL_ERROR_WANT_ACCEPT:
      return "SSL_ERROR_WANT_ACCEPT";
#ifdef SSL_ERROR_WANT_ASYNC
    case SSL_ERROR_WANT_ASYNC:
      return "SSL_ERROR_WANT_ASYNC";
#endif
#ifdef SSL_ERROR_WANT_ASYNC_JOB
    case SSL_ERROR_WANT_ASYNC_JOB:
      return "SSL_ERROR_WANT_ASYNC_JOB";
#endif
#ifdef SSL_ERROR_WANT_CLIENT_HELLO_CB
    case SSL_ERROR_WANT_CLIENT_HELLO_CB:
      return "SSL_ERROR_WANT_CLIENT_HELLO_CB";
#endif
    default:
      return "SSL_ERROR_UNKNOWN";
  }
}

__attribute__((noreturn))
static void std_tls_fail_ssl_operation(const char *operation, int ssl_err) {
  char message[512];
  unsigned long openssl_err = ERR_peek_last_error();

  if (openssl_err != 0) {
    char openssl_message[256];
    ERR_error_string_n(openssl_err, openssl_message, sizeof(openssl_message));
    snprintf(
      message,
      sizeof(message),
      "%s failed (%s): %s",
      operation,
      std_tls_ssl_error_name(ssl_err),
      openssl_message
    );
  } else if (ssl_err == SSL_ERROR_SYSCALL && errno != 0) {
    snprintf(
      message,
      sizeof(message),
      "%s failed (%s): errno=%d (%s)",
      operation,
      std_tls_ssl_error_name(ssl_err),
      errno,
      strerror(errno)
    );
  } else {
    snprintf(message, sizeof(message), "%s failed (%s)", operation, std_tls_ssl_error_name(ssl_err));
  }

  caml_failwith(message);
}

static void std_tls_engine_finalize(value engine_value) {
  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  if (engine->ssl) {
    SSL_free(engine->ssl);
  }
  if (engine->ctx) {
    SSL_CTX_free(engine->ctx);
  }
}

static struct custom_operations std_tls_engine_ops = {
  "riot.std.tls_engine",
  std_tls_engine_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value std_tls_init(value unit) {
  CAMLparam1(unit);
  SSL_load_error_strings();
  SSL_library_init();
  OpenSSL_add_all_algorithms();
  CAMLreturn(Val_unit);
}

CAMLprim value std_tls_is_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_bool(1));
}

CAMLprim value std_tls_version(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  result = caml_copy_string(OpenSSL_version(OPENSSL_VERSION));
  CAMLreturn(result);
}

CAMLprim value std_tls_create_client_engine(value hostname_value) {
  CAMLparam1(hostname_value);
  CAMLlocal1(engine_value);

  const char *hostname = String_val(hostname_value);
  SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
  if (!ctx) {
    caml_failwith("Failed to create SSL context");
  }

  SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
  SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
  SSL_CTX_set_default_verify_paths(ctx);

  SSL *ssl = SSL_new(ctx);
  if (!ssl) {
    SSL_CTX_free(ctx);
    caml_failwith("Failed to create SSL object");
  }

  SSL_set_connect_state(ssl);
  SSL_set_tlsext_host_name(ssl, hostname);

  BIO *internal_bio;
  BIO *network_bio;
  if (BIO_new_bio_pair(&internal_bio, 0, &network_bio, 0) != 1) {
    SSL_free(ssl);
    SSL_CTX_free(ctx);
    caml_failwith("Failed to create BIO pair");
  }

  SSL_set_bio(ssl, internal_bio, internal_bio);

  engine_value = caml_alloc_custom(&std_tls_engine_ops, sizeof(std_tls_engine_t), 0, 1);
  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  engine->ctx = ctx;
  engine->ssl = ssl;
  engine->internal_bio = internal_bio;
  engine->network_bio = network_bio;
  engine->is_server = 0;
  engine->handshake_done = 0;

  CAMLreturn(engine_value);
}

CAMLprim value std_tls_create_server_engine(value cert_file_value, value key_file_value) {
  CAMLparam2(cert_file_value, key_file_value);
  CAMLlocal1(engine_value);

  const char *cert_file = String_val(cert_file_value);
  const char *key_file = String_val(key_file_value);
  SSL_CTX *ctx = SSL_CTX_new(TLS_server_method());
  if (!ctx) {
    caml_failwith("Failed to create SSL context");
  }

  SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);

  if (SSL_CTX_use_certificate_file(ctx, cert_file, SSL_FILETYPE_PEM) != 1) {
    SSL_CTX_free(ctx);
    caml_failwith("Failed to load certificate file");
  }

  if (SSL_CTX_use_PrivateKey_file(ctx, key_file, SSL_FILETYPE_PEM) != 1) {
    SSL_CTX_free(ctx);
    caml_failwith("Failed to load private key file");
  }

  SSL *ssl = SSL_new(ctx);
  if (!ssl) {
    SSL_CTX_free(ctx);
    caml_failwith("Failed to create SSL object");
  }

  SSL_set_accept_state(ssl);

  BIO *internal_bio;
  BIO *network_bio;
  if (BIO_new_bio_pair(&internal_bio, 0, &network_bio, 0) != 1) {
    SSL_free(ssl);
    SSL_CTX_free(ctx);
    caml_failwith("Failed to create BIO pair");
  }

  SSL_set_bio(ssl, internal_bio, internal_bio);

  engine_value = caml_alloc_custom(&std_tls_engine_ops, sizeof(std_tls_engine_t), 0, 1);
  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  engine->ctx = ctx;
  engine->ssl = ssl;
  engine->internal_bio = internal_bio;
  engine->network_bio = network_bio;
  engine->is_server = 1;
  engine->handshake_done = 0;

  CAMLreturn(engine_value);
}

CAMLprim value std_tls_pump_encrypted_in(value engine_value, value buffer_value, value pos_value, value len_value) {
  CAMLparam4(engine_value, buffer_value, pos_value, len_value);

  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  char *buffer = (char *) Bytes_val(buffer_value);
  int pos = Int_val(pos_value);
  int len = Int_val(len_value);

  CAMLreturn(Val_int(BIO_write(engine->network_bio, buffer + pos, len)));
}

CAMLprim value std_tls_read_decrypted(value engine_value, value buffer_value, value pos_value, value len_value) {
  CAMLparam4(engine_value, buffer_value, pos_value, len_value);

  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  char *buffer = (char *) Bytes_val(buffer_value);
  int pos = Int_val(pos_value);
  int len = Int_val(len_value);

  errno = 0;
  ERR_clear_error();
  int bytes_read = SSL_read(engine->ssl, buffer + pos, len);

  if (bytes_read > 0) {
    engine->handshake_done = 1;
    CAMLreturn(Val_int(bytes_read));
  }

  switch (SSL_get_error(engine->ssl, bytes_read)) {
    case SSL_ERROR_WANT_READ:
      CAMLreturn(Val_int(-1));
    case SSL_ERROR_WANT_WRITE:
      CAMLreturn(Val_int(-2));
    case SSL_ERROR_ZERO_RETURN:
      CAMLreturn(Val_int(0));
    case SSL_ERROR_SYSCALL:
    case SSL_ERROR_SSL:
    default:
      std_tls_fail_ssl_operation("SSL_read", SSL_get_error(engine->ssl, bytes_read));
  }
}

CAMLprim value std_tls_write_plaintext(value engine_value, value buffer_value, value pos_value, value len_value) {
  CAMLparam4(engine_value, buffer_value, pos_value, len_value);

  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  char *buffer = (char *) Bytes_val(buffer_value);
  int pos = Int_val(pos_value);
  int len = Int_val(len_value);

  errno = 0;
  ERR_clear_error();
  int bytes_written = SSL_write(engine->ssl, buffer + pos, len);

  if (bytes_written > 0) {
    engine->handshake_done = 1;
    CAMLreturn(Val_int(bytes_written));
  }

  switch (SSL_get_error(engine->ssl, bytes_written)) {
    case SSL_ERROR_WANT_READ:
      CAMLreturn(Val_int(-1));
    case SSL_ERROR_WANT_WRITE:
      CAMLreturn(Val_int(-2));
    case SSL_ERROR_SYSCALL:
    case SSL_ERROR_SSL:
    default:
      std_tls_fail_ssl_operation("SSL_write", SSL_get_error(engine->ssl, bytes_written));
  }
}

CAMLprim value std_tls_read_encrypted_out(value engine_value, value buffer_value) {
  CAMLparam2(engine_value, buffer_value);

  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  char *buffer = (char *) Bytes_val(buffer_value);
  int buffer_length = caml_string_length(buffer_value);
  int pending = BIO_ctrl_pending(engine->network_bio);

  if (pending == 0) {
    CAMLreturn(Val_int(0));
  }

  int to_read = pending < buffer_length ? pending : buffer_length;
  int bytes_read = BIO_read(engine->network_bio, buffer, to_read);
  CAMLreturn(Val_int(bytes_read >= 0 ? bytes_read : 0));
}

CAMLprim value std_tls_handshake_complete(value engine_value) {
  CAMLparam1(engine_value);
  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  CAMLreturn(Val_bool(engine->handshake_done && SSL_is_init_finished(engine->ssl)));
}

CAMLprim value std_tls_alpn_protocol(value engine_value) {
  CAMLparam1(engine_value);
  CAMLlocal1(result);

  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  const unsigned char *data;
  unsigned int len;

  SSL_get0_alpn_selected(engine->ssl, &data, &len);
  if (len > 0) {
    result = caml_alloc(1, 0);
    Store_field(result, 0, caml_alloc_initialized_string(len, (const char *) data));
  } else {
    result = Val_int(0);
  }

  CAMLreturn(result);
}

CAMLprim value std_tls_do_handshake(value engine_value) {
  CAMLparam1(engine_value);

  std_tls_engine_t *engine = (std_tls_engine_t *) Data_custom_val(engine_value);
  errno = 0;
  ERR_clear_error();
  int result = SSL_do_handshake(engine->ssl);

  if (result == 1) {
    engine->handshake_done = 1;
    CAMLreturn(Val_int(0));
  }

  switch (SSL_get_error(engine->ssl, result)) {
    case SSL_ERROR_WANT_READ:
      CAMLreturn(Val_int(-1));
    case SSL_ERROR_WANT_WRITE:
      CAMLreturn(Val_int(-2));
    default:
      std_tls_fail_ssl_operation("SSL_do_handshake", SSL_get_error(engine->ssl, result));
  }
}

#else

CAMLprim value std_tls_init(value unit) { caml_failwith("TLS not available on this platform"); }

CAMLprim value std_tls_is_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_bool(0));
}

CAMLprim value std_tls_version(value unit) { caml_failwith("TLS not available on this platform"); }

CAMLprim value std_tls_create_client_engine(value hostname_value) {
  caml_failwith("TLS not available on this platform");
}

CAMLprim value std_tls_create_server_engine(value cert_file_value, value key_file_value) {
  caml_failwith("TLS not available on this platform");
}

CAMLprim value std_tls_pump_encrypted_in(value engine_value, value buffer_value, value pos_value, value len_value) {
  caml_failwith("TLS not available on this platform");
}

CAMLprim value std_tls_read_decrypted(value engine_value, value buffer_value, value pos_value, value len_value) {
  caml_failwith("TLS not available on this platform");
}

CAMLprim value std_tls_write_plaintext(value engine_value, value buffer_value, value pos_value, value len_value) {
  caml_failwith("TLS not available on this platform");
}

CAMLprim value std_tls_read_encrypted_out(value engine_value, value buffer_value) {
  caml_failwith("TLS not available on this platform");
}

CAMLprim value std_tls_handshake_complete(value engine_value) {
  caml_failwith("TLS not available on this platform");
}

CAMLprim value std_tls_alpn_protocol(value engine_value) {
  caml_failwith("TLS not available on this platform");
}

CAMLprim value std_tls_do_handshake(value engine_value) {
  caml_failwith("TLS not available on this platform");
}

#endif
