/* Cryptographic hash implementations using system libraries */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <string.h>

#ifdef __APPLE__
#include <CommonCrypto/CommonDigest.h>
#include <CommonCrypto/CommonHMAC.h>

static void std_crypto_for_each_iovec(value v_iovs, void (*f)(const unsigned char *, size_t, void *), void *ctx) {
    mlsize_t n_iovs = Wosize_val(v_iovs);
    for (mlsize_t i = 0; i < n_iovs; i++) {
        value v_iov = Field(v_iovs, i);
        size_t len = (size_t) Caml_ba_array_val(v_iov)->dim[0];
        if (len == 0) continue;
        const unsigned char *data = (const unsigned char *) Caml_ba_data_val(v_iov);
        f(data, len, ctx);
    }
}

static void std_crypto_sha1_update_cc(const unsigned char *data, size_t len, void *ctx) {
    CC_SHA1_Update((CC_SHA1_CTX *) ctx, data, len);
}

static void std_crypto_sha256_update_cc(const unsigned char *data, size_t len, void *ctx) {
    CC_SHA256_Update((CC_SHA256_CTX *) ctx, data, len);
}

typedef struct {
    CC_SHA256_CTX ctx;
} std_crypto_sha256_state;

static std_crypto_sha256_state *std_crypto_sha256_state_val(value v_state) {
    return (std_crypto_sha256_state *)Data_custom_val(v_state);
}

static struct custom_operations std_crypto_sha256_state_ops = {
    "riot.std.crypto.sha256_state",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static void std_crypto_sha512_update_cc(const unsigned char *data, size_t len, void *ctx) {
    CC_SHA512_Update((CC_SHA512_CTX *) ctx, data, len);
}

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif
static void std_crypto_md5_update_cc(const unsigned char *data, size_t len, void *ctx) {
    CC_MD5_Update((CC_MD5_CTX *) ctx, data, len);
}
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

CAMLprim value std_crypto_sha1(value data) {
    CAMLparam1(data);
    CAMLlocal1(result);

    unsigned char hash[CC_SHA1_DIGEST_LENGTH];
    CC_SHA1_CTX ctx;

    CC_SHA1_Init(&ctx);
    CC_SHA1_Update(&ctx, Bytes_val(data), caml_string_length(data));
    CC_SHA1_Final(hash, &ctx);

    result = caml_alloc_string(CC_SHA1_DIGEST_LENGTH);
    memcpy(Bytes_val(result), hash, CC_SHA1_DIGEST_LENGTH);

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha1_iovec(value v_iovs) {
    CAMLparam1(v_iovs);
    CAMLlocal1(result);

    unsigned char hash[CC_SHA1_DIGEST_LENGTH];
    CC_SHA1_CTX ctx;

    CC_SHA1_Init(&ctx);
    std_crypto_for_each_iovec(v_iovs, std_crypto_sha1_update_cc, &ctx);
    CC_SHA1_Final(hash, &ctx);

    result = caml_alloc_string(CC_SHA1_DIGEST_LENGTH);
    memcpy(Bytes_val(result), hash, CC_SHA1_DIGEST_LENGTH);

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha256(value data) {
    CAMLparam1(data);
    CAMLlocal1(result);

    unsigned char hash[CC_SHA256_DIGEST_LENGTH];
    CC_SHA256_CTX ctx;

    CC_SHA256_Init(&ctx);
    CC_SHA256_Update(&ctx, Bytes_val(data), caml_string_length(data));
    CC_SHA256_Final(hash, &ctx);

    result = caml_alloc_string(CC_SHA256_DIGEST_LENGTH);
    memcpy(Bytes_val(result), hash, CC_SHA256_DIGEST_LENGTH);

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha256_iovec(value v_iovs) {
    CAMLparam1(v_iovs);
    CAMLlocal1(result);

    unsigned char hash[CC_SHA256_DIGEST_LENGTH];
    CC_SHA256_CTX ctx;

    CC_SHA256_Init(&ctx);
    std_crypto_for_each_iovec(v_iovs, std_crypto_sha256_update_cc, &ctx);
    CC_SHA256_Final(hash, &ctx);

    result = caml_alloc_string(CC_SHA256_DIGEST_LENGTH);
    memcpy(Bytes_val(result), hash, CC_SHA256_DIGEST_LENGTH);

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha256_create(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);

    result = caml_alloc_custom(&std_crypto_sha256_state_ops, sizeof(std_crypto_sha256_state), 0, 1);
    std_crypto_sha256_state *state = std_crypto_sha256_state_val(result);
    CC_SHA256_Init(&state->ctx);

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha256_update(value v_state, value data) {
    CAMLparam2(v_state, data);

    std_crypto_sha256_state *state = std_crypto_sha256_state_val(v_state);
    CC_SHA256_Update(&state->ctx, Bytes_val(data), caml_string_length(data));

    CAMLreturn(Val_unit);
}

CAMLprim value std_crypto_sha256_update_iovec(value v_state, value v_iovs) {
    CAMLparam2(v_state, v_iovs);

    std_crypto_sha256_state *state = std_crypto_sha256_state_val(v_state);
    std_crypto_for_each_iovec(v_iovs, std_crypto_sha256_update_cc, &state->ctx);

    CAMLreturn(Val_unit);
}

CAMLprim value std_crypto_sha256_finish(value v_state) {
    CAMLparam1(v_state);
    CAMLlocal1(result);

    unsigned char hash[CC_SHA256_DIGEST_LENGTH];
    std_crypto_sha256_state *state = std_crypto_sha256_state_val(v_state);
    CC_SHA256_CTX ctx = state->ctx;
    CC_SHA256_Final(hash, &ctx);

    result = caml_alloc_string(CC_SHA256_DIGEST_LENGTH);
    memcpy(Bytes_val(result), hash, CC_SHA256_DIGEST_LENGTH);

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha512(value data) {
    CAMLparam1(data);
    CAMLlocal1(result);

    unsigned char hash[CC_SHA512_DIGEST_LENGTH];
    CC_SHA512_CTX ctx;

    CC_SHA512_Init(&ctx);
    CC_SHA512_Update(&ctx, Bytes_val(data), caml_string_length(data));
    CC_SHA512_Final(hash, &ctx);

    result = caml_alloc_string(CC_SHA512_DIGEST_LENGTH);
    memcpy(Bytes_val(result), hash, CC_SHA512_DIGEST_LENGTH);

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha512_iovec(value v_iovs) {
    CAMLparam1(v_iovs);
    CAMLlocal1(result);

    unsigned char hash[CC_SHA512_DIGEST_LENGTH];
    CC_SHA512_CTX ctx;

    CC_SHA512_Init(&ctx);
    std_crypto_for_each_iovec(v_iovs, std_crypto_sha512_update_cc, &ctx);
    CC_SHA512_Final(hash, &ctx);

    result = caml_alloc_string(CC_SHA512_DIGEST_LENGTH);
    memcpy(Bytes_val(result), hash, CC_SHA512_DIGEST_LENGTH);

    CAMLreturn(result);
}

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif
CAMLprim value std_crypto_md5(value data) {
    CAMLparam1(data);
    CAMLlocal1(result);

    unsigned char hash[CC_MD5_DIGEST_LENGTH];
    CC_MD5_CTX ctx;

    CC_MD5_Init(&ctx);
    CC_MD5_Update(&ctx, Bytes_val(data), caml_string_length(data));
    CC_MD5_Final(hash, &ctx);

    result = caml_alloc_string(CC_MD5_DIGEST_LENGTH);
    memcpy(Bytes_val(result), hash, CC_MD5_DIGEST_LENGTH);

    CAMLreturn(result);
}

CAMLprim value std_crypto_md5_iovec(value v_iovs) {
    CAMLparam1(v_iovs);
    CAMLlocal1(result);

    unsigned char hash[CC_MD5_DIGEST_LENGTH];
    CC_MD5_CTX ctx;

    CC_MD5_Init(&ctx);
    std_crypto_for_each_iovec(v_iovs, std_crypto_md5_update_cc, &ctx);
    CC_MD5_Final(hash, &ctx);

    result = caml_alloc_string(CC_MD5_DIGEST_LENGTH);
    memcpy(Bytes_val(result), hash, CC_MD5_DIGEST_LENGTH);

    CAMLreturn(result);
}
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

#else
#include <openssl/evp.h>

typedef struct {
    EVP_MD_CTX *ctx;
} std_crypto_sha256_state;

static std_crypto_sha256_state *std_crypto_sha256_state_val(value v_state) {
    return (std_crypto_sha256_state *)Data_custom_val(v_state);
}

static void std_crypto_sha256_state_finalize(value v_state) {
    std_crypto_sha256_state *state = std_crypto_sha256_state_val(v_state);
    if (state->ctx != NULL) {
        EVP_MD_CTX_free(state->ctx);
        state->ctx = NULL;
    }
}

static struct custom_operations std_crypto_sha256_state_ops = {
    "riot.std.crypto.sha256_state",
    std_crypto_sha256_state_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static void std_crypto_update_iovec_evp(EVP_MD_CTX *ctx, value v_iovs) {
    mlsize_t n_iovs = Wosize_val(v_iovs);
    for (mlsize_t i = 0; i < n_iovs; i++) {
        value v_iov = Field(v_iovs, i);
        size_t len = (size_t) Caml_ba_array_val(v_iov)->dim[0];
        if (len == 0) continue;
        unsigned char *data = (unsigned char *) Caml_ba_data_val(v_iov);
        if (EVP_DigestUpdate(ctx, data, len) != 1) {
            caml_failwith("EVP_DigestUpdate failed");
        }
    }
}

static value std_crypto_evp_digest(value data, const EVP_MD *digest, int digest_length) {
    CAMLparam1(data);
    CAMLlocal1(result);

    unsigned char hash[EVP_MAX_MD_SIZE];
    unsigned int hash_len = 0;
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    if (ctx == NULL) {
        caml_failwith("EVP_MD_CTX_new failed");
    }

    if (EVP_DigestInit_ex(ctx, digest, NULL) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestInit_ex failed");
    }

    if (EVP_DigestUpdate(ctx, Bytes_val(data), caml_string_length(data)) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestUpdate failed");
    }

    if (EVP_DigestFinal_ex(ctx, hash, &hash_len) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestFinal_ex failed");
    }

    EVP_MD_CTX_free(ctx);

    result = caml_alloc_string(digest_length);
    memcpy(Bytes_val(result), hash, digest_length);

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha1(value data) {
    return std_crypto_evp_digest(data, EVP_sha1(), EVP_MD_size(EVP_sha1()));
}

CAMLprim value std_crypto_sha1_iovec(value v_iovs) {
    CAMLparam1(v_iovs);
    CAMLlocal1(result);

    unsigned char hash[EVP_MAX_MD_SIZE];
    unsigned int hash_len = 0;
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    if (ctx == NULL) {
        caml_failwith("EVP_MD_CTX_new failed");
    }

    if (EVP_DigestInit_ex(ctx, EVP_sha1(), NULL) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestInit_ex failed");
    }

    std_crypto_update_iovec_evp(ctx, v_iovs);

    if (EVP_DigestFinal_ex(ctx, hash, &hash_len) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestFinal_ex failed");
    }

    EVP_MD_CTX_free(ctx);

    result = caml_alloc_string(EVP_MD_size(EVP_sha1()));
    memcpy(Bytes_val(result), hash, EVP_MD_size(EVP_sha1()));

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha256(value data) {
    return std_crypto_evp_digest(data, EVP_sha256(), EVP_MD_size(EVP_sha256()));
}

CAMLprim value std_crypto_sha256_iovec(value v_iovs) {
    CAMLparam1(v_iovs);
    CAMLlocal1(result);

    unsigned char hash[EVP_MAX_MD_SIZE];
    unsigned int hash_len = 0;
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    if (ctx == NULL) {
        caml_failwith("EVP_MD_CTX_new failed");
    }

    if (EVP_DigestInit_ex(ctx, EVP_sha256(), NULL) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestInit_ex failed");
    }

    std_crypto_update_iovec_evp(ctx, v_iovs);

    if (EVP_DigestFinal_ex(ctx, hash, &hash_len) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestFinal_ex failed");
    }

    EVP_MD_CTX_free(ctx);

    result = caml_alloc_string(EVP_MD_size(EVP_sha256()));
    memcpy(Bytes_val(result), hash, EVP_MD_size(EVP_sha256()));

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha256_create(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);

    result = caml_alloc_custom(&std_crypto_sha256_state_ops, sizeof(std_crypto_sha256_state), 0, 1);
    std_crypto_sha256_state *state = std_crypto_sha256_state_val(result);
    state->ctx = EVP_MD_CTX_new();
    if (state->ctx == NULL) {
        caml_failwith("EVP_MD_CTX_new failed");
    }

    if (EVP_DigestInit_ex(state->ctx, EVP_sha256(), NULL) != 1) {
        EVP_MD_CTX_free(state->ctx);
        state->ctx = NULL;
        caml_failwith("EVP_DigestInit_ex failed");
    }

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha256_update(value v_state, value data) {
    CAMLparam2(v_state, data);

    std_crypto_sha256_state *state = std_crypto_sha256_state_val(v_state);
    if (EVP_DigestUpdate(state->ctx, Bytes_val(data), caml_string_length(data)) != 1) {
        caml_failwith("EVP_DigestUpdate failed");
    }

    CAMLreturn(Val_unit);
}

CAMLprim value std_crypto_sha256_update_iovec(value v_state, value v_iovs) {
    CAMLparam2(v_state, v_iovs);

    std_crypto_sha256_state *state = std_crypto_sha256_state_val(v_state);
    std_crypto_update_iovec_evp(state->ctx, v_iovs);

    CAMLreturn(Val_unit);
}

CAMLprim value std_crypto_sha256_finish(value v_state) {
    CAMLparam1(v_state);
    CAMLlocal1(result);

    unsigned char hash[EVP_MAX_MD_SIZE];
    unsigned int hash_len = 0;
    std_crypto_sha256_state *state = std_crypto_sha256_state_val(v_state);
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    if (ctx == NULL) {
        caml_failwith("EVP_MD_CTX_new failed");
    }

    if (EVP_MD_CTX_copy_ex(ctx, state->ctx) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_MD_CTX_copy_ex failed");
    }

    if (EVP_DigestFinal_ex(ctx, hash, &hash_len) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestFinal_ex failed");
    }

    EVP_MD_CTX_free(ctx);

    result = caml_alloc_string(EVP_MD_size(EVP_sha256()));
    memcpy(Bytes_val(result), hash, EVP_MD_size(EVP_sha256()));

    CAMLreturn(result);
}

CAMLprim value std_crypto_sha512(value data) {
    return std_crypto_evp_digest(data, EVP_sha512(), EVP_MD_size(EVP_sha512()));
}

CAMLprim value std_crypto_sha512_iovec(value v_iovs) {
    CAMLparam1(v_iovs);
    CAMLlocal1(result);

    unsigned char hash[EVP_MAX_MD_SIZE];
    unsigned int hash_len = 0;
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    if (ctx == NULL) {
        caml_failwith("EVP_MD_CTX_new failed");
    }

    if (EVP_DigestInit_ex(ctx, EVP_sha512(), NULL) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestInit_ex failed");
    }

    std_crypto_update_iovec_evp(ctx, v_iovs);

    if (EVP_DigestFinal_ex(ctx, hash, &hash_len) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestFinal_ex failed");
    }

    EVP_MD_CTX_free(ctx);

    result = caml_alloc_string(EVP_MD_size(EVP_sha512()));
    memcpy(Bytes_val(result), hash, EVP_MD_size(EVP_sha512()));

    CAMLreturn(result);
}

CAMLprim value std_crypto_md5(value data) {
    return std_crypto_evp_digest(data, EVP_md5(), EVP_MD_size(EVP_md5()));
}

CAMLprim value std_crypto_md5_iovec(value v_iovs) {
    CAMLparam1(v_iovs);
    CAMLlocal1(result);

    unsigned char hash[EVP_MAX_MD_SIZE];
    unsigned int hash_len = 0;
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    if (ctx == NULL) {
        caml_failwith("EVP_MD_CTX_new failed");
    }

    if (EVP_DigestInit_ex(ctx, EVP_md5(), NULL) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestInit_ex failed");
    }

    std_crypto_update_iovec_evp(ctx, v_iovs);

    if (EVP_DigestFinal_ex(ctx, hash, &hash_len) != 1) {
        EVP_MD_CTX_free(ctx);
        caml_failwith("EVP_DigestFinal_ex failed");
    }

    EVP_MD_CTX_free(ctx);

    result = caml_alloc_string(EVP_MD_size(EVP_md5()));
    memcpy(Bytes_val(result), hash, EVP_MD_size(EVP_md5()));

    CAMLreturn(result);
}

#endif

CAMLprim value std_crypto_simple_hash(value data) {
    CAMLparam1(data);
    CAMLlocal1(result);

    int len = caml_string_length(data);
    unsigned char *bytes = (unsigned char *)Bytes_val(data);
    unsigned long hash = 5381;

    for (int i = 0; i < len; i++) {
        hash = ((hash << 5) + hash) + bytes[i];
    }

    result = caml_alloc_string(8);
    unsigned char *res_bytes = (unsigned char *)Bytes_val(result);
    for (int i = 0; i < 8; i++) {
        res_bytes[i] = (hash >> (i * 8)) & 0xFF;
    }

    CAMLreturn(result);
}

CAMLprim value std_crypto_simple_hash_iovec(value v_iovs) {
    CAMLparam1(v_iovs);
    CAMLlocal1(result);

    unsigned long hash = 5381;
    mlsize_t n_iovs = Wosize_val(v_iovs);
    for (mlsize_t i = 0; i < n_iovs; i++) {
        value v_iov = Field(v_iovs, i);
        size_t len = (size_t) Caml_ba_array_val(v_iov)->dim[0];
        const unsigned char *bytes = (const unsigned char *) Caml_ba_data_val(v_iov);

        for (size_t j = 0; j < len; j++) {
            hash = ((hash << 5) + hash) + bytes[j];
        }
    }

    result = caml_alloc_string(8);
    unsigned char *res_bytes = (unsigned char *)Bytes_val(result);
    for (int i = 0; i < 8; i++) {
        res_bytes[i] = (hash >> (i * 8)) & 0xFF;
    }

    CAMLreturn(result);
}

#ifdef __APPLE__
CAMLprim value std_crypto_hmac_sha256(value key, value data) {
    CAMLparam2(key, data);
    CAMLlocal1(result);

    unsigned char mac[CC_SHA256_DIGEST_LENGTH];

    CCHmac(kCCHmacAlgSHA256,
           String_val(key), caml_string_length(key),
           String_val(data), caml_string_length(data),
           mac);

    result = caml_alloc_string(CC_SHA256_DIGEST_LENGTH);
    memcpy(Bytes_val(result), mac, CC_SHA256_DIGEST_LENGTH);

    CAMLreturn(result);
}
#else
#include <openssl/hmac.h>

CAMLprim value std_crypto_hmac_sha256(value key, value data) {
    CAMLparam2(key, data);
    CAMLlocal1(result);

    unsigned char mac[EVP_MAX_MD_SIZE];
    unsigned int mac_len = 0;

    HMAC(EVP_sha256(),
         String_val(key), caml_string_length(key),
         (unsigned char *)String_val(data), caml_string_length(data),
         mac, &mac_len);

    result = caml_alloc_string(mac_len);
    memcpy(Bytes_val(result), mac, mac_len);

    CAMLreturn(result);
}
#endif
