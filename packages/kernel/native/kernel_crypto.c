/* Cryptographic hash implementations using system libraries */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <string.h>

#ifdef __APPLE__
#include <CommonCrypto/CommonDigest.h>
#include <CommonCrypto/CommonHMAC.h>

/* SHA1 implementation using macOS CommonCrypto */
CAMLprim value kernel_crypto_sha1(value data) {
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

/* SHA256 implementation using macOS CommonCrypto */
CAMLprim value kernel_crypto_sha256(value data) {
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

/* SHA512 implementation */
CAMLprim value kernel_crypto_sha512(value data) {
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

/* MD5 implementation using macOS CommonCrypto */
#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif
CAMLprim value kernel_crypto_md5(value data) {
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
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

#else
/* Linux/other platforms - use OpenSSL */
#include <openssl/evp.h>

static value kernel_crypto_evp_digest(value data, const EVP_MD *digest, int digest_length) {
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

CAMLprim value kernel_crypto_sha1(value data) {
    return kernel_crypto_evp_digest(data, EVP_sha1(), EVP_MD_size(EVP_sha1()));
}

CAMLprim value kernel_crypto_sha256(value data) {
    return kernel_crypto_evp_digest(data, EVP_sha256(), EVP_MD_size(EVP_sha256()));
}

CAMLprim value kernel_crypto_sha512(value data) {
    return kernel_crypto_evp_digest(data, EVP_sha512(), EVP_MD_size(EVP_sha512()));
}

CAMLprim value kernel_crypto_md5(value data) {
    return kernel_crypto_evp_digest(data, EVP_md5(), EVP_MD_size(EVP_md5()));
}

#endif

/* Simple XOR-based hash for testing */
CAMLprim value kernel_crypto_simple_hash(value data) {
    CAMLparam1(data);
    CAMLlocal1(result);

    int len = caml_string_length(data);
    unsigned char *bytes = (unsigned char *)Bytes_val(data);
    unsigned long hash = 5381;

    for (int i = 0; i < len; i++) {
        hash = ((hash << 5) + hash) + bytes[i]; /* hash * 33 + c */
    }

    result = caml_alloc_string(8);
    unsigned char *res_bytes = (unsigned char *)Bytes_val(result);
    for (int i = 0; i < 8; i++) {
        res_bytes[i] = (hash >> (i * 8)) & 0xFF;
    }

    CAMLreturn(result);
}

/* High-quality hash using SipHash-2-4 (simplified version) */
/* In production, use a proper SipHash implementation */
CAMLprim value kernel_crypto_siphash(value key, value data) {
    CAMLparam2(key, data);
    caml_failwith("SipHash not yet implemented");
    CAMLreturn(Val_unit);
}

#ifdef __APPLE__
/* HMAC-SHA256 implementation using macOS CommonCrypto */
CAMLprim value kernel_crypto_hmac_sha256(value key, value data) {
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
/* Linux/other platforms - use OpenSSL HMAC */
#include <openssl/hmac.h>

CAMLprim value kernel_crypto_hmac_sha256(value key, value data) {
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
