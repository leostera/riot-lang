#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <string.h>
#include <stdlib.h>
#include <zlib.h>

typedef struct {
    z_stream stream;
    int initialized;
    int finished;
} gzip_decoder_t;

static void gzip_decoder_finalize(value v_decoder) {
    gzip_decoder_t *decoder = (gzip_decoder_t *)Data_custom_val(v_decoder);
    if (decoder->initialized) {
        inflateEnd(&decoder->stream);
        decoder->initialized = 0;
    }
}

static struct custom_operations gzip_decoder_ops = {
    "riot.kernel.gzip_decoder",
    gzip_decoder_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static value alloc_step_result(int error_code, int consumed, int produced, int status_code) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc_tuple(4);
    Store_field(result, 0, Val_int(error_code));
    Store_field(result, 1, Val_int(consumed));
    Store_field(result, 2, Val_int(produced));
    Store_field(result, 3, Val_int(status_code));
    CAMLreturn(result);
}

CAMLprim value kernel_gzip_create_decoder(value v_unit) {
    CAMLparam1(v_unit);
    CAMLlocal1(v_decoder);

    v_decoder = caml_alloc_custom(&gzip_decoder_ops, sizeof(gzip_decoder_t), 0, 1);
    gzip_decoder_t *decoder = (gzip_decoder_t *)Data_custom_val(v_decoder);
    memset(decoder, 0, sizeof(*decoder));

    int ret = inflateInit2(&decoder->stream, 15 + 16);
    if (ret != Z_OK) {
        caml_failwith("failed to initialize gzip decoder");
    }

    decoder->initialized = 1;
    decoder->finished = 0;

    CAMLreturn(v_decoder);
}

CAMLprim value kernel_gzip_decode(
    value v_decoder,
    value v_src,
    value v_src_pos,
    value v_src_len,
    value v_dst,
    value v_dst_pos,
    value v_dst_len
) {
    CAMLparam5(v_decoder, v_src, v_src_pos, v_src_len, v_dst);
    CAMLxparam2(v_dst_pos, v_dst_len);

    gzip_decoder_t *decoder = (gzip_decoder_t *)Data_custom_val(v_decoder);
    int src_pos = Int_val(v_src_pos);
    int src_len = Int_val(v_src_len);
    int dst_pos = Int_val(v_dst_pos);
    int dst_len = Int_val(v_dst_len);

    if (decoder->finished) {
        CAMLreturn(alloc_step_result(0, 0, 0, 2));
    }

    if (src_len == 0) {
        CAMLreturn(alloc_step_result(0, 0, 0, 0));
    }

    decoder->stream.next_in = (Bytef *)Bytes_val(v_src) + src_pos;
    decoder->stream.avail_in = (uInt)src_len;
    decoder->stream.next_out = (Bytef *)Bytes_val(v_dst) + dst_pos;
    decoder->stream.avail_out = (uInt)dst_len;

    int ret = inflate(&decoder->stream, Z_NO_FLUSH);
    int consumed = src_len - (int)decoder->stream.avail_in;
    int produced = dst_len - (int)decoder->stream.avail_out;

    if (ret == Z_STREAM_END) {
        decoder->finished = 1;
        CAMLreturn(alloc_step_result(0, consumed, produced, 2));
    }

    if (ret == Z_NEED_DICT) {
        CAMLreturn(alloc_step_result(2, consumed, produced, 0));
    }

    if (ret == Z_DATA_ERROR) {
        CAMLreturn(alloc_step_result(1, consumed, produced, 0));
    }

    if (ret == Z_MEM_ERROR) {
        CAMLreturn(alloc_step_result(4, consumed, produced, 0));
    }

    if (ret == Z_BUF_ERROR) {
        if (decoder->stream.avail_out == 0) {
            CAMLreturn(alloc_step_result(0, consumed, produced, 1));
        }
        CAMLreturn(alloc_step_result(0, consumed, produced, 0));
    }

    if (ret != Z_OK) {
        CAMLreturn(alloc_step_result(3, consumed, produced, 0));
    }

    if (decoder->stream.avail_out == 0) {
        CAMLreturn(alloc_step_result(0, consumed, produced, 1));
    }

    CAMLreturn(alloc_step_result(0, consumed, produced, 0));
}

CAMLprim value kernel_gzip_close_decoder(value v_decoder) {
    CAMLparam1(v_decoder);
    gzip_decoder_finalize(v_decoder);
    CAMLreturn(Val_unit);
}

CAMLprim value kernel_gzip_decode_bytecode(value *argv, int argn) {
    return kernel_gzip_decode(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}
