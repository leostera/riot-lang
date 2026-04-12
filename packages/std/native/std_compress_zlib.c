#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

typedef struct {
  z_stream stream;
  int initialized;
  int finished;
} std_gzip_encoder_t;

typedef struct {
  z_stream stream;
  int initialized;
  int finished;
} std_gzip_decoder_t;

static std_gzip_encoder_t *std_gzip_encoder_val(value encoder_value) {
  return *((std_gzip_encoder_t **)Data_custom_val(encoder_value));
}

static std_gzip_decoder_t *std_gzip_decoder_val(value decoder_value) {
  return *((std_gzip_decoder_t **)Data_custom_val(decoder_value));
}

static void std_gzip_encoder_finalize(value encoder_value) {
  std_gzip_encoder_t *encoder = std_gzip_encoder_val(encoder_value);
  if (encoder == NULL) {
    return;
  }
  if (encoder->initialized) {
    deflateEnd(&encoder->stream);
    encoder->initialized = 0;
  }
  free(encoder);
  *((std_gzip_encoder_t **)Data_custom_val(encoder_value)) = NULL;
}

static void std_gzip_decoder_finalize(value decoder_value) {
  std_gzip_decoder_t *decoder = std_gzip_decoder_val(decoder_value);
  if (decoder == NULL) {
    return;
  }
  if (decoder->initialized) {
    inflateEnd(&decoder->stream);
    decoder->initialized = 0;
  }
  free(decoder);
  *((std_gzip_decoder_t **)Data_custom_val(decoder_value)) = NULL;
}

static struct custom_operations std_gzip_encoder_ops = {
  "riot.std.gzip_encoder",
  std_gzip_encoder_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations std_gzip_decoder_ops = {
  "riot.std.gzip_decoder",
  std_gzip_decoder_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value std_gzip_alloc_step_result(int error_code, int consumed, int produced, int status_code) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc_tuple(4);
  Store_field(result, 0, Val_int(error_code));
  Store_field(result, 1, Val_int(consumed));
  Store_field(result, 2, Val_int(produced));
  Store_field(result, 3, Val_int(status_code));
  CAMLreturn(result);
}

CAMLprim value std_gzip_create_encoder(value level_value) {
  CAMLparam1(level_value);
  CAMLlocal1(encoder_value);

  encoder_value = caml_alloc_custom(&std_gzip_encoder_ops, sizeof(std_gzip_encoder_t *), 0, 1);
  std_gzip_encoder_t *encoder = malloc(sizeof(std_gzip_encoder_t));
  if (encoder == NULL) {
    caml_failwith("failed to allocate gzip encoder");
  }
  *((std_gzip_encoder_t **)Data_custom_val(encoder_value)) = encoder;
  memset(encoder, 0, sizeof(*encoder));

  int level = Int_val(level_value);
  int result = deflateInit2(&encoder->stream, level, Z_DEFLATED, 15 + 16, 8, Z_DEFAULT_STRATEGY);
  if (result != Z_OK) {
    caml_failwith("failed to initialize gzip encoder");
  }

  encoder->initialized = 1;
  encoder->finished = 0;
  CAMLreturn(encoder_value);
}

CAMLprim value std_gzip_create_decoder(value unit_value) {
  CAMLparam1(unit_value);
  CAMLlocal1(decoder_value);

  decoder_value = caml_alloc_custom(&std_gzip_decoder_ops, sizeof(std_gzip_decoder_t *), 0, 1);
  std_gzip_decoder_t *decoder = malloc(sizeof(std_gzip_decoder_t));
  if (decoder == NULL) {
    caml_failwith("failed to allocate gzip decoder");
  }
  *((std_gzip_decoder_t **)Data_custom_val(decoder_value)) = decoder;
  memset(decoder, 0, sizeof(*decoder));

  int result = inflateInit2(&decoder->stream, 15 + 16);
  if (result != Z_OK) {
    caml_failwith("failed to initialize gzip decoder");
  }

  decoder->initialized = 1;
  decoder->finished = 0;
  CAMLreturn(decoder_value);
}

CAMLprim value std_gzip_decode(
  value decoder_value,
  value src_value,
  value src_pos_value,
  value src_len_value,
  value dst_value,
  value dst_pos_value,
  value dst_len_value
) {
  CAMLparam5(decoder_value, src_value, src_pos_value, src_len_value, dst_value);
  CAMLxparam2(dst_pos_value, dst_len_value);

  std_gzip_decoder_t *decoder = std_gzip_decoder_val(decoder_value);
  int src_pos = Int_val(src_pos_value);
  int src_len = Int_val(src_len_value);
  int dst_pos = Int_val(dst_pos_value);
  int dst_len = Int_val(dst_len_value);

  if (decoder->finished) {
    CAMLreturn(std_gzip_alloc_step_result(0, 0, 0, 2));
  }

  if (src_len == 0) {
    CAMLreturn(std_gzip_alloc_step_result(0, 0, 0, 0));
  }

  decoder->stream.next_in = (Bytef *)Bytes_val(src_value) + src_pos;
  decoder->stream.avail_in = (uInt)src_len;
  decoder->stream.next_out = (Bytef *)Bytes_val(dst_value) + dst_pos;
  decoder->stream.avail_out = (uInt)dst_len;

  int result = inflate(&decoder->stream, Z_NO_FLUSH);
  int consumed = src_len - (int)decoder->stream.avail_in;
  int produced = dst_len - (int)decoder->stream.avail_out;

  if (result == Z_STREAM_END) {
    decoder->finished = 1;
    CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 2));
  }

  if (result == Z_NEED_DICT) {
    CAMLreturn(std_gzip_alloc_step_result(2, consumed, produced, 0));
  }

  if (result == Z_DATA_ERROR) {
    CAMLreturn(std_gzip_alloc_step_result(1, consumed, produced, 0));
  }

  if (result == Z_MEM_ERROR) {
    CAMLreturn(std_gzip_alloc_step_result(4, consumed, produced, 0));
  }

  if (result == Z_BUF_ERROR) {
    if (decoder->stream.avail_out == 0) {
      CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 1));
    }
    CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 0));
  }

  if (result != Z_OK) {
    CAMLreturn(std_gzip_alloc_step_result(3, consumed, produced, 0));
  }

  if (decoder->stream.avail_out == 0) {
    CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 1));
  }

  CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 0));
}

CAMLprim value std_gzip_encode(
  value encoder_value,
  value src_value,
  value src_pos_value,
  value src_len_value,
  value dst_value,
  value dst_pos_value,
  value dst_len_value,
  value flush_value
) {
  CAMLparam5(encoder_value, src_value, src_pos_value, src_len_value, dst_value);
  CAMLxparam3(dst_pos_value, dst_len_value, flush_value);

  std_gzip_encoder_t *encoder = std_gzip_encoder_val(encoder_value);
  int src_pos = Int_val(src_pos_value);
  int src_len = Int_val(src_len_value);
  int dst_pos = Int_val(dst_pos_value);
  int dst_len = Int_val(dst_len_value);
  int flush = Int_val(flush_value);
  int z_flush = Z_NO_FLUSH;

  if (encoder->finished) {
    CAMLreturn(std_gzip_alloc_step_result(0, 0, 0, 2));
  }

  switch (flush) {
    case 0: z_flush = Z_NO_FLUSH; break;
    case 1: z_flush = Z_SYNC_FLUSH; break;
    case 2: z_flush = Z_FINISH; break;
    default: CAMLreturn(std_gzip_alloc_step_result(3, 0, 0, 0));
  }

  encoder->stream.next_in = (Bytef *)Bytes_val(src_value) + src_pos;
  encoder->stream.avail_in = (uInt)src_len;
  encoder->stream.next_out = (Bytef *)Bytes_val(dst_value) + dst_pos;
  encoder->stream.avail_out = (uInt)dst_len;

  int result = deflate(&encoder->stream, z_flush);
  int consumed = src_len - (int)encoder->stream.avail_in;
  int produced = dst_len - (int)encoder->stream.avail_out;

  if (result == Z_STREAM_END) {
    encoder->finished = 1;
    CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 2));
  }

  if (result == Z_MEM_ERROR) {
    CAMLreturn(std_gzip_alloc_step_result(4, consumed, produced, 0));
  }

  if (result == Z_BUF_ERROR) {
    if (encoder->stream.avail_out == 0) {
      CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 1));
    }
    CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 0));
  }

  if (result != Z_OK) {
    CAMLreturn(std_gzip_alloc_step_result(3, consumed, produced, 0));
  }

  if (encoder->stream.avail_out == 0) {
    CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 1));
  }

  CAMLreturn(std_gzip_alloc_step_result(0, consumed, produced, 0));
}

CAMLprim value std_gzip_close_decoder(value decoder_value) {
  CAMLparam1(decoder_value);
  std_gzip_decoder_finalize(decoder_value);
  CAMLreturn(Val_unit);
}

CAMLprim value std_gzip_close_encoder(value encoder_value) {
  CAMLparam1(encoder_value);
  std_gzip_encoder_finalize(encoder_value);
  CAMLreturn(Val_unit);
}

CAMLprim value std_gzip_encode_bytecode(value *argv, int argn) {
  (void)argn;
  return std_gzip_encode(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
}

CAMLprim value std_gzip_decode_bytecode(value *argv, int argn) {
  (void)argn;
  return std_gzip_decode(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}
