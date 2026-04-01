#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#define TAR_PHASE_HEADER 0
#define TAR_PHASE_DATA 1
#define TAR_PHASE_PADDING 2
#define TAR_PHASE_END 3

#define TAR_ERR_NONE 0
#define TAR_ERR_INVALID_HEADER 1
#define TAR_ERR_ENTRY_IN_PROGRESS 2
#define TAR_ERR_INVALID_STATE 3
#define TAR_ERR_UNEXPECTED_EOF 4
#define TAR_ERR_OUT_OF_MEMORY 5

typedef struct {
    char *buffer;
    size_t capacity;
    size_t start;
    size_t end;
    int phase;
    uint64_t remaining_data;
    uint64_t remaining_padding;
    char current_path[256];
    char current_other_kind[2];
    char current_link_target[101];
    int current_kind;
    int current_has_other_kind;
    int current_has_link_target;
    int current_has_mode;
    int current_mode;
    int64_t current_size;
} tar_reader_t;

static void tar_reader_finalize(value v_reader) {
    tar_reader_t *reader = (tar_reader_t *)Data_custom_val(v_reader);
    if (reader->buffer != NULL) {
        free(reader->buffer);
        reader->buffer = NULL;
    }
}

static struct custom_operations tar_reader_ops = {
    "riot.kernel.tar_reader",
    tar_reader_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static int is_zero_block(const unsigned char *block) {
    for (size_t i = 0; i < 512; i++) {
        if (block[i] != 0) {
            return 0;
        }
    }
    return 1;
}

static void compact_buffer(tar_reader_t *reader) {
    if (reader->start == 0) {
        return;
    }

    if (reader->start == reader->end) {
        reader->start = 0;
        reader->end = 0;
        return;
    }

    memmove(reader->buffer, reader->buffer + reader->start, reader->end - reader->start);
    reader->end -= reader->start;
    reader->start = 0;
}

static int ensure_capacity(tar_reader_t *reader, size_t additional) {
    size_t available = reader->capacity - reader->end;
    if (available >= additional) {
        return 1;
    }

    compact_buffer(reader);
    available = reader->capacity - reader->end;
    if (available >= additional) {
        return 1;
    }

    size_t needed = reader->end + additional;
    size_t new_capacity = reader->capacity == 0 ? 4096 : reader->capacity;
    while (new_capacity < needed) {
        new_capacity *= 2;
    }

    char *new_buffer = realloc(reader->buffer, new_capacity);
    if (new_buffer == NULL) {
        return 0;
    }

    reader->buffer = new_buffer;
    reader->capacity = new_capacity;
    return 1;
}

static size_t buffered_len(tar_reader_t *reader) {
    return reader->end - reader->start;
}

static int parse_octal_field(const unsigned char *field, size_t field_len, uint64_t *out) {
    size_t i = 0;
    while (i < field_len && (field[i] == ' ' || field[i] == '\0')) {
        i++;
    }

    uint64_t value = 0;
    int seen_digit = 0;
    for (; i < field_len; i++) {
        unsigned char ch = field[i];
        if (ch == '\0' || ch == ' ') {
            break;
        }
        if (ch < '0' || ch > '7') {
            return 0;
        }
        seen_digit = 1;
        value = (value * 8) + (uint64_t)(ch - '0');
    }

    if (!seen_digit) {
        value = 0;
    }

    *out = value;
    return 1;
}

static void copy_field(char *dst, size_t dst_len, const unsigned char *src, size_t src_len) {
    size_t len = 0;
    while (len < src_len && src[len] != '\0') {
        len++;
    }
    if (len >= dst_len) {
        len = dst_len - 1;
    }
    memcpy(dst, src, len);
    dst[len] = '\0';
}

static int finalize_entry_if_done(tar_reader_t *reader) {
    if (reader->remaining_data != 0) {
        return TAR_ERR_ENTRY_IN_PROGRESS;
    }

    if (reader->phase == TAR_PHASE_END) {
        return TAR_ERR_INVALID_STATE;
    }

    if (reader->phase == TAR_PHASE_HEADER) {
        return TAR_ERR_NONE;
    }

    while (reader->remaining_padding > 0) {
        size_t available = buffered_len(reader);
        if (available == 0) {
            return TAR_ERR_UNEXPECTED_EOF;
        }

        size_t chunk = available;
        if (chunk > reader->remaining_padding) {
            chunk = (size_t)reader->remaining_padding;
        }

        reader->start += chunk;
        reader->remaining_padding -= (uint64_t)chunk;
    }

    compact_buffer(reader);
    reader->phase = TAR_PHASE_HEADER;
    return TAR_ERR_NONE;
}

static int prepare_next_header(tar_reader_t *reader) {
    if (reader->phase == TAR_PHASE_DATA || reader->phase == TAR_PHASE_PADDING) {
        if (reader->remaining_data != 0) {
            return TAR_ERR_ENTRY_IN_PROGRESS;
        }
        int finalize_err = finalize_entry_if_done(reader);
        if (finalize_err != TAR_ERR_NONE) {
            return finalize_err;
        }
    }

    return TAR_ERR_NONE;
}

static int parse_header(tar_reader_t *reader) {
    if (buffered_len(reader) < 512) {
        return 0;
    }

    const unsigned char *block = (const unsigned char *)(reader->buffer + reader->start);
    if (is_zero_block(block)) {
        if (buffered_len(reader) < 1024) {
            return 0;
        }
        const unsigned char *next_block = block + 512;
        if (!is_zero_block(next_block)) {
            return -TAR_ERR_INVALID_HEADER;
        }
        reader->start += 1024;
        compact_buffer(reader);
        reader->phase = TAR_PHASE_END;
        return 2;
    }

    uint64_t size = 0;
    uint64_t mode = 0;
    if (!parse_octal_field(block + 124, 12, &size) || !parse_octal_field(block + 100, 8, &mode)) {
        return -TAR_ERR_INVALID_HEADER;
    }

    char name[101];
    char prefix[156];
    char linkname[101];

    copy_field(name, sizeof(name), block + 0, 100);
    copy_field(prefix, sizeof(prefix), block + 345, 155);
    copy_field(linkname, sizeof(linkname), block + 157, 100);

    if (prefix[0] != '\0') {
        snprintf(reader->current_path, sizeof(reader->current_path), "%s/%s", prefix, name);
    } else {
        snprintf(reader->current_path, sizeof(reader->current_path), "%s", name);
    }

    reader->current_size = (int64_t)size;
    reader->current_mode = (int)mode;
    reader->current_has_mode = 1;
    reader->current_has_link_target = linkname[0] != '\0';
    if (reader->current_has_link_target) {
        snprintf(reader->current_link_target, sizeof(reader->current_link_target), "%s", linkname);
    } else {
        reader->current_link_target[0] = '\0';
    }

    reader->current_has_other_kind = 0;
    switch (block[156]) {
        case '\0':
        case '0':
            reader->current_kind = 0;
            break;
        case '5':
            reader->current_kind = 1;
            break;
        case '2':
            reader->current_kind = 2;
            break;
        case '1':
            reader->current_kind = 3;
            break;
        default:
            reader->current_kind = 4;
            reader->current_has_other_kind = 1;
            reader->current_other_kind[0] = (char)block[156];
            reader->current_other_kind[1] = '\0';
            break;
    }

    reader->remaining_data = size;
    reader->remaining_padding = (512 - (size % 512)) % 512;
    reader->phase = TAR_PHASE_DATA;
    reader->start += 512;
    compact_buffer(reader);
    return 1;
}

static value alloc_some(value inner) {
    value some = caml_alloc(1, 0);
    Store_field(some, 0, inner);
    return some;
}

static value alloc_header_tuple(tar_reader_t *reader) {
    CAMLparam0();
    CAMLlocal5(header, mode_opt, link_opt, other_opt, size_val);
    header = caml_alloc_tuple(6);
    Store_field(header, 0, caml_copy_string(reader->current_path));
    Store_field(header, 1, Val_int(reader->current_kind));
    if (reader->current_has_other_kind) {
        other_opt = alloc_some(caml_copy_string(reader->current_other_kind));
    } else {
        other_opt = Val_int(0);
    }
    Store_field(header, 2, other_opt);
    size_val = caml_copy_int64(reader->current_size);
    Store_field(header, 3, size_val);
    if (reader->current_has_mode) {
        mode_opt = alloc_some(Val_int(reader->current_mode));
    } else {
        mode_opt = Val_int(0);
    }
    Store_field(header, 4, mode_opt);
    if (reader->current_has_link_target) {
        link_opt = alloc_some(caml_copy_string(reader->current_link_target));
    } else {
        link_opt = Val_int(0);
    }
    Store_field(header, 5, link_opt);
    CAMLreturn(header);
}

CAMLprim value kernel_tar_create_reader(value v_unit) {
    CAMLparam1(v_unit);
    CAMLlocal1(v_reader);

    v_reader = caml_alloc_custom(&tar_reader_ops, sizeof(tar_reader_t), 0, 1);
    tar_reader_t *reader = (tar_reader_t *)Data_custom_val(v_reader);
    memset(reader, 0, sizeof(*reader));
    reader->phase = TAR_PHASE_HEADER;

    CAMLreturn(v_reader);
}

CAMLprim value kernel_tar_feed_reader(value v_reader, value v_src, value v_src_pos, value v_src_len) {
    CAMLparam4(v_reader, v_src, v_src_pos, v_src_len);

    tar_reader_t *reader = (tar_reader_t *)Data_custom_val(v_reader);
    int src_pos = Int_val(v_src_pos);
    int src_len = Int_val(v_src_len);

    if (src_len == 0) {
        CAMLreturn(Val_int(0));
    }

    if (!ensure_capacity(reader, (size_t)src_len)) {
        caml_failwith("failed to grow tar reader buffer");
    }

    memcpy(reader->buffer + reader->end, Bytes_val(v_src) + src_pos, (size_t)src_len);
    reader->end += (size_t)src_len;

    CAMLreturn(Val_int(src_len));
}

CAMLprim value kernel_tar_next_entry(value v_reader) {
    CAMLparam1(v_reader);
    CAMLlocal3(result, header_opt, header);

    tar_reader_t *reader = (tar_reader_t *)Data_custom_val(v_reader);
    int prep_err = prepare_next_header(reader);
    if (prep_err == TAR_ERR_ENTRY_IN_PROGRESS) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, Val_int(TAR_ERR_ENTRY_IN_PROGRESS));
        Store_field(result, 1, Val_int(0));
        Store_field(result, 2, Val_int(0));
        CAMLreturn(result);
    }
    if (prep_err == TAR_ERR_UNEXPECTED_EOF) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, Val_int(TAR_ERR_UNEXPECTED_EOF));
        Store_field(result, 1, Val_int(0));
        Store_field(result, 2, Val_int(0));
        CAMLreturn(result);
    }

    if (reader->phase == TAR_PHASE_END) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, Val_int(TAR_ERR_NONE));
        Store_field(result, 1, Val_int(2));
        Store_field(result, 2, Val_int(0));
        CAMLreturn(result);
    }

    int parsed = parse_header(reader);
    if (parsed == 0) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, Val_int(TAR_ERR_NONE));
        Store_field(result, 1, Val_int(0));
        Store_field(result, 2, Val_int(0));
        CAMLreturn(result);
    }
    if (parsed < 0) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, Val_int(-parsed));
        Store_field(result, 1, Val_int(0));
        Store_field(result, 2, Val_int(0));
        CAMLreturn(result);
    }
    if (parsed == 2) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, Val_int(TAR_ERR_NONE));
        Store_field(result, 1, Val_int(2));
        Store_field(result, 2, Val_int(0));
        CAMLreturn(result);
    }

    header = alloc_header_tuple(reader);
    header_opt = alloc_some(header);
    result = caml_alloc_tuple(3);
    Store_field(result, 0, Val_int(TAR_ERR_NONE));
    Store_field(result, 1, Val_int(1));
    Store_field(result, 2, header_opt);
    CAMLreturn(result);
}

CAMLprim value kernel_tar_read_entry_data(value v_reader, value v_dst, value v_dst_pos, value v_dst_len) {
    CAMLparam4(v_reader, v_dst, v_dst_pos, v_dst_len);
    CAMLlocal1(result);

    tar_reader_t *reader = (tar_reader_t *)Data_custom_val(v_reader);
    int dst_pos = Int_val(v_dst_pos);
    int dst_len = Int_val(v_dst_len);

    result = caml_alloc_tuple(3);

    if (reader->phase != TAR_PHASE_DATA && reader->phase != TAR_PHASE_PADDING) {
        Store_field(result, 0, Val_int(TAR_ERR_INVALID_STATE));
        Store_field(result, 1, Val_int(0));
        Store_field(result, 2, Val_int(0));
        CAMLreturn(result);
    }

    if (reader->remaining_data == 0) {
        if (buffered_len(reader) < reader->remaining_padding) {
            Store_field(result, 0, Val_int(TAR_ERR_NONE));
            Store_field(result, 1, Val_int(0));
            Store_field(result, 2, Val_int(0));
            CAMLreturn(result);
        }

        reader->start += (size_t)reader->remaining_padding;
        reader->remaining_padding = 0;
        reader->phase = TAR_PHASE_HEADER;
        compact_buffer(reader);

        Store_field(result, 0, Val_int(TAR_ERR_NONE));
        Store_field(result, 1, Val_int(2));
        Store_field(result, 2, Val_int(0));
        CAMLreturn(result);
    }

    size_t available = buffered_len(reader);
    if (available == 0) {
        Store_field(result, 0, Val_int(TAR_ERR_NONE));
        Store_field(result, 1, Val_int(0));
        Store_field(result, 2, Val_int(0));
        CAMLreturn(result);
    }

    size_t chunk = available;
    if (chunk > reader->remaining_data) {
        chunk = (size_t)reader->remaining_data;
    }
    if (chunk > (size_t)dst_len) {
        chunk = (size_t)dst_len;
    }

    memcpy(Bytes_val(v_dst) + dst_pos, reader->buffer + reader->start, chunk);
    reader->start += chunk;
    reader->remaining_data -= (uint64_t)chunk;

    if (reader->remaining_data == 0) {
        reader->phase = TAR_PHASE_PADDING;
    }

    compact_buffer(reader);
    Store_field(result, 0, Val_int(TAR_ERR_NONE));
    Store_field(result, 1, Val_int(1));
    Store_field(result, 2, Val_int((int)chunk));
    CAMLreturn(result);
}

CAMLprim value kernel_tar_skip_entry(value v_reader) {
    CAMLparam1(v_reader);
    CAMLlocal1(result);

    tar_reader_t *reader = (tar_reader_t *)Data_custom_val(v_reader);
    result = caml_alloc_tuple(2);

    if (reader->phase != TAR_PHASE_DATA && reader->phase != TAR_PHASE_PADDING) {
        Store_field(result, 0, Val_int(TAR_ERR_INVALID_STATE));
        Store_field(result, 1, Val_int(0));
        CAMLreturn(result);
    }

    while (reader->remaining_data > 0) {
        size_t available = buffered_len(reader);
        if (available == 0) {
            Store_field(result, 0, Val_int(TAR_ERR_NONE));
            Store_field(result, 1, Val_int(0));
            CAMLreturn(result);
        }
        size_t chunk = available;
        if (chunk > reader->remaining_data) {
            chunk = (size_t)reader->remaining_data;
        }
        reader->start += chunk;
        reader->remaining_data -= (uint64_t)chunk;
    }

    reader->phase = TAR_PHASE_PADDING;
    while (reader->remaining_padding > 0) {
        size_t available = buffered_len(reader);
        if (available == 0) {
            compact_buffer(reader);
            Store_field(result, 0, Val_int(TAR_ERR_NONE));
            Store_field(result, 1, Val_int(0));
            CAMLreturn(result);
        }
        size_t chunk = available;
        if (chunk > reader->remaining_padding) {
            chunk = (size_t)reader->remaining_padding;
        }
        reader->start += chunk;
        reader->remaining_padding -= (uint64_t)chunk;
    }

    reader->phase = TAR_PHASE_HEADER;
    compact_buffer(reader);
    Store_field(result, 0, Val_int(TAR_ERR_NONE));
    Store_field(result, 1, Val_int(1));
    CAMLreturn(result);
}

CAMLprim value kernel_tar_close_reader(value v_reader) {
    CAMLparam1(v_reader);
    tar_reader_finalize(v_reader);
    CAMLreturn(Val_unit);
}
