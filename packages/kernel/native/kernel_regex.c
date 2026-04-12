#define PCRE2_CODE_UNIT_WIDTH 8

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <pcre2.h>

typedef struct {
    pcre2_code *code;
} kernel_regex_t;

static void kernel_regex_finalize(value v_regex) {
    kernel_regex_t *regex = (kernel_regex_t *) Data_custom_val(v_regex);
    if (regex->code != NULL) {
        pcre2_code_free(regex->code);
        regex->code = NULL;
    }
}

static struct custom_operations kernel_regex_ops = {
    "riot.kernel.regex",
    kernel_regex_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static value kernel_regex_make_ok(value v_payload) {
    CAMLparam1(v_payload);
    CAMLlocal1(v_result);
    v_result = caml_alloc(1, 0);
    Store_field(v_result, 0, v_payload);
    CAMLreturn(v_result);
}

static value kernel_regex_make_error(const char *message, int has_offset, PCRE2_SIZE offset) {
    CAMLparam0();
    CAMLlocal4(v_result, v_error, v_message, v_offset);

    v_message = caml_copy_string(message);
    if (has_offset) {
        v_offset = caml_alloc(1, 0);
        Store_field(v_offset, 0, Val_int((int) offset));
    } else {
        v_offset = Val_int(0);
    }

    v_error = caml_alloc(2, 0);
    Store_field(v_error, 0, v_message);
    Store_field(v_error, 1, v_offset);

    v_result = caml_alloc(1, 1);
    Store_field(v_result, 0, v_error);
    CAMLreturn(v_result);
}

static kernel_regex_t *kernel_regex_data(value v_regex) {
    return (kernel_regex_t *) Data_custom_val(v_regex);
}

static pcre2_match_data *kernel_regex_create_match_data(kernel_regex_t *regex) {
    pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(regex->code, NULL);
    if (match_data == NULL) {
        caml_failwith("failed to allocate pcre2 match data");
    }
    return match_data;
}

CAMLprim value kernel_regex_compile(value v_pattern) {
    CAMLparam1(v_pattern);
    CAMLlocal1(v_regex);

    int error_code = 0;
    PCRE2_SIZE error_offset = 0;
    pcre2_code *code = pcre2_compile((PCRE2_SPTR) String_val(v_pattern),
                                     PCRE2_ZERO_TERMINATED,
                                     0,
                                     &error_code,
                                     &error_offset,
                                     NULL);

    if (code == NULL) {
        PCRE2_UCHAR buffer[256];
        int rc = pcre2_get_error_message(error_code, buffer, sizeof(buffer));
        const char *message = rc >= 0 ? (const char *) buffer : "unknown pcre2 compile error";
        CAMLreturn(kernel_regex_make_error(message, error_offset != PCRE2_UNSET, error_offset));
    }

    v_regex = caml_alloc_custom(&kernel_regex_ops, sizeof(kernel_regex_t), 0, 1);
    kernel_regex_data(v_regex)->code = code;
    CAMLreturn(kernel_regex_make_ok(v_regex));
}

CAMLprim value kernel_regex_is_match(value v_regex, value v_haystack) {
    CAMLparam2(v_regex, v_haystack);

    kernel_regex_t *regex = kernel_regex_data(v_regex);
    pcre2_match_data *match_data = kernel_regex_create_match_data(regex);
    int rc = pcre2_match(regex->code,
                         (PCRE2_SPTR) String_val(v_haystack),
                         caml_string_length(v_haystack),
                         0,
                         0,
                         match_data,
                         NULL);
    pcre2_match_data_free(match_data);

    if (rc == PCRE2_ERROR_NOMATCH) {
        CAMLreturn(Val_false);
    }
    if (rc < 0) {
        caml_failwith("pcre2_match failed");
    }

    CAMLreturn(Val_true);
}

CAMLprim value kernel_regex_find(value v_regex, value v_haystack) {
    CAMLparam2(v_regex, v_haystack);
    CAMLlocal3(v_pair, v_start, v_stop);

    kernel_regex_t *regex = kernel_regex_data(v_regex);
    pcre2_match_data *match_data = kernel_regex_create_match_data(regex);
    int rc = pcre2_match(regex->code,
                         (PCRE2_SPTR) String_val(v_haystack),
                         caml_string_length(v_haystack),
                         0,
                         0,
                         match_data,
                         NULL);

    if (rc == PCRE2_ERROR_NOMATCH) {
        pcre2_match_data_free(match_data);
        CAMLreturn(Val_int(0));
    }
    if (rc < 0) {
        pcre2_match_data_free(match_data);
        caml_failwith("pcre2_match failed");
    }

    PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
    v_start = Val_int((int) ovector[0]);
    v_stop = Val_int((int) ovector[1]);
    pcre2_match_data_free(match_data);

    v_pair = caml_alloc(2, 0);
    Store_field(v_pair, 0, v_start);
    Store_field(v_pair, 1, v_stop);

    value v_some = caml_alloc(1, 0);
    Store_field(v_some, 0, v_pair);
    CAMLreturn(v_some);
}
