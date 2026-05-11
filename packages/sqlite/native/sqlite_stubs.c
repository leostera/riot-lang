#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <sqlite3.h>
#include <string.h>

typedef struct {
  sqlite3 *db;
} riot_sqlite_db;

typedef struct {
  sqlite3_stmt *stmt;
} riot_sqlite_stmt;

static void riot_sqlite_finalize_db(value value_db) {
  riot_sqlite_db *db = (riot_sqlite_db *)Data_custom_val(value_db);
  if (db->db != NULL) {
    (void)sqlite3_close_v2(db->db);
    db->db = NULL;
  }
}

static void riot_sqlite_finalize_stmt(value value_stmt) {
  riot_sqlite_stmt *stmt = (riot_sqlite_stmt *)Data_custom_val(value_stmt);
  if (stmt->stmt != NULL) {
    (void)sqlite3_finalize(stmt->stmt);
    stmt->stmt = NULL;
  }
}

static struct custom_operations riot_sqlite_db_ops = {
  "riot.sqlite.db",
  riot_sqlite_finalize_db,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations riot_sqlite_stmt_ops = {
  "riot.sqlite.stmt",
  riot_sqlite_finalize_stmt,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value riot_sqlite_error_value(int code, const char *message) {
  CAMLparam0();
  CAMLlocal2(record, message_value);
  const char *safe_message = message == NULL ? sqlite3_errstr(code) : message;
  message_value = caml_copy_string(safe_message);
  record = caml_alloc_tuple(2);
  Store_field(record, 0, Val_int(code));
  Store_field(record, 1, message_value);
  CAMLreturn(record);
}

static value riot_sqlite_ok(value payload) {
  CAMLparam1(payload);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

static value riot_sqlite_error(int code, const char *message) {
  CAMLparam0();
  CAMLlocal2(result, error);
  error = riot_sqlite_error_value(code, message);
  result = caml_alloc(1, 1);
  Store_field(result, 0, error);
  CAMLreturn(result);
}

static value riot_sqlite_db_error(sqlite3 *db, int code) {
  return riot_sqlite_error(code, db == NULL ? sqlite3_errstr(code) : sqlite3_errmsg(db));
}

static value riot_sqlite_stmt_error(sqlite3_stmt *stmt, int code) {
  sqlite3 *db = stmt == NULL ? NULL : sqlite3_db_handle(stmt);
  return riot_sqlite_db_error(db, code);
}

static sqlite3 *riot_sqlite_db_val(value value_db) {
  riot_sqlite_db *db = (riot_sqlite_db *)Data_custom_val(value_db);
  return db->db;
}

static sqlite3_stmt *riot_sqlite_stmt_val(value value_stmt) {
  riot_sqlite_stmt *stmt = (riot_sqlite_stmt *)Data_custom_val(value_stmt);
  return stmt->stmt;
}

CAMLprim value riot_sqlite_flag_readonly(value unit_value) {
  (void)unit_value;
  return Val_int(SQLITE_OPEN_READONLY);
}

CAMLprim value riot_sqlite_flag_readwrite(value unit_value) {
  (void)unit_value;
  return Val_int(SQLITE_OPEN_READWRITE);
}

CAMLprim value riot_sqlite_flag_create(value unit_value) {
  (void)unit_value;
  return Val_int(SQLITE_OPEN_CREATE);
}

CAMLprim value riot_sqlite_flag_uri(value unit_value) {
  (void)unit_value;
  return Val_int(SQLITE_OPEN_URI);
}

CAMLprim value riot_sqlite_row_code(value unit_value) {
  (void)unit_value;
  return Val_int(SQLITE_ROW);
}

CAMLprim value riot_sqlite_done_code(value unit_value) {
  (void)unit_value;
  return Val_int(SQLITE_DONE);
}

CAMLprim value riot_sqlite_open(value path_value, value flags_value) {
  CAMLparam2(path_value, flags_value);
  CAMLlocal2(block, result);
  sqlite3 *db = NULL;
  int rc = sqlite3_open_v2(String_val(path_value), &db, Int_val(flags_value), NULL);
  if (rc != SQLITE_OK) {
    result = riot_sqlite_db_error(db, rc);
    if (db != NULL) {
      (void)sqlite3_close_v2(db);
    }
    CAMLreturn(result);
  }

  block = caml_alloc_custom(&riot_sqlite_db_ops, sizeof(riot_sqlite_db), 0, 1);
  ((riot_sqlite_db *)Data_custom_val(block))->db = db;
  CAMLreturn(riot_sqlite_ok(block));
}

CAMLprim value riot_sqlite_close(value db_value) {
  CAMLparam1(db_value);
  sqlite3 *db = riot_sqlite_db_val(db_value);
  if (db == NULL) {
    CAMLreturn(riot_sqlite_ok(Val_unit));
  }

  int rc = sqlite3_close_v2(db);
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_db_error(db, rc));
  }

  ((riot_sqlite_db *)Data_custom_val(db_value))->db = NULL;
  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_busy_timeout(value db_value, value millis_value) {
  CAMLparam2(db_value, millis_value);
  sqlite3 *db = riot_sqlite_db_val(db_value);
  if (db == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite connection is closed"));
  }

  int rc = sqlite3_busy_timeout(db, Int_val(millis_value));
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_db_error(db, rc));
  }

  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_prepare(value db_value, value sql_value) {
  CAMLparam2(db_value, sql_value);
  CAMLlocal2(block, result);
  sqlite3 *db = riot_sqlite_db_val(db_value);
  sqlite3_stmt *stmt = NULL;
  if (db == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite connection is closed"));
  }

  int rc = sqlite3_prepare_v2(
    db,
    String_val(sql_value),
    (int)caml_string_length(sql_value),
    &stmt,
    NULL);
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_db_error(db, rc));
  }

  block = caml_alloc_custom(&riot_sqlite_stmt_ops, sizeof(riot_sqlite_stmt), 0, 1);
  ((riot_sqlite_stmt *)Data_custom_val(block))->stmt = stmt;
  result = riot_sqlite_ok(block);
  CAMLreturn(result);
}

CAMLprim value riot_sqlite_finalize(value stmt_value) {
  CAMLparam1(stmt_value);
  riot_sqlite_stmt *stmt_block = (riot_sqlite_stmt *)Data_custom_val(stmt_value);
  sqlite3_stmt *stmt = stmt_block->stmt;
  sqlite3 *db;
  if (stmt == NULL) {
    CAMLreturn(riot_sqlite_ok(Val_unit));
  }

  db = sqlite3_db_handle(stmt);
  int rc = sqlite3_finalize(stmt);
  stmt_block->stmt = NULL;
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_db_error(db, rc));
  }

  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_reset(value stmt_value) {
  CAMLparam1(stmt_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite statement is finalized"));
  }

  int rc = sqlite3_reset(stmt);
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_stmt_error(stmt, rc));
  }

  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_clear_bindings(value stmt_value) {
  CAMLparam1(stmt_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite statement is finalized"));
  }

  int rc = sqlite3_clear_bindings(stmt);
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_stmt_error(stmt, rc));
  }

  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_bind_parameter_count(value stmt_value) {
  CAMLparam1(stmt_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    caml_invalid_argument("SQLite statement is finalized");
  }
  CAMLreturn(Val_int(sqlite3_bind_parameter_count(stmt)));
}

CAMLprim value riot_sqlite_bind_null(value stmt_value, value index_value) {
  CAMLparam2(stmt_value, index_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite statement is finalized"));
  }
  int rc = sqlite3_bind_null(stmt, Int_val(index_value));
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_stmt_error(stmt, rc));
  }
  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_bind_int64(value stmt_value, value index_value, value int_value) {
  CAMLparam3(stmt_value, index_value, int_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite statement is finalized"));
  }
  int rc = sqlite3_bind_int64(stmt, Int_val(index_value), Int64_val(int_value));
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_stmt_error(stmt, rc));
  }
  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_bind_double(value stmt_value, value index_value, value float_value) {
  CAMLparam3(stmt_value, index_value, float_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite statement is finalized"));
  }
  int rc = sqlite3_bind_double(stmt, Int_val(index_value), Double_val(float_value));
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_stmt_error(stmt, rc));
  }
  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_bind_text(value stmt_value, value index_value, value text_value) {
  CAMLparam3(stmt_value, index_value, text_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite statement is finalized"));
  }
  int rc = sqlite3_bind_text(
    stmt,
    Int_val(index_value),
    String_val(text_value),
    (int)caml_string_length(text_value),
    SQLITE_TRANSIENT);
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_stmt_error(stmt, rc));
  }
  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_bind_blob(value stmt_value, value index_value, value blob_value) {
  CAMLparam3(stmt_value, index_value, blob_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite statement is finalized"));
  }
  int rc = sqlite3_bind_blob(
    stmt,
    Int_val(index_value),
    Bytes_val(blob_value),
    (int)caml_string_length(blob_value),
    SQLITE_TRANSIENT);
  if (rc != SQLITE_OK) {
    CAMLreturn(riot_sqlite_stmt_error(stmt, rc));
  }
  CAMLreturn(riot_sqlite_ok(Val_unit));
}

CAMLprim value riot_sqlite_step(value stmt_value) {
  CAMLparam1(stmt_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    CAMLreturn(riot_sqlite_error(SQLITE_MISUSE, "SQLite statement is finalized"));
  }

  int rc = sqlite3_step(stmt);
  if (rc == SQLITE_ROW || rc == SQLITE_DONE) {
    CAMLreturn(riot_sqlite_ok(Val_int(rc)));
  }

  CAMLreturn(riot_sqlite_stmt_error(stmt, rc));
}

CAMLprim value riot_sqlite_column_count(value stmt_value) {
  CAMLparam1(stmt_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    caml_invalid_argument("SQLite statement is finalized");
  }
  CAMLreturn(Val_int(sqlite3_column_count(stmt)));
}

CAMLprim value riot_sqlite_column_name(value stmt_value, value index_value) {
  CAMLparam2(stmt_value, index_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  const char *name;
  if (stmt == NULL) {
    caml_invalid_argument("SQLite statement is finalized");
  }
  name = sqlite3_column_name(stmt, Int_val(index_value));
  CAMLreturn(caml_copy_string(name == NULL ? "" : name));
}

CAMLprim value riot_sqlite_column_type(value stmt_value, value index_value) {
  CAMLparam2(stmt_value, index_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    caml_invalid_argument("SQLite statement is finalized");
  }
  CAMLreturn(Val_int(sqlite3_column_type(stmt, Int_val(index_value))));
}

CAMLprim value riot_sqlite_column_int64(value stmt_value, value index_value) {
  CAMLparam2(stmt_value, index_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    caml_invalid_argument("SQLite statement is finalized");
  }
  CAMLreturn(caml_copy_int64(sqlite3_column_int64(stmt, Int_val(index_value))));
}

CAMLprim value riot_sqlite_column_double(value stmt_value, value index_value) {
  CAMLparam2(stmt_value, index_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    caml_invalid_argument("SQLite statement is finalized");
  }
  CAMLreturn(caml_copy_double(sqlite3_column_double(stmt, Int_val(index_value))));
}

CAMLprim value riot_sqlite_column_text(value stmt_value, value index_value) {
  CAMLparam2(stmt_value, index_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  const unsigned char *text;
  int len;
  if (stmt == NULL) {
    caml_invalid_argument("SQLite statement is finalized");
  }
  text = sqlite3_column_text(stmt, Int_val(index_value));
  len = sqlite3_column_bytes(stmt, Int_val(index_value));
  CAMLreturn(caml_alloc_initialized_string(len, text == NULL ? "" : (const char *)text));
}

CAMLprim value riot_sqlite_column_blob(value stmt_value, value index_value) {
  CAMLparam2(stmt_value, index_value);
  CAMLlocal1(bytes);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  const void *blob;
  int len;
  if (stmt == NULL) {
    caml_invalid_argument("SQLite statement is finalized");
  }
  blob = sqlite3_column_blob(stmt, Int_val(index_value));
  len = sqlite3_column_bytes(stmt, Int_val(index_value));
  bytes = caml_alloc_string(len);
  if (len > 0 && blob != NULL) {
    memcpy(Bytes_val(bytes), blob, (size_t)len);
  }
  CAMLreturn(bytes);
}

CAMLprim value riot_sqlite_changes(value db_value) {
  CAMLparam1(db_value);
  sqlite3 *db = riot_sqlite_db_val(db_value);
  if (db == NULL) {
    caml_invalid_argument("SQLite connection is closed");
  }
  CAMLreturn(Val_int(sqlite3_changes(db)));
}

CAMLprim value riot_sqlite_stmt_readonly(value stmt_value) {
  CAMLparam1(stmt_value);
  sqlite3_stmt *stmt = riot_sqlite_stmt_val(stmt_value);
  if (stmt == NULL) {
    caml_invalid_argument("SQLite statement is finalized");
  }
  CAMLreturn(Val_bool(sqlite3_stmt_readonly(stmt) != 0));
}
