#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <sys/time.h>
#include <time.h>

static value copy_tm(struct tm *tm) {
  value tm_val = caml_alloc(9, 0);
  Store_field(tm_val, 0, Val_int(tm->tm_sec));
  Store_field(tm_val, 1, Val_int(tm->tm_min));
  Store_field(tm_val, 2, Val_int(tm->tm_hour));
  Store_field(tm_val, 3, Val_int(tm->tm_mday));
  Store_field(tm_val, 4, Val_int(tm->tm_mon));
  Store_field(tm_val, 5, Val_int(tm->tm_year));
  Store_field(tm_val, 6, Val_int(tm->tm_wday));
  Store_field(tm_val, 7, Val_int(tm->tm_yday));
  Store_field(tm_val, 8, Val_bool(tm->tm_isdst > 0));
  return tm_val;
}

CAMLprim value caml_kernel_gettimeofday(value unit) {
  CAMLparam1(unit);
  struct timeval tv;
  gettimeofday(&tv, NULL);
  CAMLreturn(caml_copy_double((double)tv.tv_sec + ((double)tv.tv_usec / 1000000.0)));
}

CAMLprim value caml_kernel_localtime(value unix_time) {
  CAMLparam1(unix_time);
  CAMLlocal1(tm_val);
  time_t seconds = (time_t)Double_val(unix_time);
  struct tm tm_storage;
  struct tm *tm = localtime_r(&seconds, &tm_storage);

  if (tm == NULL) {
    tm_storage = (struct tm){0};
    tm = &tm_storage;
  }

  tm_val = copy_tm(tm);
  CAMLreturn(tm_val);
}

CAMLprim value caml_kernel_gmtime(value unix_time) {
  CAMLparam1(unix_time);
  CAMLlocal1(tm_val);
  time_t seconds = (time_t)Double_val(unix_time);
  struct tm tm_storage;
  struct tm *tm = gmtime_r(&seconds, &tm_storage);

  if (tm == NULL) {
    tm_storage = (struct tm){0};
    tm = &tm_storage;
  }

  tm_val = copy_tm(tm);
  CAMLreturn(tm_val);
}

CAMLprim value caml_kernel_mktime(value tm_val) {
  CAMLparam1(tm_val);
  CAMLlocal2(result, normalized_tm);
  struct tm tm;

  tm.tm_sec = Int_val(Field(tm_val, 0));
  tm.tm_min = Int_val(Field(tm_val, 1));
  tm.tm_hour = Int_val(Field(tm_val, 2));
  tm.tm_mday = Int_val(Field(tm_val, 3));
  tm.tm_mon = Int_val(Field(tm_val, 4));
  tm.tm_year = Int_val(Field(tm_val, 5));
  tm.tm_wday = Int_val(Field(tm_val, 6));
  tm.tm_yday = Int_val(Field(tm_val, 7));
  tm.tm_isdst = Bool_val(Field(tm_val, 8)) ? 1 : 0;

  time_t seconds = mktime(&tm);

  normalized_tm = copy_tm(&tm);
  result = caml_alloc(2, 0);
  Store_field(result, 0, caml_copy_double((double)seconds));
  Store_field(result, 1, normalized_tm);
  CAMLreturn(result);
}
