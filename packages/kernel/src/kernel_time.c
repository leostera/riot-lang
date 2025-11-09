#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <sys/time.h>
#include <time.h>
#include <stdint.h>

#ifdef __APPLE__
#include <mach/mach_time.h>
#endif

/* Get current time with microsecond precision */
CAMLprim value caml_kernel_gettimeofday(value unit) {
  CAMLparam1(unit);
  struct timeval tv;
  gettimeofday(&tv, NULL);
  double result = (double)tv.tv_sec + (double)tv.tv_usec / 1000000.0;
  CAMLreturn(caml_copy_double(result));
}

/* Convert time_t to broken-down time (local timezone) */
CAMLprim value caml_kernel_localtime(value unix_time) {
  CAMLparam1(unix_time);
  CAMLlocal1(tm_val);
  
  time_t t = (time_t)Double_val(unix_time);
  struct tm *tm = localtime(&t);
  
  tm_val = caml_alloc(9, 0);
  Store_field(tm_val, 0, Val_int(tm->tm_sec));
  Store_field(tm_val, 1, Val_int(tm->tm_min));
  Store_field(tm_val, 2, Val_int(tm->tm_hour));
  Store_field(tm_val, 3, Val_int(tm->tm_mday));
  Store_field(tm_val, 4, Val_int(tm->tm_mon));
  Store_field(tm_val, 5, Val_int(tm->tm_year));
  Store_field(tm_val, 6, Val_int(tm->tm_wday));
  Store_field(tm_val, 7, Val_int(tm->tm_yday));
  Store_field(tm_val, 8, Val_bool(tm->tm_isdst > 0));
  
  CAMLreturn(tm_val);
}

/* Convert time_t to broken-down time (UTC) */
CAMLprim value caml_kernel_gmtime(value unix_time) {
  CAMLparam1(unix_time);
  CAMLlocal1(tm_val);
  
  time_t t = (time_t)Double_val(unix_time);
  struct tm *tm = gmtime(&t);
  
  tm_val = caml_alloc(9, 0);
  Store_field(tm_val, 0, Val_int(tm->tm_sec));
  Store_field(tm_val, 1, Val_int(tm->tm_min));
  Store_field(tm_val, 2, Val_int(tm->tm_hour));
  Store_field(tm_val, 3, Val_int(tm->tm_mday));
  Store_field(tm_val, 4, Val_int(tm->tm_mon));
  Store_field(tm_val, 5, Val_int(tm->tm_year));
  Store_field(tm_val, 6, Val_int(tm->tm_wday));
  Store_field(tm_val, 7, Val_int(tm->tm_yday));
  Store_field(tm_val, 8, Val_bool(tm->tm_isdst > 0));
  
  CAMLreturn(tm_val);
}

/* Convert broken-down time to time_t */
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
  
  time_t t = mktime(&tm);
  
  /* Create normalized tm value */
  normalized_tm = caml_alloc(9, 0);
  Store_field(normalized_tm, 0, Val_int(tm.tm_sec));
  Store_field(normalized_tm, 1, Val_int(tm.tm_min));
  Store_field(normalized_tm, 2, Val_int(tm.tm_hour));
  Store_field(normalized_tm, 3, Val_int(tm.tm_mday));
  Store_field(normalized_tm, 4, Val_int(tm.tm_mon));
  Store_field(normalized_tm, 5, Val_int(tm.tm_year));
  Store_field(normalized_tm, 6, Val_int(tm.tm_wday));
  Store_field(normalized_tm, 7, Val_int(tm.tm_yday));
  Store_field(normalized_tm, 8, Val_bool(tm.tm_isdst > 0));
  
  /* Return tuple (float * tm) */
  result = caml_alloc(2, 0);
  Store_field(result, 0, caml_copy_double((double)t));
  Store_field(result, 1, normalized_tm);
  
  CAMLreturn(result);
}

/* Get monotonic time in nanoseconds - immune to system clock adjustments */
CAMLprim value caml_kernel_monotonic_time_nanos(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  
  #ifdef __APPLE__
    /* macOS: use mach_absolute_time */
    static mach_timebase_info_data_t timebase = {0, 0};
    if (timebase.denom == 0) {
      mach_timebase_info(&timebase);
    }
    uint64_t abs_time = mach_absolute_time();
    uint64_t nanos = abs_time * timebase.numer / timebase.denom;
    result = caml_copy_int64(nanos);
    
  #elif defined(__linux__)
    /* Linux: use CLOCK_MONOTONIC */
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    uint64_t nanos = (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
    result = caml_copy_int64(nanos);
    
  #else
    /* Fallback: use gettimeofday (not truly monotonic but portable) */
    struct timeval tv;
    gettimeofday(&tv, NULL);
    uint64_t nanos = (uint64_t)tv.tv_sec * 1000000000ULL + 
                     (uint64_t)tv.tv_usec * 1000ULL;
    result = caml_copy_int64(nanos);
  #endif
  
  CAMLreturn(result);
}
