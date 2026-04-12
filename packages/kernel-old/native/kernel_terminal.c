/* Terminal size detection using ioctl(TIOCGWINSZ) */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifdef _WIN32
  /* Windows doesn't support ioctl - use GetConsoleScreenBufferInfo instead */
  #include <windows.h>
  
  CAMLprim value caml_get_terminal_size(value fd_val) {
    CAMLparam1(fd_val);
    CAMLlocal1(result);
    
    HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    
    if (GetConsoleScreenBufferInfo(hConsole, &csbi)) {
      int cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
      int rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
      
      result = caml_alloc_tuple(2);
      Store_field(result, 0, Val_int(cols));
      Store_field(result, 1, Val_int(rows));
      CAMLreturn(result);
    } else {
      caml_failwith("Failed to get console size");
    }
  }
#else
  /* Unix-like systems: use ioctl with TIOCGWINSZ */
  #include <sys/ioctl.h>
  #include <unistd.h>
  #include <termios.h>
  
  CAMLprim value caml_get_terminal_size(value fd_val) {
    CAMLparam1(fd_val);
    CAMLlocal1(result);
    
    int fd = Int_val(fd_val);
    struct winsize ws;
    
    if (ioctl(fd, TIOCGWINSZ, &ws) == -1) {
      caml_failwith("ioctl TIOCGWINSZ failed");
    }
    
    /* Return tuple (cols, rows) */
    result = caml_alloc_tuple(2);
    Store_field(result, 0, Val_int(ws.ws_col));
    Store_field(result, 1, Val_int(ws.ws_row));
    
    CAMLreturn(result);
  }
#endif
