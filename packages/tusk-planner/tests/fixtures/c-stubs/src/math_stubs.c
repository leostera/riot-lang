#include <caml/mlvalues.h>

value caml_add_native(value a, value b) {
    return Val_int(Int_val(a) + Int_val(b));
}
