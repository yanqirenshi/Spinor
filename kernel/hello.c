#include <stdio.h>
#include <stdbool.h>
#include "spinor.h"


SpObject* user_sum_list(SpObject* user_xs) {
    return (sp_is_nil(user_xs)->value.boolean ? sp_make_int(0) : sp_add(sp_car(user_xs), user_sum_list(sp_cdr(user_xs))));
}


int main(void) {
    sp_print(sp_make_str("Spinor on bare metal!"));
    sp_print(sp_add(sp_make_int(40), sp_make_int(2)));
    sp_print(sp_cons(sp_make_int(10), sp_cons(sp_make_int(20), sp_cons(sp_make_int(30), sp_cons(sp_make_int(12), sp_make_nil())))));
    sp_print(user_sum_list(sp_cons(sp_make_int(10), sp_cons(sp_make_int(20), sp_cons(sp_make_int(30), sp_cons(sp_make_int(12), sp_make_nil()))))));
    sp_print(sp_str_append(sp_make_str("Linear "), sp_make_str("Spinor")));

    return 0;
}
