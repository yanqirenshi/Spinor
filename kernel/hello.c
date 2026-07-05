#include <stdio.h>
#include <stdbool.h>
#include "spinor.h"


SpObject* user_sum_list(SpObject* user_xs) {
    return (sp_is_nil(user_xs)->value.boolean ? sp_make_int(0) : sp_add(sp_car(user_xs), user_sum_list(sp_cdr(user_xs))));
}

SpObject* user_churn(SpObject* user_n) {
    while(1) {
        if (sp_eq(user_n, sp_make_int(0))->value.boolean) {
            return sp_make_str("drop churn: done");
        } else {
        if (sp_is_nil(({ sp_free(sp_cons(sp_make_int(10), sp_cons(sp_make_int(20), sp_cons(sp_make_int(30), sp_cons(sp_make_int(12), sp_make_nil()))))); sp_make_nil(); }))->value.boolean) {
            SpObject* _tco_tmp_0 = sp_sub(user_n, sp_make_int(1));
            user_n = _tco_tmp_0;
            continue;
        } else {
            return sp_make_str("drop churn: unreachable");
        }

        }

    }
}


int main(void) {
    sp_print(sp_make_str("Spinor on bare metal!"));
    sp_print(sp_add(sp_make_int(40), sp_make_int(2)));
    sp_print(sp_cons(sp_make_int(10), sp_cons(sp_make_int(20), sp_cons(sp_make_int(30), sp_cons(sp_make_int(12), sp_make_nil())))));
    sp_print(user_sum_list(sp_cons(sp_make_int(10), sp_cons(sp_make_int(20), sp_cons(sp_make_int(30), sp_cons(sp_make_int(12), sp_make_nil()))))));
    sp_print(sp_str_append(sp_make_str("Linear "), sp_make_str("Spinor")));
    sp_print(sp_make_str("drop test: alloc+drop x 10000 (approx 6MB through 4MiB heap)"));
    sp_print(user_churn(sp_make_int(10000)));
    sp_print(user_sum_list(sp_cons(sp_make_int(1), sp_cons(sp_make_int(2), sp_cons(sp_make_int(3), sp_make_nil())))));
    sp_print(sp_make_str("drop test: allocation after drop works"));

    return 0;
}
