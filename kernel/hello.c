#include <stdio.h>
#include <stdbool.h>
#include "spinor.h"


SpObject* user_sum_list(SpObject* user_xs) {
    SpObject* _t0 = sp_is_nil(user_xs);
    int _c1 = _t0->value.boolean;
    SpObject* _t2 = NULL;
    if (_c1) {
    SpObject* _t3 = sp_make_int(0);
    _t2 = _t3;
    } else {
    SpObject* _t4 = sp_car(user_xs);
    SpObject* _t5 = sp_cdr(user_xs);
    SpObject* _t6 = user_sum_list(_t5);
    SpObject* _t7 = sp_add(_t4, _t6);
    _t2 = _t7;
    (void)_t4;
    (void)_t5;
    (void)_t6;
    }
    sp_free(_t0);
    return _t2;
}

SpObject* user_churn(SpObject* user_n) {
    bool _owned_user_n = false;
    while (1) {
    SpObject* _t0 = sp_make_int(0);
    SpObject* _t1 = sp_eq(user_n, _t0);
    int _c2 = _t1->value.boolean;
    sp_free(_t0);
    sp_free(_t1);
    if (_c2) {
    SpObject* _t3 = sp_make_str("auto-drop churn: done");
    if (_owned_user_n) sp_free(user_n);
    return _t3;
    } else {
    SpObject* _t4 = sp_make_int(10);
    SpObject* _t5 = sp_make_int(20);
    SpObject* _t6 = sp_make_int(30);
    SpObject* _t7 = sp_make_int(12);
    SpObject* _t8 = sp_make_nil();
    SpObject* _t9 = sp_cons(_t7, _t8);
    SpObject* _t10 = sp_cons(_t6, _t9);
    SpObject* _t11 = sp_cons(_t5, _t10);
    SpObject* _t12 = sp_cons(_t4, _t11);
    SpObject* _t13 = sp_is_nil(_t12);
    int _c14 = _t13->value.boolean;
    sp_free(_t12);
    sp_free(_t13);
    if (_c14) {
    SpObject* _t15 = sp_make_str("auto-drop churn: unreachable");
    if (_owned_user_n) sp_free(user_n);
    return _t15;
    } else {
    SpObject* _t16 = sp_make_int(1);
    SpObject* _t17 = sp_sub(user_n, _t16);
    if (_owned_user_n) sp_free(user_n);
    user_n = _t17;
    _owned_user_n = true;
    sp_free(_t16);
    continue;
    }
    }
    }
}

SpObject* user_drop_demo(void) {
    SpObject* _t0 = sp_make_int(1);
    SpObject* _t1 = sp_make_int(2);
    SpObject* _t2 = sp_make_int(3);
    SpObject* _t3 = sp_make_nil();
    SpObject* _t4 = sp_cons(_t2, _t3);
    SpObject* _t5 = sp_cons(_t1, _t4);
    SpObject* _t6 = sp_cons(_t0, _t5);
    sp_free(_t6);
    SpObject* _t7 = sp_make_nil();
    SpObject* _t8 = sp_is_nil(_t7);
    int _c9 = _t8->value.boolean;
    SpObject* _t10 = NULL;
    if (_c9) {
    SpObject* _t11 = sp_make_str("explicit drop still works");
    _t10 = _t11;
    } else {
    SpObject* _t12 = sp_make_str("unreachable");
    _t10 = _t12;
    }
    sp_free(_t7);
    sp_free(_t8);
    return _t10;
}


int main(void) {
    {
        SpObject* _t0 = sp_make_str("Spinor on bare metal!");
        SpObject* _t1 = sp_print(_t0);
        (void)_t1;
        sp_free(_t0);
        (void)_t1;
    }

    {
        SpObject* _t0 = sp_make_int(40);
        SpObject* _t1 = sp_make_int(2);
        SpObject* _t2 = sp_add(_t0, _t1);
        SpObject* _t3 = sp_print(_t2);
        (void)_t3;
        sp_free(_t0);
        sp_free(_t1);
        sp_free(_t2);
        (void)_t3;
    }

    {
        SpObject* _t0 = sp_make_int(10);
        SpObject* _t1 = sp_make_int(20);
        SpObject* _t2 = sp_make_int(30);
        SpObject* _t3 = sp_make_int(12);
        SpObject* _t4 = sp_make_nil();
        SpObject* _t5 = sp_cons(_t3, _t4);
        SpObject* _t6 = sp_cons(_t2, _t5);
        SpObject* _t7 = sp_cons(_t1, _t6);
        SpObject* _t8 = sp_cons(_t0, _t7);
        SpObject* _t9 = sp_print(_t8);
        (void)_t9;
        sp_free(_t8);
        (void)_t9;
    }

    {
        SpObject* _t0 = sp_make_int(10);
        SpObject* _t1 = sp_make_int(20);
        SpObject* _t2 = sp_make_int(30);
        SpObject* _t3 = sp_make_int(12);
        SpObject* _t4 = sp_make_nil();
        SpObject* _t5 = sp_cons(_t3, _t4);
        SpObject* _t6 = sp_cons(_t2, _t5);
        SpObject* _t7 = sp_cons(_t1, _t6);
        SpObject* _t8 = sp_cons(_t0, _t7);
        SpObject* _t9 = user_sum_list(_t8);
        SpObject* _t10 = sp_print(_t9);
        (void)_t10;
        sp_free(_t8);
        (void)_t9;
        (void)_t10;
    }

    {
        SpObject* _t0 = sp_make_str("Linear ");
        SpObject* _t1 = sp_make_str("Spinor");
        SpObject* _t2 = sp_str_append(_t0, _t1);
        SpObject* _t3 = sp_print(_t2);
        (void)_t3;
        sp_free(_t0);
        sp_free(_t1);
        sp_free(_t2);
        (void)_t3;
    }

    {
        SpObject* _t0 = sp_make_str("auto-drop test: alloc x 100000 with NO explicit drop (approx 60MB through 4MiB heap)");
        SpObject* _t1 = sp_print(_t0);
        (void)_t1;
        sp_free(_t0);
        (void)_t1;
    }

    {
        SpObject* _t0 = sp_make_int(100000);
        SpObject* _t1 = user_churn(_t0);
        SpObject* _t2 = sp_print(_t1);
        (void)_t2;
        sp_free(_t0);
        (void)_t1;
        (void)_t2;
    }

    {
        SpObject* _t0 = user_drop_demo();
        SpObject* _t1 = sp_print(_t0);
        (void)_t1;
        (void)_t0;
        (void)_t1;
    }

    {
        SpObject* _t0 = sp_make_int(1);
        SpObject* _t1 = sp_make_int(2);
        SpObject* _t2 = sp_make_int(3);
        SpObject* _t3 = sp_make_nil();
        SpObject* _t4 = sp_cons(_t2, _t3);
        SpObject* _t5 = sp_cons(_t1, _t4);
        SpObject* _t6 = sp_cons(_t0, _t5);
        SpObject* _t7 = user_sum_list(_t6);
        SpObject* _t8 = sp_print(_t7);
        (void)_t8;
        sp_free(_t6);
        (void)_t7;
        (void)_t8;
    }

    {
        SpObject* _t0 = sp_make_str("auto-drop test: allocation after churn works");
        SpObject* _t1 = sp_print(_t0);
        (void)_t1;
        sp_free(_t0);
        (void)_t1;
    }


    return 0;
}
