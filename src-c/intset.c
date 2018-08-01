
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>


typedef struct int_ll_t {
    uint64_t value;
    struct int_ll_t *next;
} int_ll_t;

typedef struct int_set_t {
    uint64_t min_bound;
    uint64_t max_bound;
    uint64_t *in_bounds;
    size_t num_in_bounds;
    struct int_ll_t **out_bounds;
    size_t out_bounds_size;
} int_set_t;

int_set_t *new_int_set(uint64_t min_bound, uint64_t max_bound, size_t out_bounds_size) {
    size_t num_in_bounds = (size_t)(max_bound - min_bound) / 64 + 1;

    int_set_t *set = (int_set_t *)calloc(1, sizeof(int_set_t));
    set->min_bound = min_bound;
    set->max_bound = max_bound;
    set->in_bounds = (uint64_t *)calloc(num_in_bounds, sizeof(uint64_t));
    set->num_in_bounds = num_in_bounds;
    set->out_bounds = (int_ll_t **)calloc(out_bounds_size, sizeof(int_ll_t*));
    set->out_bounds_size = out_bounds_size;

    return set;
}

void free_int_set(int_set_t *set) {
    for(size_t i = 0; i < set->out_bounds_size; i++) {
        int_ll_t *ll = set->out_bounds[i];
        while(ll != NULL) {
            int_ll_t *ll_next = ll->next;
            free(ll);
            ll = ll_next;
        }
    }
    free(set->in_bounds);
    free(set->out_bounds);
    free(set);
}

void _int_set_add_in_bounds(int_set_t *set, uint64_t n) {
    uint64_t o = n / 64;
    uint64_t i = n % 64;
    uint64_t mask = (uint64_t)1 << i;
    set->in_bounds[o] |= mask;
}

void _int_set_add_out_bounds(int_set_t *set, uint64_t n) {
    uint64_t i = n % set->out_bounds_size;
    int_ll_t **ll = &(set->out_bounds[i]);
    while(*ll != NULL) {
        if((*ll)->value == n) {
            break;
        }
        ll = &((*ll)->next);
    }
    *ll = (int_ll_t *)malloc(sizeof(int_ll_t));
    (*ll)->value = n;
    (*ll)->next = NULL;
}

void int_set_add(int_set_t *set, uint64_t n) {
    if(n >= set->min_bound && n <= set->max_bound) {
        _int_set_add_in_bounds(set, n - set->min_bound);
    } else {
        _int_set_add_out_bounds(set, n);
    }
}

bool _int_set_check_in_bounds(int_set_t *set, uint64_t n) {
    uint64_t o = n / 64;
    uint64_t i = n % 64;
    uint64_t mask = (uint64_t)1 << i;
    return ((set->in_bounds[o] & mask) != 0);
}

bool _int_set_check_out_bounds(int_set_t *set, uint64_t n) {
    uint64_t i = n % set->out_bounds_size;
    int_ll_t *ll = set->out_bounds[i];
    while(ll != NULL) {
        if(ll->value == n) {
            return true;
        }
        ll = ll->next;
    }
    return false;
}

bool int_set_check(int_set_t *set, uint64_t n) {
    if(n >= set->min_bound && n <= set->max_bound) {
        return _int_set_check_in_bounds(set, n - set->min_bound);
    } else {
        return _int_set_check_out_bounds(set, n);
    }
}
