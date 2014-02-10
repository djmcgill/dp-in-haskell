#ifndef KNAPSACK_H
#define KNAPSACK_H

typedef struct index {
    int payload;
    struct index *next;
} index;

typedef struct vw_t {
	int value;
	int weight;
	int original_ix;
} vw_t;

typedef struct selection {
	int position;
	int quantity;
	UT_hash_handle hh;
} selection;

typedef struct solution {
	vw_t total_vw;
	index *selection_list;
} solution;

typedef struct collated_solution {
	vw_t total_vw;
	selection **selection_hashmap;
} collated_solution;

collated_solution knapsack          (int, size_t, vw_t[]);
void              read_file         (const char*, int*, int*, vw_t**);
int               scale_by_gcd      (int*, size_t, vw_t[]);
int               cmp_vws           (const void*, const void*);
int               cmp_weight        (const void*, const void*);

void              increment_position(selection**, int);
selection**       empty_selection   ();
void              cleanup_selection (selection**);

#endif