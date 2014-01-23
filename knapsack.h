#ifndef KNAPSACK_H
#define KNAPSACK_H

typedef struct selection {
	int position;
	int quantity;
	UT_hash_handle hh;
} selection;

typedef struct solution {
	int total_value;
	int total_weight;
	selection** sol_selection;
} solution;

solution    knapsack          (int, size_t, int[], int[]);
void        read_file         (char*, int*, int*, int[], int[]);
int         scale_by_gcd      (int*, size_t, int[]);

void        increment_position(selection**, int);
selection** empty_selection   ();
void        cleanup_selection (selection**);

#endif