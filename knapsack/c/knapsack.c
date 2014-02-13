#include<stdio.h>

#define NDEBUG
#include<assert.h>

#include "uthash-master/src/uthash.h"
#include "uthash-master/src/utlist.h"
#include "gcd.c"
#include "knapsack.h"

#include <stdlib.h>

int main () {
	int cap = 0, n = 0;
	vw_t *vws = NULL;
	collated_solution bestAns;
	char* FILE_NAME = "test_problem_1.data";

	read_file(FILE_NAME, &cap, &n, &vws);
	qsort (vws, n, sizeof(vw_t), cmp_weight);

	bestAns = knapsack (cap, n, vws);
	free (vws);

	// print answers
	printf ("\nThe C solution is has a total weight of %i, a total value of %i and a selection of:\n",
		bestAns.total_vw.weight, bestAns.total_vw.value);

	selection *current_selection, *tmp;
	HASH_ITER(hh, *(bestAns.selection_hashmap), current_selection, tmp) {
		printf ("\tindex: %i, quantity: %i\n", current_selection->position, current_selection->quantity);
	}
	// free bestAns.selection_hashmap
	HASH_ITER(hh, *(bestAns.selection_hashmap), current_selection, tmp) {
		HASH_DEL(*(bestAns.selection_hashmap), current_selection);
		free (current_selection);
	}
	free (bestAns.selection_hashmap);
	return 0;
}

void read_file (const char* const file_name,
	            int* restrict pCap,
	            int* restrict pN,
	            vw_t** restrict pVWs) {
	FILE *pFile;
	int i = 0;
	pFile = fopen ("test_problem_1.data","r");
	fscanf (pFile, "%i %i\n", pCap, pN);
	assert (*pN > 0 && *pCap >= 0);

	int n = *pN;
	assert (n >= 0);
	*pVWs = malloc (n * sizeof(vw_t));
	for (i = 0; i < n; i++) {
		vw_t temp_vw;
		// TODO: insert directly into pVW
		fscanf (pFile, "%i %i\n", &(temp_vw.value), &(temp_vw.weight));
		temp_vw.original_ix = i;
		assert (temp_vw.value >= 0 && temp_vw.weight > 0);
		(*pVWs)[i] = temp_vw;
	}
	fclose(pFile);
}

collated_solution knapsack(int cap, size_t n,
                  vw_t vws[restrict static n]) {
	int i = 0;
	int j = 0;

	int gcd_w = scale_by_gcd(&cap, n, vws);

	// make an array of size (cap+1) where the ith element is the best solution for a weight of i
	solution* solutions = malloc((cap+1)*sizeof(solution));
	solutions[0] = (solution){0,0,-1,NULL};

	vw_t best_vw;
	int best_position;

	for (i = 1; i <= cap; i++) {
		best_vw = (vw_t){0,0,-1};
		best_position = -1;

		// for each value/weight, try to add it to best_vw
		for (j = 0; j < n; j++) {
			int value = vws[j].value;
			int weight = vws[j].weight;

			// if the weight doesn't fit then we can't do anything
			// since all the weights after this are bigger than the
			// current weight we can't use those either
			if (weight > i) {break;}

			// let's see what the new solution would look like
			int prospective_position = i - weight;
			vw_t prospective_vw;
			prospective_vw.value = value + solutions[prospective_position].total_vw.value;
			prospective_vw.weight = weight + solutions[prospective_position].total_vw.weight;
			prospective_vw.original_ix = vws[j].original_ix;
			assert (prospective_vw.weight <= i);

			// update the best value if needed
			if (cmp_vws (&prospective_vw, &best_vw) >= 1) {
				best_vw = prospective_vw;
				best_position = prospective_position;
			}
		}

		// update solutions[i]
		solutions[i].total_vw = best_vw;
		if (best_position != -1) {
			// found a solution
			solutions[i].selection_list = solutions[best_position].selection_list;
			index *new_index = malloc (sizeof(index));
			*new_index = (index){best_vw.original_ix, NULL};
			LL_PREPEND(solutions[i].selection_list, new_index);
		} else {
			// no solutions for this i
			solutions[i].selection_list = NULL;
		}
	}

	// prepare the solution to be returned
	collated_solution bestAns;
	bestAns.total_vw = solutions[cap].total_vw;
	bestAns.total_vw.weight *= gcd_w;

	bestAns.selection_hashmap = empty_selection();
	index *tmp_index;
	LL_FOREACH (solutions[cap].selection_list, tmp_index) {
		increment_position (bestAns.selection_hashmap, tmp_index->payload);
	}

	// free each of the solutions [0,cap]
	for (i = 0; i <= cap; i++) {
		free (solutions[i].selection_list);
	}

	free (solutions);
	return bestAns;
}

int scale_by_gcd (int* capP, size_t n,
                  vw_t vws[restrict static n]) {
	int i = 0, gcd_w;

	// find the gcd of all of the weights (foldl' gcd cap (map snd vws))
	gcd_w = *capP;
	for (i = 0; i < n; i++) {
		// gcd_w gcd= weights[i]; sadly doesn't work :(
		gcd_w = gcd (gcd_w, vws[i].weight);
	}

	if (gcd_w != 1) {
		// scale the weights by their gcd
		*capP /= gcd_w;
		for (i = 0; i < n; i++) {
			vws[i].weight /= gcd_w;
		}
	}
	return gcd_w;
}

// where the highest solution is the highest value (or in the case of a tie the lowest weight)
int cmp_vws (const void *arg1, const void *arg2) {
	const vw_t *x = arg1;
	const vw_t *y = arg2;

	if (x->value > y->value) {
		return 1;
	} else if (x->value == y->value) {
		if (x->weight < y->weight) {
			return 1;
		} else if (x->weight == y->weight) {
			return 0;
		} else {
			return -1;
		}
	} else {
		return -1;
	}
}

int cmp_weight (const void *arg1, const void *arg2) {
	const vw_t *x = arg1;
	const vw_t *y = arg2;
	return x->weight - y->weight;
}


selection** empty_selection() {
	selection** ss = malloc(sizeof(selection*));
	*ss = NULL;
	return ss;
}

void cleanup_selection(selection **selections) {
	selection *current_selection, *tmp;

  	HASH_ITER(hh, *selections, current_selection, tmp) {
    	HASH_DEL(*selections,current_selection);
    	free(current_selection);
    }
    free (selections);
}

// (destructively) increment the quantity at a position in a hashmap
void increment_position(selection **selections, int position_) {
	selection *s;
	HASH_FIND_INT(*selections, &position_, s);

	// the position wasn't there, add it
	if (s == NULL) {
		s = malloc(sizeof(selection));
		s->position = position_;
		s->quantity = 1;
		HASH_ADD_INT (*selections, position, s);

	// the position was there, increment its quantity
	} else {
		s->quantity += 1;
	}
}
