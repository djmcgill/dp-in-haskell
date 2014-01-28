#include<stdio.h>
#include<assert.h>

#include "uthash-master/src/uthash.h"
#include "gcd.c"
#include "knapsack.h"

int main () {
	int cap = 0, n = 0, i = 0;
	int *values = NULL, *weights = NULL;
	solution bestAns;
	char* FILE_NAME = "test_problem_1.data";

	read_file(FILE_NAME, &cap, &n, &values, &weights);

	bestAns = knapsack (cap, n, values, weights);
	free (weights);
    free (values);

	// print answers
	printf ("The solution is has a total weight of %i, a total value of %i and a selection of:\n",
		bestAns.total_weight, bestAns.total_value);

	selection *current_selection, *tmp;
	HASH_ITER(hh, *(bestAns.sol_selection), current_selection, tmp) {
		printf ("\tindex: %i, quantity: %i\n", current_selection->position, current_selection->quantity);
	}

	// free sol_selection
	HASH_ITER(hh, *(bestAns.sol_selection), current_selection, tmp) {
		HASH_DEL(*(bestAns.sol_selection), current_selection);
		free (current_selection);
	}
	free (bestAns.sol_selection);
	return 0;
}

void read_file (const char* const file_name,
	            int* pCap,
	            int* pN,
	            int** restrict pValues,
	            int** restrict pWeights) {
	FILE *pFile;
	int i = 0;

	pFile = fopen ("test_problem_1.data","r");
	fscanf (pFile, "%i %i\n", pCap, pN);
	assert (*pN > 0 && *pCap >= 0);

	pValues = malloc (*pN *sizeof(int));
	pWeights = malloc (*pN *sizeof(int));

	for (i = 0; i < *pN; i++) {
		fscanf (pFile, "%i %i\n", *pValues + i, *pWeights + i);
		assert (*pValues[i] >= 0 && *pWeights[i] > 0);

	}
	fclose(pFile);
}

solution knapsack(int cap, size_t n,
                  int weights[restrict static n],
                  int values[restrict static n]) {
	// assuming that len(values) == len (weights) == n
	int i = 0;
	int j = 0;

	int gcdWeights = scale_by_gcd(&cap, n, weights);

	// make an array of size (cap+1) where the ith element is the best solution for a weight of i
	solution* solutions = malloc((cap+1)*sizeof(solution));
	solutions[0] = (solution){0,0,empty_selection()};

	int best_value;
	int best_weight;
	int best_j;
	int best_position;

	for (i = 1; i <= cap; i++) {
		best_value = 0;
		best_weight = 0;
		best_j = -1;
		best_position = -1;

		// for each value/weight, try to add it to best_selection
		for (j = 0; j < n; j++) {
			int value = values[j];
			int weight = weights[j];

			// if the weight doesn't fit then we can't do anything
			if (weight > i) {continue;}

			int prospective_position = i - weight;
			int prospective_value = value + solutions[prospective_position].total_value;
			int prospective_weight = weight + solutions[prospective_position].total_weight;
			assert (prospective_weight <= i);

			// if we've found a new maximum value (i.e. higher value or same value + lower weight)
			if ((prospective_value > best_value) ||
				(prospective_value == best_weight && prospective_weight < best_weight)) {

				best_value = prospective_value;
				best_weight = prospective_weight;
				best_j = j;
				best_position = prospective_position;
			}
		}

		// update solutions[i]
		solutions[i].total_value = best_value;
		solutions[i].total_weight = best_weight;

		// copy over the selection hashmap (including each selection)
		solutions[i].sol_selection = empty_selection();

		if (best_j != -1 && best_position != -1) {
			// if we have a new position, copy over the old selection and increment the position
			selection* s;
			for (s = *(solutions[best_position].sol_selection); s != NULL; s=s->hh.next) {
				selection* new_s = malloc(sizeof(selection));
				*new_s = *s;
				HASH_ADD_INT (*(solutions[i].sol_selection), position, new_s);
			}
		increment_position(solutions[i].sol_selection, best_j);
		}
	}

	solution bestAns = solutions[cap];
	bestAns.total_weight *= gcdWeights;

	// free each of the solutions (except solutions[cap] since its sol_selection gets reused in bestAns)
	for (i = 0; i < cap; i++) {
		cleanup_selection(solutions[i].sol_selection);
	}
	free (solutions);
	return bestAns;
}

int scale_by_gcd (int* capP, size_t n,
                  int weights[restrict static n]) {
	int i = 0, gcdWeights;

	// find the gcd of all of the weights (foldl' gcd cap weights)
	gcdWeights = *capP;
	for (i = 0; i < n; i++) {
		// gcdWeights gcd= weights[i]; sadly doesn't work :(
		gcdWeights = gcd (gcdWeights, weights[i]);
	}

	// scale the weights by their gcd
	*capP /= gcdWeights;

	for (i = 0; i < n; i++) {
		weights[i] /= gcdWeights;
	}
	return gcdWeights;
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
