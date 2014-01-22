#include<stdio.h>
#include<assert.h>

#include "uthash.h"

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

// XXX: is this really the best way to go about it?
int gcd (int a, int b) {
	int t;
	while (b != 0) {
		t = b;
		b = a % b;
		a = t;
	}
	return a;
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

solution knapsack (int cap, int n, int* values, int* weights) {
	// assuming that len(values) == len (weights) == n
	int i = 0;
	int j = 0;

	// find the gcd of all of the weights (foldl gcd cap weights)
	int gcdWeights = cap;
	for (i = 0; i < n; i++) {
		// gcdWeights gcd= weights[i]; sadly doesn't work :(
		gcdWeights = gcd (gcdWeights, weights[i]);
	}
	// scale the weights by their gcd
	cap /= gcdWeights;
	for (i = 0; i < n; i++) {
		weights[i] /= gcdWeights;
	}
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

int main () {
	int cap, n, i = 0;
	int *values, *weights;
	FILE *pFile;
	solution bestAns;

	pFile = fopen ("test_problem_1.data","r");
	fscanf (pFile, "%i %i\n", &cap, &n);

	values = malloc (n*sizeof(int));
	weights = malloc (n*sizeof(int));

	for (i = 0; i < n; i++) {
		fscanf(pFile, "%i %i\n", values + i, weights + i);
	}
	fclose(pFile);

	bestAns = knapsack (cap, n, values, weights);
	free (weights);
    free (values);

	printf ("The solution is has a total weight of %i, a total value of %i and a selection of:\n",
		bestAns.total_weight, bestAns.total_value);

	selection *current_selection, *tmp;

	// print answers
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


