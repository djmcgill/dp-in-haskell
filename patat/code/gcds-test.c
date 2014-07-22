#include "gcds.c"
#include <stdio.h>
#include <time.h>

int main () {
	int i;
	time_t old_t, new_t;
	old_t = time(NULL);
	for (i = 0; i < ITERATIONS; i++) {
		gcds (CAP, N, WS);
	}
	new_t = time(NULL);
	printf ("C99: %i : %i ms\n", N, (new_t-old_t)/ITERATIONS);
	return 0;
}