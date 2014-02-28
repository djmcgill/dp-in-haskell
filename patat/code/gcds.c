#include <stdlib.h>

int gcd(int a, int b) { return !b ? a : gcd(b, a%b); }

int gcds (int capacity, size_t n, int weights[n]) {
	int i, gcd_all = capacity;
	for (i = 0; i < n; i++) {
		gcd_all = gcd (gcd_all, weights[i]);
	}
	return gcd_all;
}
