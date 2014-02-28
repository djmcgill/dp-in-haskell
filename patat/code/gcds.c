#include <stdlib.h>

int CAP = 84;
int WS[] = {26,22,57,13,73,38,20,14,2,71,15,90,57,3,79,73,53,41,44,11,100,15,53,60,19,94,76,85,72,31,7,16,75,76,30,12,15,50,63,89,78,83,77,13,80,27,29,73,10,55,49,98,42,98,26,57,54,36,85,1,45,87,99,76,27,1,12,35,73,77,76,30,65,22,57,37,35,17,61,5,40,63,80,62,58,69,53,75,88,31,17,71,58,46,45,18,86,67,78,68};
int N = 100;
int ITERATIONS = 100000;

int gcd(int a, int b) { return !b ? a : gcd(b, a%b); }

int volatile gcds (int capacity, size_t n, int weights[restrict static n]) {
	int i, gcd_all = capacity;
	for (i = 0; i < n; i++) {
		gcd_all = gcd (gcd_all, weights[i]);
	}
	return gcd_all;
}
