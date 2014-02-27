#include <stdlib.h>

int gcd (int a, int b) {
	int t;
	while (b) {
		t = b;
		b = a % b;
		a = t;
	}
	return a;
}

int gcds (int capacity, size_t n, int weights[restrict static n]) {
	int i, ans = capacity;
	for (i = 0; i < n; i++) {
		ans = gcd (ans, weights[i]);
	}
	return ans;
}

int main () {
	return 0;
}