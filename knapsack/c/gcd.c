// TODO: compare gcd algorithms
int gcd (int a, int b) {
	int t;
	while (b) {
		t = b;
		b = a % b;
		a = t;
	}
	return a;
}

int gcd2(int x, int y) {
	while (x && y) {
		if (x >= y) {x %= y;} else {y %= x;}
	}
	return (x + y);
}
