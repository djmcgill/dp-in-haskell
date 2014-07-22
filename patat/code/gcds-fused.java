class GCDS {

public int gcds (int capacity, int[] weights) {
	int gcd_all = capacity;
	for (int weight : weights) {
		if (gcd_all == 1) {break;}
		gcd_all = gcd (gcd_all, weight);
	}
	return gcd_all;
}

public int gcd(int a, int b) { return b==0 ? a : gcd(b, a%b); }

}