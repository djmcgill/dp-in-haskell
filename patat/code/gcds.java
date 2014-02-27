class GCDS {

public int gcds (int capacity, int[] weights) {
	int ans = capacity;
	for (int weight : weights) {
		ans = gcd (ans, weight);
	}
	return ans;
}

public int gcd(int a, int b) {
   if (b==0) return a;
   return gcd(b,a%b);
}

}