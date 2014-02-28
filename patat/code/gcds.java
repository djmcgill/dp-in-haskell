class GCDS {
private int CAP = 84;
private int[] WS = {26,90,65,32,84,78,69,77,58,85};

public int gcds (int capacity, int[] weights) {
	int gcd_all = capacity;
	for (int weight : weights) {
		gcd_all = gcd (gcd_all, weight);
	}
	return gcd_all;
}

public int gcd(int a, int b) { return b==0 ? a : gcd(b, a%b); }

public static void main () {
	System.out.println(gcds(CAP, WS));
}
}