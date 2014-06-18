import java.util.*;

class LCS {
	private String s1;
	private String s2;

	public static void main (String[] args) {
		if (args.length != 2) {
			System.out.printf("Usage: LCS <string_1> <string_2>\n");
			System.exit(0);
		}

		LCS problem = new LCS (args[0], args[1]);;
		Map<MyIndex, Integer> allSubstrings = problem.findAllCommonSubstrings();
		List<String> lcSubStrings = problem.getLongestSubstrings(allSubstrings);
		System.out.printf("The longest common substrings are:\n");
		for (String substring : lcSubStrings) {
			System.out.printf("\t%s\n", substring);
		}
	}

	public LCS (String _s1, String _s2) {
		s1 = _s1;
		s2 = _s2;
	}

	private Map<MyIndex, Integer> findAllCommonSubstrings () {
		Map<MyIndex, Integer> collisions = new HashMap<>();

		int i = 0;
		for (Character c1 : s1.toCharArray()) {
			int j = 0;
			for (Character c2 : s2.toCharArray()) {
				if (c1 == c2) {
					MyIndex oldKey = new MyIndex(i-1, j-1);
					Integer previous = collisions.get(oldKey);
					if (previous == null) {previous = 0;}
					collisions.remove(oldKey);
					collisions.put(new MyIndex (i, j), previous + 1);
				}
			j++;
			}
		i++;
		}
		return collisions;
	}

	private List<String> getLongestSubstrings (Map<MyIndex, Integer> substrings) {
		List<String> longestSubstrings = new LinkedList<>();
		if (substrings == null || substrings.isEmpty()) {
			return longestSubstrings;
		}
		// get the maximum
		int maximum = 0;
		for (int value : substrings.values()) {
			if (value >= maximum) {maximum = value;}
		}

		// get the indices of the most common substrings from the hashmap
		// then look them up in the original string
		for (Map.Entry<MyIndex, Integer> entry : substrings.entrySet()) {
			if (entry.getValue().equals(maximum)) {

				int endIndex = entry.getKey().getFst() + 1;
				int startIndex = endIndex - entry.getValue();
				longestSubstrings.add(s1.substring(startIndex, endIndex));
			}
		}
		return longestSubstrings;
	}
}

class MyIndex {
	final private int fst, snd;

	public MyIndex (int _fst, int _snd) {
		fst = _fst;
		snd = _snd;
	}

	public int getFst() {return fst;}
	public int getSnd() {return snd;}

	@Override
	public boolean equals (Object o) {
		if (o == null) {return false;}
		if (!(o instanceof MyIndex)) {return false;}
		MyIndex pair = (MyIndex) o;
		return this.fst == pair.fst && this.snd == pair.snd;
	}

	@Override
	public int hashCode() {return fst*16777619 ^ snd;} // FNV hash
}