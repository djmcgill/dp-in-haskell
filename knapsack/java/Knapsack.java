import java.io.*;
import java.nio.charset.*;
import java.nio.file.*;
import java.text.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

public class Knapsack {
	private VW[] vws;
	private int capacity;
	private int n;

	public static void main (String[] argv) throws IOException, ParseException {
		if (argv.length != 1) {System.out.println("Usage: Knapsack \"<filename>\"");}
		Path path = Paths.get(argv[0]);
		Knapsack problem = new Knapsack (path);
		Solution solution = problem.solve();
		System.out.printf("\nThe Java solution is has a total weight of %i, a total value of %i and a selection of:\n"
			             , solution.total_weight, solution.total_value);

		Bag<Integer> chosenIndices = new Bag<Integer>(solution.selection);
		for (Map.Entry<Integer, AtomicInteger> entry : chosenIndices.entries()) {
			System.out.printf ("\tindex: %i, quantity: %i\n"
				              ,entry.getKey()
				              ,entry.getValue());
		}
	}

	public Knapsack (Path path) throws IOException, ParseException {
		// turn a file into an instance of knapsack
		// JAVA8: http://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#lines-java.nio.file.Path-
		// Path path = FileSystems.getDefault().getPath(stringPath);
		List<String> lines = Files.readAllLines(path, Charset.defaultCharset());
		if (lines.isEmpty()) {throw new ParseException("Empty file", 0);}

		String[] capacityAndN = lines.get(0).split(" ");
		capacity = Integer.parseInt(capacityAndN[0]);
		n = Integer.parseInt(capacityAndN[1]);

		// read each of the value/weight pairs
		vws  = new VW[n];
		ListIterator<String> valueWeights = lines.listIterator(1);
		for (int i = 0; i < n; i++) {
			String[] valueAndWeight;
			try {
				valueAndWeight = valueWeights.next().split(" ");
			} catch (NoSuchElementException e) {
				throw new ParseException("Expected more value/weights",0);
			}
			int value = Integer.parseInt(valueAndWeight[0]);
			int weight = Integer.parseInt(valueAndWeight[1]);
			vws[i] = new VW(value, weight);
		}
	}

	public Solution solve () {

		int gcd_all = capacity;
		for (VW vw : vws) {
			gcd_all = gcd (gcd_all, vw.weight);
		}
		if (gcd_all != 1) {
			// TODO: scale the weights
			Arrays.sort(vws, new CompareVWsByWeight());
		}

		// create the n+1 vector of possible solutions
		Solution[] solutions = new Solution[capacity+1];
		solutions[0] = new Solution();
		for (int i = 1; i <= capacity; i++) {
			List<Solution> candidateSolutions = new LinkedList<Solution>();
			for (int j = 0; j < n; j++) {
				// let's see what this solution would look like

				if (vws[j].weight > i) {break;} // no more weights will fit

				int ix = i-vws[j].weight;
				Solution tempSolution = new Solution(solutions[ix]);
				tempSolution.chooseVW(j, vws[j]);

				candidateSolutions.add(tempSolution);
			}
			solutions[i] = Collections.max(candidateSolutions);
		}
		return solutions[capacity]; // since there are cap+1 solutions
	}

	public int gcd(int a, int b) { return b==0 ? a : gcd(b, a%b); }
}



class Solution implements Comparable<Solution> {
	public ArrayList<Integer> selection;
	public int total_value;
	public int total_weight;

	public Solution () {
		selection = new ArrayList<Integer>();
		total_value = 0;
		total_weight = 0;
	}

	public void chooseVW (int ix, VW vw) {
		selection.add(ix);
		total_value += vw.value;
		total_weight += vw.weight;
	}

	public Solution (Solution _solution) {
		// could possibly make this lazy
		selection = new ArrayList<Integer>(_solution.selection);
		total_value = _solution.total_value;
		total_weight = _solution.total_weight;
	}

	@Override
	// get the solution with the highest value or,
	// in the case of a tie, the lowest weight
	public int compareTo (Solution s2) {
		if (s2 == null) {return  1;}

		int value_diff = this.total_value - s2.total_value;
		if (value_diff != 0) {
			return value_diff;
		} else {
			return this.total_weight - s2.total_weight;
		}
	}
}




class VW {
	public int value;
	public int weight;

	public VW (int _value, int _weight) {
		value  = _value;
		weight = _weight;
	}
}

// When sorting, we only care about the weight
class CompareVWsByWeight implements Comparator<VW> {
	@Override
	public int compare (VW vw1, VW vw2) {
		if (vw1 == null || vw2 == null) {throw new NullPointerException();}
		return vw1.weight - vw2.weight;
	}
}

class Bag<T> {
	Map<T, AtomicInteger> counts;

	public Bag () {
		counts = new Hashtable<T, AtomicInteger>();
	}

	public Bag (List<T> values) {
	    counts = new Hashtable<T, AtomicInteger>();
	    for (T value : values) {
	    	this.add(value);
	    }
	}

	public void add(T value) {
		AtomicInteger count = counts.get(value);
		if (count == null) {
			counts.put(value, new AtomicInteger(1));
		} else {
			count.incrementAndGet();
		}
	}

	public Set<Map.Entry<T,AtomicInteger>> entries() {
		return counts.entrySet();
	}

}