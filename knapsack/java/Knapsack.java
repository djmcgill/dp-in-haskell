import java.io.*;
import java.nio.charset.*;
import java.nio.file.*;
import java.text.*;
import java.util.*;

public class Knapsack {
	private int[] values;
	private int[] weights;
	private int capacity;
	private int n;

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
		values  = new int[n];
		weights = new int[n];
		ListIterator<String> valueWeights = lines.listIterator(1);
		for (int i = 0; i < n; i++) {
			String[] valueAndWeight;
			try {
				valueAndWeight = valueWeights.next().split(" ");
			} catch (NoSuchElementException e) {
				throw new ParseException("Expected more value/weights",0);
			}
			values[i] = Integer.parseInt(valueAndWeight[0]);
			weights[i] = Integer.parseInt(valueAndWeight[1]);
		}
	}

	public Solution solve () {

		int gcd_all = capacity;
		for (int weight : weights) {
			gcd_all = gcd (gcd_all, weight);
		}
		if (gcd_all != 1) {
			// scale the weights and sort the vws
		}

		// create the n+1 vector of possible solutions
		Solution[] solutions = new Solution[capacity+1];
		solutions[0] = new Solution();
		for (int i = 1; i <= capacity; i++) {
			Solution tempBest = new Solution();
			for (int j = 0; j < n; j++) {
				// let's see what this solution would look like

				if (weights[j] > i) {break;} // no more weights will fit

				Solution tempSolution = new Solution(solutions[i-weights[j]]);
				tempSolution.chooseVW(j, values[j], weights[j]);

				tempBest = tempSolution.value > tempBest.value
						 ? tempSolution : tempBest;
			}
			solutions[i] = tempBest;
		}
		return solutions[capacity]; // since there are cap+1 solutions
	}

	public int gcd(int a, int b) { return b==0 ? a : gcd(b, a%b); }
}



class Solution {
	private ArrayList<Integer> selection;
	public int value;
	public int weight;

	public Solution () {
		selection = new ArrayList<Integer>();
		value = 0;
		weight = 0;
	}

	public void chooseVW (int _selection, int _value, int _weight) {
		selection.add(_selection);
		value += _value;
		weight += _weight;
	}

	public Solution (Solution _solution) {
		// could possibly make this lazy
		selection = new ArrayList<Integer>(_solution.selection);
		value = _solution.value;
		weight = _solution.weight;
	}
}