import java.util.*;

public class F {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Scanner in = new Scanner(System.in);
		
		long upper = in.nextLong();
		long lower = in.nextLong();
		upper = upper - lower;
		upper *= 1000;
		
		int numSeg = in.nextInt();
		numSeg = numSeg * 2;
		long[] seg = new long[numSeg];
		for(int i = 0; i < numSeg; i ++) {
			seg[i] = in.nextInt();
		}
		
		int numLine = in.nextInt();
		
		double tempIn = 0;
		long[] yLines = new long[numLine];
		
		for(int i = 0; i < numLine; i ++) {
			tempIn = in.nextDouble();
			tempIn *= 1000;
			yLines[i] = (long) tempIn;
		}
		for(int i = 0; i < numLine; i ++) {
			yLines[i] -= lower * 1000;
		}
		lower = 0;
		
		for(int i = 0; i < numSeg; i ++) {
			if(i % 2 == 0) {
				seg[i] -= seg[i + 1];
			} else {
				seg[i] *= upper;
			}
		}

		int index = 0;
		
		for(int i = 0; i < numLine; i ++) {
			long min = Long.MAX_VALUE;
			long y = yLines[i];
			for(int j = 0; j < numSeg; j = j + 2) {
				long position = seg[j] * y + seg[j + 1];
				if(position < min) {
					min = position;
					index = (j / 2) + 1;
				}
			}
			yLines[i] = index;
		}
		
		for(int i = 0; i < numLine; i ++) {
			System.out.println(yLines[i]);
		}
		
	}

}
