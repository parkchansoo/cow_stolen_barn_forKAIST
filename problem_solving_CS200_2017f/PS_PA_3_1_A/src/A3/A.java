import java.util.*;

public class A {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Scanner in = new Scanner(System.in);

		int numberOfInputP = in.nextInt();
		int numberOfInputQ = in.nextInt();

		
		int y1 = in.nextInt();
		int y2 = in.nextInt();
		if(y1 > y2)
			y1 = y1 - y2;
		else
			y1 = y2 - y1;
		
		int[] pointP = new int[numberOfInputP];
		int[] pointQ = new int[numberOfInputQ];
		
		for(int i = 0; i < numberOfInputP; i ++) {
			pointP[i] = in.nextInt();
		}
		
		for(int i = 0; i < numberOfInputQ; i ++) {
			pointQ[i] = in.nextInt();
		}

		Arrays.sort(pointP);
		Arrays.sort(pointQ);
		
		
		int min = 2147483647;
		int count = 0;
		int ip = 0;
		int iq = 0;
		while( (ip < numberOfInputP) && (iq < numberOfInputQ) ) {
						
			int curr = pointP[ip] - pointQ[iq];
			
			if(curr > 0) {
				if(curr < min) {
					min = curr;
					count = 1;
				} else if(curr == min) {
					count ++;
				}
				
				iq ++;
			} else if(curr < 0) {
				curr = -curr;
				if(curr < min) {
					min = curr;
					count = 1;
				} else if(curr == min) {
					count ++;
				}
				
				ip ++;		
			} else { // pointP[ip] is same with pointQ[iq]
				if(curr < min) {
					min = curr;
					count = 1;
				} else if(curr == min) {
					count ++;
				}
				// how to check next elements
				if( (ip < numberOfInputP - 1) && (iq < numberOfInputQ - 1) ){
					int next = pointP[ip + 1] - pointQ[iq + 1];
					if(next == 0) {
						ip ++;
						iq ++;
					} else if(next > 0) {
						iq ++;
					} else {
						ip ++;
					}
				} else { //there is no possible way to get another min
					break;
				}
			}
		}
		
		min += y1;
		
		System.out.println(min + " " + count);
		
		
	}


}
