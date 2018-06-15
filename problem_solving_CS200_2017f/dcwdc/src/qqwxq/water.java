import java.util.Scanner;

class Main {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		
		//check running time
		long startTime = System.currentTimeMillis();
		
		//to put inputs into integer
		
		Scanner input = new Scanner(System.in);
		int key1 = input.nextInt();
		int key2 = input.nextInt();
		int key3 = input.nextInt();
		int key4 = input.nextInt();	

		int answer = waterOperation(key1, key2, key3, key4);
		System.out.println(answer);
		
		
		input.close();
		// End time
		long endTime = System.currentTimeMillis();
		 
		// Total time
		long lTime = endTime - startTime;
		System.out.println("TIME : " + lTime + "(ms)");
	}
	
	
	public static int waterOperation(int a, int b, int c, int d) {
		
		//set the goal_the other is 0 or full
		int goal = 0;
		boolean goalBottleisA = true; // which bottle is the goal bottle: a - true, b - false
		boolean otherisFull = true; // whether non-goal bottle is full or 0
		
		
		if(c == 0) {
			if(d == 0) return 0;
			if (d == b)	return 1;
			goal = d;
			goalBottleisA = false;
			otherisFull = false;
		}
		else if(a == c) {
			if(d == 0) return 1;
			if(d == b) return 2;
			goal = d;
			goalBottleisA = false;
		}
		else if(d == 0) {
			goal = c;
			otherisFull = false;
		}
		else if(d == b) {
			goal = c;
		}
		else {
			return -1;
		}
		
		int i = a;
		int j = b;
		while(i != j) {
			if(i < j) j -= i;
			else i -= j;
		}
		
		if(goal % i != 0) return -1;
		
		int count = b;
		int k1 = 0;
		int k2 = 0;
		
		while(count != goal) {
			if(count > goal) {
				count -= a;
				k1 ++;
			}
			else {
				count += b;
				k1 ++;
			}
		}
		
		
		count = a;
		while(count != goal) {
			if(count > goal) {
				count -= b;
				k2 ++;
			}
			else {
				count += a;
				k2 ++;
			}
		}

		
		
		boolean versus = true;
		int lastB = 0;
		int lastA = 0;
		if(!goalBottleisA) {
			if(!otherisFull) lastB = k1 * 2 + 1; //a: full, b: goal => a: 0, b: goal
			else lastB = k1 * 2; //a: full, b: goal => a: full, b: goal
		}
		else {
			if(!otherisFull) lastB = k1 * 2 + 2; //a: full, b: goal => a: goal, b: 0
			else lastB = k1 * 2 + 3; //a: full, b: goal => a: goal, b: full
		}	
		
		if(goalBottleisA) {
			if(!otherisFull) lastA = k2 * 2 + 1; //a: goal, b: full => a: goal, b: 0
			else lastA = k2 * 2; //a: goal, b: full => a: goal, b: full
		}
		else {
			if(!otherisFull) lastA = k2 * 2 + 2; //a: goal, b: full => a: 0, b: goal
			else lastA = k2 * 2 + 3; //a: goal, b: full => a: full, b: goal
		}					

		if(lastA < lastB) return lastA;
		return lastB;
		
	}
}
