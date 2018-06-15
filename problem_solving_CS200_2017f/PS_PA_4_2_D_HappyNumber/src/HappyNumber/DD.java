import java.util.ArrayList;
import java.util.Scanner;

public class DD {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Scanner in = new Scanner(System.in);
		int digit = in.nextInt();
		ArrayList<Integer> list = new ArrayList<Integer>();
		
		while(digit > 1) {
			if(list.contains(digit)) {
				System.out.println("UNHAPPY");
				return;
			}
			list.add(digit);
			digit = sumSquare(digit);
		}
		System.out.println("HAPPY");
		return;
	}
	
	public static int sumSquare(int num) {
		int ret = 0;
		for(int i = num; i > 0; i = i / 10) {
			int remain = i % 10;
			ret += remain * remain;
		}
		return ret;
	}

}
