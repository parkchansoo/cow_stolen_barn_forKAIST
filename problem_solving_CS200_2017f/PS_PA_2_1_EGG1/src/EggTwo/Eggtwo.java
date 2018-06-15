package EggTwo;
import java.util.*;

public class Eggtwo {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
			 Scanner scan = new Scanner(System.in);
			 long q = scan.nextLong();
			 long n = (long) Math.sqrt(q);
			 if(q == 1) {
				 System.out.println(q);
			 }
			 while(true) {
				 n ++;
				 if(q <= n * (n + 1) / 2)
					 break;
			 }
			 System.out.println(n);
	}

}