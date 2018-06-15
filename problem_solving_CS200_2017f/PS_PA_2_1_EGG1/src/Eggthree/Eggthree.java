import java.util.*;

public class Eggthree {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		 Scanner scan = new Scanner(System.in);
		 long q = scan.nextLong();
		 long n = (long) Math.cbrt((double) q) - 1;
		 while(true) {
			n ++;
			if(q <= n * (n * n + 5) / 6)
				break;
		}
		System.out.println(n);
	}

}
