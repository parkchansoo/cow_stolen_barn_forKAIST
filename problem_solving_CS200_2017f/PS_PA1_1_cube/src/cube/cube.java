package cube;
import java.io.*;
import java.util.Scanner;

public class cube {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		
		int[] cube = new int[10000];
		int i = 1;
		for(i = 1; i < 10000; i ++) cube[i] = i * i * i; 
		
		Scanner in = new Scanner(System.in);
		int nums = in.nextInt();
		int count = 0;
		int goal = 0;
		int temp = 0;
		
		for( ; nums > 0; nums --) {
			goal = in.nextInt();
			temp = search(goal, cube);
			for(i = temp; i > 0; i --) {
				for(int j = i; j > 0; j --) {
					if(goal == (cube[i] + cube[j])) count ++;
				}
			}
			System.out.println(count);
			count = 0;
		}
		
		in.close();
	}
	
	public static int search(int goal, int[] arr) {
		int middle = 500;
		int left = 0;
		int right = 1000;
		for(int j = 0; j < 6; j ++) {
			if(goal < arr[middle]) {
				right = (right + left) / 2;
				middle =  (right + left) / 2;
			}
			else if(goal > arr[middle]) {
				left = (right + left) / 2;
				middle =  (right + left) / 2;
			}
			else {
				return middle;
			}
		}
		return right;
	}

}
