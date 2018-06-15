package A2;

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
		
		quickSort(pointP, 0, numberOfInputP - 1);
		quickSort(pointQ, 0, numberOfInputQ - 1);
		
		//find closet point and compare
		int min = 2147483647;
		int count = 0;
		for(int i = 0; i < numberOfInputP; i ++) {
			int index = findingClosestPoint(pointQ, pointP[i], 0, numberOfInputQ - 1);
			if(index < 0) {
				index = -index;
				if(index < min) {
					min = index;
					count = 2;
				} else if(index == min) {
					count += 2;
				}
			} else {
				if(index < min) {
					min = index;
					count = 1;
				} else if(index == min) {
					count ++;
				}
			}
		}
		
		min += y1;
		System.out.println(min + " " + count);

	}
	
	public static int partition(int[] arr, int left, int right) {
	      int i = left, j = right;
	      int tmp;
	      int pivot = arr[(left + right) / 2];
	     
	      while (i <= j) {
	            while (arr[i] < pivot)
	                  i++;
	            while (arr[j] > pivot)
	                  j--;
	            if (i <= j) {
	                  tmp = arr[i];
	                  arr[i] = arr[j];
	                  arr[j] = tmp;
	                  i++;
	                  j--;
	            }
	      };
	     
	      return i;
	}
	 
	public static void quickSort(int[] arr, int left, int right) {
	      int index = partition(arr, left, right);
	      if (left < index - 1)
	            quickSort(arr, left, index - 1);
	      if (index < right)
	            quickSort(arr, index, right);
	}
	
	public static int findingClosestPoint(int[] arr, int thePoint, int l, int r) {
		//base case
		if(r == l) {
			int a = arr[r + 1] - thePoint;
			int b = thePoint - arr[l];
			if (a < 0) a = -a;
			if (b < 0) b = -b;
			if(a > b)
				return b;
			else if(a < b)
				return a;
			else return -a;
		} else if(r == l + 1) {
			int a = arr[r] - thePoint;
			int b = thePoint - arr[l];
			if(a < 0) a = -a;
			if(b < 0) b = -b;
			if(a > b)
				return b;
			else if(a < b)
				return a;
			else return -a;
		}
		
		int mid = (l + r) / 2;
		if(arr[mid] == thePoint) {
			return 0;
		}
		if(arr[mid] > thePoint) {
			return findingClosestPoint(arr, thePoint, l, mid - 1);
		} else {
			return findingClosestPoint(arr, thePoint, mid, r);
		}
	}
	
}
