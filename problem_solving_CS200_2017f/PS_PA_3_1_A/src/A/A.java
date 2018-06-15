package A;
import java.util.*;

public class A {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		
		//let's get inputs
		
		Scanner in = new Scanner(System.in);

		int numberOfInputs = in.nextInt();
		int numberOfInputs2 = in.nextInt();

		inputs[] dataSet = new inputs[numberOfInputs + numberOfInputs2];
		for(int i = 0; i < numberOfInputs + numberOfInputs2; i ++) {
			dataSet[i] = new inputs();
		}
		
		int y1 = in.nextInt();
		int y2 = in.nextInt();
		if(y1 > y2)
			y1 = y1 - y2;
		else
			y1 = y2 - y1;
		
		int indexPosition = 0;
		for(; indexPosition < numberOfInputs; indexPosition ++) {
			dataSet[indexPosition].data = in.nextInt();
		}
		
		for(; indexPosition < numberOfInputs + numberOfInputs2; indexPosition ++) {
			int i = indexPosition - numberOfInputs;
			dataSet[indexPosition].data = in.nextInt();
			dataSet[indexPosition].isFirst = false;
		}
		
		/** testing input System
		System.out.println(numberOfInputs + " , " + numberOfInputs2);
		System.out.println(y1 + " , " + y2);
		for(int i = 0; i < numberOfInputs; i ++) {
			System.out.print(firstInputs[i] + ", ");
		}
		System.out.println("");
		for(int i = 0; i < numberOfInputs2; i ++) {
			System.out.print(secondInputs[i] + ", ");
		}
		**/
		

		
		/** testing data source
		for(int i = 0; i < numberOfInputs + numberOfInputs2; i ++) {
			System.out.print(dataSet[i].data + ", ");
		}**/
		
		
		
		//do sorting
		quickSort(dataSet, 0, numberOfInputs + numberOfInputs2 - 1);
		/** for testing quickSort!!!
		System.out.println("after sorting...");
		
		for(int i = 0; i < numberOfInputs + numberOfInputs2; i ++) {
			System.out.print(dataSet[i].data + ", ");
			System.out.println(dataSet[i].isFirst);
		} **/
		
		int min = 2147483647;
		int count = 0;
		for(int i = 0; i < numberOfInputs + numberOfInputs2 - 1; i ++) {
			if(dataSet[i].isFirst != dataSet[i + 1].isFirst) {
				int difference = dataSet[i].data - dataSet[i + 1].data;
				if(difference < 0)
					difference = -difference;
				if(difference < min) {
					min = difference;
					count = 1;
				} else if(difference == min) {
					count ++;
				}
			}
		}
		int ans = min + y1;
		System.out.println(ans + " " + count);
		
	}
	
	
	//using quick sort
	public static int partition(inputs arr[], int left, int right)
	{
	      int i = left, j = right;
	      inputs tmp;
	      int pivot = arr[(left + right) / 2].data;
	     
	      while (i <= j) {
	            while (arr[i].data < pivot)
	                  i++;
	            while (arr[j].data > pivot)
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
	 
	public static void quickSort(inputs arr[], int left, int right) {
	      int index = partition(arr, left, right);
	      if (left < index - 1)
	            quickSort(arr, left, index - 1);
	      if (index < right)
	            quickSort(arr, index, right);
	}
	
	static class inputs {
		public int data;
		public boolean isFirst;
		
		public inputs () {
			data = 0;
			isFirst = true;
		}
	}
}
