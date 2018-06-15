package C_GameMap;
import java.util.*;

public class runCC {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Scanner in = new Scanner(System.in);
		int vertexNum = in.nextInt();
		int edgeNum = in.nextInt();
		Vertex[] vertex = new Vertex[vertexNum];
		
		for(int i = 0; i < vertexNum; i ++) {
			vertex[i] = new Vertex(i, vertexNum);
		}
		
		for(int i = 0; i < edgeNum; i ++) {
			int a = in.nextInt();
			int b = in.nextInt();
			vertex[a].setDeg(b);
			vertex[b].setDeg(a);
		}
		
		quickSort(vertex, 0, vertexNum - 1);
		
		//printGraph(vertex, vertexNum, edgeNum);
		int[] pathTo = new int[vertexNum];
		for(int i = 0; i < vertexNum; i ++) {
			pathTo[i] = -1;
		}
		
		/**System.out.println(findLongest(vertex, vertexNum, pathTo, 1));
		
		for(int i = 0; i < vertexNum; i ++) {
			System.out.print(pathTo[i] + ", ");
		}**/
		
		for(int i = 0; i < vertexNum; i ++) {
			System.out.print(vertex[i].getDeg());
		}
		System.out.println("");
		
		for(int i = 0; i < vertexNum; i ++) {
			if(pathTo[i] == -1) {
				findLongest(vertex, vertexNum, pathTo, i);
			}
			for(int j = 0; j < vertexNum; j ++) {
				System.out.print(pathTo[j] + ", ");
			}
			System.out.println("for " + i + "'s itertation.");
		}
		
		int max = 0;
		for(int i = 0; i < vertexNum; i ++) {
			if(max < pathTo[i])
				max = pathTo[i];
		}
		System.out.println(max);
		in.close();
	}
	
	public static int findLongest(Vertex[] vertex, int vertexNum, int[] pathTo, int item) {

		int currDeg = vertex[item].getDeg();
		int[] con = vertex[item].getCon();
		int ret = 0;
		for(int i = 0; i < currDeg; i ++) {
			if(vertex[con[i]].getDeg() < currDeg) {
				System.out.println("current item is '" + item + ", deg: " + currDeg + ", con[" + i + "]'s deg is " + vertex[con[i]].getDeg());
				int temp = 0;
				if(pathTo[con[i]] > -1)
					temp = pathTo[con[i]];
				else {
					temp = findLongest(vertex, vertexNum, pathTo, con[i]);
				}
				if(ret < temp) {
					ret = temp;
				}
			}
		}
		
		pathTo[item] = ++ret;
		return ret;
	}
	
	public static void printGraph(Vertex[] vertices, int vertexNum, int edgeNum) {
		for(int i = 0; i < vertexNum; i ++) {
			vertices[i].printCon();
		}
	}
	
	//using quick sort
	public static int partition(Vertex arr[], int left, int right){
	      int i = left, j = right;
	      Vertex tmp;
	      int pivot = arr[(left + right) / 2].deg;
	     
	      while (i <= j) {
	            while (arr[i].deg < pivot)
	                  i++;
	            while (arr[j].deg > pivot)
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
	 
	public static void quickSort(Vertex arr[], int left, int right) {
	      int index = partition(arr, left, right);
	      if (left < index - 1)
	            quickSort(arr, left, index - 1);
	      if (index < right)
	            quickSort(arr, index, right);
	}
	
	static class Vertex {
		public int deg;
		public int item;
		public int[] con;
		
		public Vertex (int item, int size) {
			this.item = item;
			con = new int[size];
			deg = 0;
		}
		
		public void setDeg (int next) {
			con[deg ++] = next;
		}
		
		public int getDeg() {
			return deg;
		}
		
		public int[] getCon() {
			return con;
		}
		
		public boolean findCon(int vt) {
			for(int i = 0; i < deg; i ++) {
				if(con[i] == vt) return true;
			}
			return false;
		}
		
		public void printCon() {
			System.out.println("vertex " + item + " contains...");
			for(int i = 0; i < deg; i ++) {
				System.out.print(con[i] + ", ");
			}
			System.out.println("");
		}
	}

}
