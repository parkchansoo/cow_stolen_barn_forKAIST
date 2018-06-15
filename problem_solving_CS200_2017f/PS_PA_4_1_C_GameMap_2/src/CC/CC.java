package CC;
import java.util.*;

public class CC {

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
		in.close();
		
		int[] pathTo = new int[vertexNum];
		for(int i = 0; i < vertexNum; i ++) {
			pathTo[i] = -1;
		}
		
		for(int i = 0; i < vertexNum; i ++) {
			if(pathTo[i] == -1) {
				findLongest(vertex, vertexNum, pathTo, i);
			}
		}
		
		int max = 0;
		for(int i = 0; i < vertexNum; i ++) {
			if(max < pathTo[i])
				max = pathTo[i];
		}
		System.out.println(max);
		return;
	}
	
	public static int findLongest(Vertex[] vertex, int vertexNum, int[] pathTo, int item) {
		int currDeg = vertex[item].getDeg();
		int[] con = vertex[item].getCon();
		int ret = 0;
		for(int i = 0; i < currDeg; i ++) {
			if(vertex[con[i]].getDeg() < currDeg) {
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

	
	static class Vertex {
		private int deg;
		private int item;
		private int[] con;
		
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
		
	}

}
