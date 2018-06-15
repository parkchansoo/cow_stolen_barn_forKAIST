package Bindaedduk;
import java.util.*;

public class Bindaedduk {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Scanner scanner = new Scanner(System.in);
		int size = scanner.nextInt();
		dduk k = new dduk(size);
		for(int i = 0; i < size; i ++) {
			k.dduks[i] = scanner.nextInt();
			char c = scanner.next().charAt(0);
			if(c == '-')
				k.dduks[i] = -k.dduks[i];
		}
		
		if(k.isSorted()) {
			System.out.println(0);
		} else if(k.isBeforeSort()) {
			System.out.println(1);
		} else {
			
			ArrayList<dduk> trace = new ArrayList<>();
			trace.add(k);
			int f = 0;
			int queueSize = 1;
			
			dduk current = new dduk(size);
			dduk next = new dduk(size);
			int finalIndex = 0;
			boolean flag = true;
						
			while(flag) {
				current = trace.get(f);
				for(int i = 1; i <= size; i ++) {
					if(i == current.getPrevious()) {
					} else {
						next.putNextDduk(current);						
						next.flip(i);

						if(next.isBeforeSort()) {
							flag = false;
							finalIndex = next.getIndex() + 1;
							break;
						}
						queueSize ++;
						dduk inDduk = new dduk(size);
						inDduk.putNextDduk(next);
						trace.add(inDduk);
					}
				}
				f ++;
				queueSize --;
			}
			System.out.println(finalIndex);
			scanner.close();
		}

	}
	

static class dduk {
	
	private int index; //flip count
	private int previous; 
	public int[] dduks;
	private int size;
	
	public dduk (int size) {
		this.index = 0;
		this.previous = -1;
		this.size = size;
		this.dduks = new int[this.size];
	}

	public void setSize(int size) {
		this.size = size;
	}
	
	public void setIndex(int index) {
		this.index = index;
	}
	
	public void setPrevious(int previous) {
		this.previous = previous;
	}
	
	public void setDduks(int[] dduks) {
		this.dduks = dduks;
	}
	
	public int getSize() {
		return size;
	}
	
	public int getIndex() {
		return index;
	}
	
	public int getPrevious() {
		return previous;
	}
	
	public int[] setDduks() {
		return dduks;
	}
	
	public void plusIndex() {
		index ++;
	}
	
	public void flip(int grap) {
		int half_grap = grap / 2;
		if(grap % 2 == 1) {
			dduks[half_grap] = -dduks[half_grap];
		}
		for(int i  = 0; i < half_grap; i ++) {
			int temp = -dduks[i];
			dduks[i] = -dduks[grap - i - 1];
			dduks[grap - i - 1] = temp;
		}
		setPrevious(grap);
		index ++;
	}
	
	public void inputDduks() {
		Scanner scan = new Scanner(System.in);
		for(int i = 0; i < size; i ++) {
			dduks[i] = scan.nextInt();
			char c = scan.next().charAt(0);
			if(c == '-')
				dduks[i] = -dduks[i];
		}
	}
	
	public boolean isSorted() {
		for(int i = 0; i < size; i ++) {
			if(dduks[i] != i + 1)
				return false;
		}
		return true;
	}
	
	public boolean isBeforeSort() {
		for(int i = size - 1; i >= 0; i --) { // check bottom to top
			if(dduks[i] != i + 1) {
				for(int j = 0; j <= i; j ++) { // check top to bottom dduks can sorted in 1 flip)
					if (dduks[j] != - (i + 1 - j))
						return false;
				}
				return true;
			}
		}
		System.out.println("is already sorted");
		return false; // is already sorted
	}
	
	public void printDduk() {
		for(int i = 0; i < size; i ++) {
			System.out.print(dduks[i] + ", ");
		}
		System.out.println("index : " + index + ". previous : " + previous);
	}
	
	public void putNextDduk(dduk dduks) {
		for(int i = 0; i < size; i ++) {
			this.dduks[i] = dduks.dduks[i];
		}
		this.previous = dduks.getPrevious();
		this.index = dduks.getIndex();
	}
}

}
