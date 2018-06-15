import java.util.*;

public class BB {
	public static int ans;
	public static int[] threePow = new int[16];
	public static int[] visit = new int[43046721];
	public static int[] fpos = new int[2];
	public static int operation = 0;

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		int[][] board = {{0, 0, 0, 0},
						{0, 0, 0, 0},
						{0, 0, 0, 0},
						{0, 0, 0, 0}
						};
		
		ans = 0;
		int threeVal = 1;
		for(int i = 0; i < 16; i ++) {
			threePow[i] = threeVal;
			threeVal *= 3;
		}
		
		Scanner in = new Scanner(System.in);
		int px = in.nextInt();
		int py = 0;
		int finalx = in.nextInt();
		int finaly = in.nextInt();
		fpos[0] = finalx - 1;
		fpos[1] = finaly - 1;
		px --;
		in.close();
		
		
		int[] status = {0, 0, 0, 0};
		dfs(board, px, py, status, 1);
		System.out.println(ans);
	}
	
	public static void dfs(int[][] board, int px, int py, int[] status, int turn) {
		board[px][py] = turn;
		status[px] ++;
		
		int indexed = indexing(board);

		if(visit[indexed] == 1) {
			board[px][py] = 0;
			status[px] --;
			return;
		}
		visit[indexed] = 1;
		if(px == fpos[1] && py == fpos[0]) {
			if(gameset(board, px, py) == 2) {
				ans ++;
			}
			board[px][py] = 0;
			status[px] --;
			return;
		} else {
			if(gameset(board, px, py) == turn) {
				board[px][py] = 0;
				status[px] --;
				return;
			}
		}
		
		for(int i = 0; i < 4; i ++) {
			if(status[i] < 4) {
				dfs(board, i, status[i], status, 3 - turn);
			}
		}
		
		board[px][py] = 0;
		status[px] --;
		return;
	}
	
	public static int indexing(int[][] board) {
		int countIndex = 1;
		int ret = 0;
		for(int i = 0; i < 4; i ++) {
			for(int j = 0; j < 4; j ++) {
				ret += board[i][j]*countIndex;
				countIndex *= 3;
			}
		}
		return ret;
	}
	
	public static void printS(int[] status) {
		System.out.print("(");
		for(int i = 0; i < 4; i ++) {
			System.out.print(status[i] + ", ");
		}
		System.out.println(")");
	}
	
	public static int gameset(int[][] board, int px, int py) {
		//search through x- axis
		int x = px;
		int y = py;
		int count = 0;
		x --;
		while(x > -1 && x < 4 && board[x][y] == board[px][py]) {
			x --; count ++;
		}
		if(count > 1) return board[px][py];
		
		x = px;
		x ++;
		while(x > -1 && x < 4 && board[x][y] == board[px][py]) {
			x ++; count ++;
		}
		if(count > 1) return board[px][py];
		
		//search through y-axis
		count = 0;
		x = px;
		y --;
		while(y > -1 && board[x][y] == board[px][py]) {
			y --; count ++;
		}
		if(count > 1) return board[px][py];		
		
		//search through x, y diagonal /
		count = 0;
		y = py;
		x ++; y ++;
		while(x > -1 && x < 4 && y > -1 && y < 4 && board[x][y] == board[px][py]) {
			x ++; y ++; count ++;
		}
		if(count > 1) return board[px][py];
				
		x = px; y = py;
		x --; y --;
		while(x > -1 && x < 4 && y > -1 && y < 4 && board[x][y] == board[px][py]) {
			x --; y --; count ++;
		}
		if(count > 1) return board[px][py];
		
		//search through x, -y diagonal
		count = 0;
		x = px; y = py;
		x ++; y --;
		while(x > -1 && x < 4 && y > -1 && y < 4 && board[x][y] == board[px][py]) {
			x ++; y --; count ++;
		}
		if(count > 1) return board[px][py];
				
		x = px; y = py;
		x --; y ++;
		while(x > -1 && x < 4 && y > -1 && y < 4 && board[x][y] == board[px][py]) {
			x --; y ++; count ++;
		}
		if(count > 1) return board[px][py];
		
		return 0;
	}

	
}
