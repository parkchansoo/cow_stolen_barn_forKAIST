//package C;
import java.util.*;

public class C {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Scanner in = new Scanner(System.in);
		
		String w_ = in.nextLine();
		int w = Integer.parseInt(w_);
		String P = in.nextLine();
		
		//System.out.println(P);
		
		P = P.replaceAll("\\s+","");
		StringBuilder sb = new StringBuilder(P);
		
		int l = sb.length();
		for(int i = 0; i < l; i ++) {
			if(sb.charAt(i) == '.' || sb.charAt(i) == ';' || sb.charAt(i) == ':'
					|| sb.charAt(i) == '<' || sb.charAt(i) == '>' || sb.charAt(i) == '"'
					|| sb.charAt(i) == '{' || sb.charAt(i) == '}' || sb.charAt(i) == '/'
					|| sb.charAt(i) == '*' || sb.charAt(i) == '-' || sb.charAt(i) == '+'
					|| sb.charAt(i) == '=' || sb.charAt(i) == '!' || sb.charAt(i) == '@'
					|| sb.charAt(i) == '~' || sb.charAt(i) == '`' || sb.charAt(i) == '#'
					|| sb.charAt(i) == '$' || sb.charAt(i) == '%' || sb.charAt(i) == '^'
					|| sb.charAt(i) == '&' || sb.charAt(i) == '_' || sb.charAt(i) == '|'
					|| sb.charAt(i) == '?' || sb.charAt(i) == '\\') {
				sb.deleteCharAt(i);
				i --;
				l --;
			}
		}
		P = sb.toString();
		//System.out.println(P);
		
		if(l == 0) {
			System.out.println(-1);
			return;
		}
		
		int Plength = P.length();
		int[] cti = new int[Plength + 1]; //char to int
		Stack<Integer> checkParenthesis = new Stack<Integer>();
		int countS = 0;
		int countB = 0;
		int countL = 0;
		cti[Plength] = 0;
		
		//S = 1, B = 2, L = 3, ',' = 5, '(' = 11, ')' = 12, '[' = 31, ']' = 32
		for(int i = 0; i < Plength; i ++) {
			char curr = P.charAt(i);
			if(curr == 'S') {
				cti[i] = 1;
				countS ++;
			} else if(curr == 'B') {
				cti[i] = 2;
				countB ++;
			} else if(curr == 'L') {
				cti[i] = 3;
				countL ++;
			} else if(curr == ',') {
				cti[i] = 5;
			} else if(curr == '(') {
				cti[i] = 11;
				checkParenthesis.push(11);
			} else if(curr == '[') {
				cti[i] = 31;
				checkParenthesis.push(31);
			} else if(curr == ')') {
				cti[i] = 12;
				if(checkParenthesis.isEmpty()) {
					System.out.println(-1);
					return;
				}
				if(checkParenthesis.pop() != 11) {
						System.out.println(-1);
						//System.out.println("parenthesis error: )");
						return;
				}
			} else if(curr == ']') {
				cti[i + 1] = 32;
				if(checkParenthesis.isEmpty()) {
					System.out.println(-1);
					return;
				}
				if(checkParenthesis.pop() != 31) {
					System.out.println(-1);
					//System.out.println("parenthesis error: ]");
					return;
				}
			// different character incoming
			} else {
				System.out.println(-1);
				//System.out.println("wrong character income");
				return;
			}
		}
		
		if(cti[0] < 1 || 3 < cti[0]) {
			System.out.println(-1);
			//System.out.println("1st character error");
			return;
		}
		
		
		for(int i = 0; i < Plength; i ++) {
			if(cti[i] == 2 || cti[i] == 3) {
				if( cti[i + 1] != 11 & cti[i + 1] != 31) {
					System.out.println(-1);
					//System.out.println("next B, L is nor (, [");
					return;
				}
			}
			if(cti[i] == 1) {
				if (cti [i + 1] != 5 && cti [i + 1] != 12 && cti [i + 1] != 32 && cti[i + 1] != 0) {
					System.out.println(-1);
					//System.out.println("next S error");
					return;
				}
			}
			
			if(cti[i] == 5 || cti[i] == 11 || cti[i] == 31) {
				if (cti [i + 1] != 1 && cti [i + 1] != 2 && cti [i + 1] != 3) {
					System.out.println(-1);
					//System.out.println("next , ( [ error");
					return;
				}
			}
			if(cti[i] == 11 || cti[i] == 31) {
				if(cti [i - 1] != 2 && cti [i - 1] != 3) {
					System.out.println(-1);
					//System.out.println("before ( [ error");
					return;
				}
			}
			if(cti[i] == 12 || cti[i] == 32) {
				if(cti[i + 1] != 0 && cti[i + 1] != 5 && cti[i + 1] != 12 && cti[i + 1] != 32) {
					System.out.println(-1);
					//System.out.println("after ] ) error");
					return;
				}
			}
			if(i > 0) {
				if(cti[i] == 1 || cti[i] == 2 || cti[i] == 3) {
					if(cti[i - 1] != 5 && cti[i - 1] != 12 && cti[i - 1] != 32 && cti[i - 1] != 11 && cti[i - 1] != 31) {
						System.out.println(-1);
						//System.out.println("before S B L error");
						return;
					}
				}
			}
		}
		

		if(!checkParenthesis.isEmpty()) {
			System.out.println(-1);
			//System.out.println("parenthesis is not done");
			return;
		}
		
		//System.out.println("counting is done");
		//System.out.println(" S is " + countS + ", B is " + countB + ", L is " + countL);
		System.out.println(countL * w + countB + 1);
		//System.out.println(-1);
	}

}
