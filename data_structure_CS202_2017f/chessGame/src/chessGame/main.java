package chessGame;

public class Main {
    static int TestCaseNum = 0;
    static int TestSuccCnt = 0;

    public static void main(String[] args) {

        /* testing myQueue DS */
        myQueue<Integer> queue = new myQueue<>();
        for (int i = 0; i < 101; i ++) {
            queue.enqueue(i);
        }
        for (int i = 0; i < 101; i ++) {
            System.out.print(queue.dequeue() + " - ");
        }
        System.out.println("");

        /* Test cases */
        String eval = "";
        String fval = "";

        // TestCase1
        fval += minimunKnightMove(new int[] {1, 2}, new int[] {4, 5}, 5);
        eval = "2";
        printTestResult(eval, fval);

        // TestCase2
        fval = "" + minimunKnightMove(new int[] {1, 1}, new int[] {4, 5}, 5);
        eval = "3";
        printTestResult(eval, fval);

        // TestCase3
        fval = "" + minimunKnightMove(new int[] {5, 7}, new int[] {15, 20}, 22);
        eval = "9";
        printTestResult(eval, fval);

        // TestCase4
        fval = "" + minimunKnightMove(new int[] {1, 1}, new int[] {2, 2}, 3);
        eval = "-1";
        printTestResult(eval, fval);

        testResultSumup();
    }

    public static void printTestResult(String eVal, String fVal) {
        /* calling print function == test case added */
        TestCaseNum ++;
        System.out.print("Test case" + TestCaseNum + " Result : ");

        if(eVal.equals(fVal)) {
            System.out.println("* success! *");
            TestSuccCnt ++;
        }
        else {
            System.out.println("- failed; -");
        }

        System.out.println("    Expected value was (" + eVal + ")");
        System.out.println("    Function Output is (" + fVal + ")\n");
    }

    public static void testResultSumup() {
        System.out.println("==================================================");
        System.out.println("Test : " + TestSuccCnt + "/" + TestCaseNum + ".");
    }

    public static String printArr(int[] arr) {
        String ret = "";
        int len = arr.length;
        for(int i = 0; i < len; i++) {
            ret += arr[i] + "-";
        }
        return ret;
    }

    public static int minimunKnightMove(int[] startPos, int[] targetPos, int N) {
        int ret = 0;
        myQueue<myNode> moves = new myQueue<>();x
        startPos[0] --;
        startPos[1] --;
        targetPos[0] --;
        targetPos[1] --;
        // initializing board history
        int[][] board = new int[N][N];
        for(int i = 0; i < N; i ++) {
            for(int j = 0; j < N; j ++) {
                board[i][j] = 0;
            }
        }
        boolean flag = false;
        int currX = startPos[0];
        int currY = startPos[1];
        int step = 0;
        board[currX][currY] = 1;
        myNode currStat = new myNode(currX, currY, step);

        moves.enqueue(currStat);
        int[][] movable = {
                        {1, 2},
                        {1, -2},
                        {2, 1},
                        {2, -1},
                        {-1, 2},
                        {-1, -2},
                        {-2, 1},
                        {-2, -1}    };
        while(!moves.isEmpty() && !flag) {
            currStat = moves.dequeue();
            for(int i = 0; i < 8; i ++ ) {
                currX = currStat.x + movable[i][0];
                currY = currStat.y + movable[i][1];
                if(currX == targetPos[0] && currY == targetPos[1]) {
                    step = currStat.step + 1;
                    flag = true;
                    /* you can use for watching how Knight are moving
                     * System.out.println("curr....x: " + currX  + "/ y: " + currY + " / step: " + step);
                     */
                    break;
                }
                if( (currX > -1 && currX < N) && (currY > -1 && currY < N) && (board[currX][currY] == 0)) {
                    board[currX][currY] = 1; // recording that knight already stepped
                    step = currStat.step + 1;
                    myNode nextStat = new myNode(currX, currY, step);
                    moves.enqueue(nextStat);
                    /* you can use for watching how Knight are moving
                     * System.out.println("curr....x: " + currX  + "/ y: " + currY + " / step: " + step);
                     */
                }
            }
        }
        return flag ? step : -1;
    }
}
