package balloonGame;

public class Main {
    public static int TestCaseNum = 0;
    public static int TestSuccCnt = 0;

    /* Test our function goes well */
    public static void main(String args[]) {
        String testRet = "";
        String testAns = "";
        /* Test cases */
        // test1
        testAns = "1-2";
        testRet = balloonCounting(2, 2, new int[] {});
        printTestResult(testAns, testRet);

        // test2
        testAns = "1-2-3-4";
        testRet = balloonCounting(2, 2, new int[] {2});
        printTestResult(testAns, testRet);

        // test3
        testAns = "4-2-5-7-6-3-1";
        testRet = balloonCounting(5, 3, new int[] {2});
        printTestResult(testAns, testRet);

        // test4
        testAns = "5-3-4-2-8-7-9-6-1-11-13-10-12";
        testRet = balloonCounting(7, 11, new int[] {1, 4, 6});
        printTestResult(testAns, testRet);

        // test5
        testAns = "8-5-2-9-6-12-7-1-14-4-13-3-11-10";
        testRet = balloonCounting(10, 7, new int[] {1, 5});
        printTestResult(testAns, testRet);

        /* End of test, print test sumup */
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


    /* our target function to evaluate balloon game result */
    public static String balloonCounting(int size, int M, int[] boom) {
        String ret = "";
        LinkedBalloonList lbl = new LinkedBalloonList();
        lbl.setBalloonList(size, boom);
        // checking lbl setting
        System.out.println("*method: balloonCounting - test [setBalloonList]");
        System.out.println("LinkedBalloonList: " + lbl.printLBL());
        System.out.println("size: " + lbl.getSize());
        System.out.println("BoomList: " + lbl.printBoomList());

        lbl.rotate(1);
        M = M -1;
        while(!lbl.isEmpty()) {
            lbl.rotate(M);
            if(lbl.getHead().isBoom()) {
                ret += lbl.boom();
                lbl.rotate(1);
            }
            else
                ret += lbl.remove();

            if(!lbl.isEmpty())
                ret += "-";
            System.out.println("debugging while loop::: " + lbl.printLBL());
        }
        return ret;
    }
}
