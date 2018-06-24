package balloonGame;

public class Main {
    public static int TestCaseNum = 0;
    public static int TestSuccCnt = 0;

    public static void main(String args[]) {
        String testRet = "";
        String testAns = "";
        /* Test cases */
        // test1
        testAns = "1-2";
        testRet = ballonCounting(2, 2, new int[] {});
        printTestResult(testAns, testRet);

        // test2
        testAns = "1-2-3-4";
        testRet = ballonCounting(2, 2, new int[] {2});
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

    public static String ballonCounting(int size, int M, int[] boom) {
        String ret = "";
        return ret;
    }
}
