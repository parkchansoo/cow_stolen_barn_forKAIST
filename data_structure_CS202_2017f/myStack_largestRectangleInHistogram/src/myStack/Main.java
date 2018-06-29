package myStack;

public class Main {

    public static int TestCaseNum = 0;
    public static int TestSuccCnt = 0;

    public static void main(String[] args) {
        // give test case
        // test1
        double[] hist = new double[] {10, 1.5, 6, 7.5, 8, 5.5, 2, 9};
        String eVal = "22.0";
        String fVal = "" + getLargestRectArea(hist);
        printTestResult(eVal, fVal);
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

    public static double getLargestRectArea(double[] histogram) {
        double ret = 0;
        int histSize = histogram.length;
        myStack<Integer> s = new myStack<Integer>();
        int[] lBound = new int[histSize];
        int[] rBound = new int[histSize];

        for(int i = 0; i < histSize; i ++) {
            if(s.isEmpty()) {
                lBound[i] = -1;
                s.push(i);
                System.out.println("pushed::" + i);
            } else {
                while(!s.isEmpty() && histogram[s.top()] >= histogram[i])
                    rBound[s.pop()] = i;
                if(s.isEmpty()) {
                   lBound[i] = -1;
                } else {
                    lBound[i] = s.top();
                }
                s.push(i);
                System.out.println("pushed::" + i);
            }
        }
        while(!s.isEmpty())
            rBound[s.pop()] = histSize;

        /*
        System.out.println(printArr(lBound));
        System.out.println(printArr(rBound));
        */

        for(int i = 0; i < histSize; i ++ ) {
            rBound[i] = rBound[i] - lBound[i] - 1;
        }
        // System.out.println(printArr(rBound));

        double temp;
        for(int i = 0; i < histSize; i ++ ) {
            if((temp = (double)rBound[i] * histogram[i]) > ret)
                ret = temp;
        }
        return ret;
    }
}
