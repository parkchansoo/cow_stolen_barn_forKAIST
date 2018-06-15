import java.util.*;
import java.io.*;

public class Bloxorx {
    public static void main(String[] args) {
        String fileName = args[0];

        try {
            Scanner scanner = new Scanner(new FileInputStream(fileName));

            int lineNum = 0;
            String printing = "";

            while(scanner.hasNextLine()){
                String line = scanner.nextLine();
                printing += line;
                lineNum ++;
            }

            scanner.close();
            System.out.println("total line count was " + lineNum);
            System.out.println(printing);


        } catch(FileNotFoundException ex) {
            System.out.println("Unable to open file '" + fileName + "'");
        }
    }
}
//Users/parkchansoo/IdeaProjects/algo_pp1