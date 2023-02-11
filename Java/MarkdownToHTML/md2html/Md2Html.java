package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;

public  class Md2Html {
    public static void main(String[] args) {
        //convert("fileIn.txt", "fileOut.txt");
        convert(args[0], args[1]);
    }

    static void convert(String fileIn, String fileOut) {
        try {
            ConvertWriter convert = new ConvertWriter(new BufferedReader( new InputStreamReader(
                    new FileInputStream(fileIn), "utf8")),
                    new FileWriter(fileOut, StandardCharsets.UTF_8));
            try {
                convert.printConvertAll();
            } catch (IOException e) {
                System.err.println(e.getMessage());
            } finally {
                convert.close();
            }
        } catch (FileNotFoundException e) {
            System.err.println("File " + fileIn + " not founded: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Unsupported encoding in file " + fileIn + " or +" +
                    fileOut + ": " + e.getMessage());
        }
    }

}