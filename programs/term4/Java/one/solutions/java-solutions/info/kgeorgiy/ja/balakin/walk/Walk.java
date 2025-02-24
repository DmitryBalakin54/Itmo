package info.kgeorgiy.ja.balakin.walk;

import java.nio.file.*;
import java.io.*;
import java.nio.charset.StandardCharsets;


public class Walk {
    private static final String EMPTY_HASH = "0".repeat(8);

    public static void main(String[] args) {
        if (args == null) {
            System.err.println("empty arguments");
            return;
        }

        if (args.length < 2) {
            System.err.println("incorrect number of arguments");
            return;
        }

        if (args[0] == null || args[1] == null) {
            System.err.println("incorrect arguments");
            return;
        }

        try {
            Path in = Path.of(args[0]);

            try {
                Path out = Path.of(args[1]);

                try {
                    Path pathToOut;
                    if ((pathToOut = out.getParent()) != null) {
                        Files.createDirectories(pathToOut);
                    }

                } catch (IOException e) {
                    System.err.println("Cannot create directory: " + e.getMessage());
                    return;
                }

                try (BufferedReader reader = Files.newBufferedReader(in, StandardCharsets.UTF_8)){

                    try(BufferedWriter writer = Files.newBufferedWriter(out, StandardCharsets.UTF_8)) {
                        String path;
                        while ((path = reader.readLine()) != null) {
                            writer.write(res(path) + System.lineSeparator());
                        }
                    }

                } catch (AccessDeniedException e){
                    System.err.println("Access denied: " + e.getMessage());
                } catch (FileNotFoundException | NoSuchFileException e) {
                    System.err.println("No such file: " + e.getMessage());
                } catch (InvalidPathException e) {
                    System.err.println("Invalid path in input file: " + e.getMessage());
                } catch (IOException e) {
                    System.err.println("IOException: " + e.getMessage());
                }

            } catch (InvalidPathException e) {
                System.err.println("Invalid path for output file: " + e.getMessage());
            }

        } catch (InvalidPathException e) {
            System.err.println("Invalid path for input file: " + e.getMessage());
        }
    }

    static String res(String pathString) {
        try {
            Path path = Path.of(pathString);

            try (FileInputStream stream = new FileInputStream(path.toFile())) {
                int hash = JHashFromStream(stream);
                return toHex(hash) + " " + path;

            } catch (IOException e) {
                System.err.println("IOException: " + path);
                return EMPTY_HASH + " " + path;
            }

        } catch (InvalidPathException e) {
            System.err.println("Invalid path: " + e.getMessage());
            return EMPTY_HASH + " " + pathString;
        }
    }

    public static String toHex(int i) {
        String res = Integer.toHexString(i);
        if (res.length() < 8) {
            res = "0".repeat(8 - res.length()) + res;
        }

        return res;
    }
    public static int JHashFromStream(InputStream s) throws IOException{
        int hash = 0;

        byte[] key = new byte[1024];
        int cnt;
        while ((cnt = s.read(key)) != -1) {
            for (int i = 0; i < cnt; i++) {
                hash += (key[i] & 0xFF);
                hash += (hash << 10);
                hash ^= (hash >>> 6);
            }
        }

        hash += (hash << 3);
        hash ^= (hash >>> 11);
        hash += (hash << 15);
        return hash;
    }
}