package md2html;

import java.io.*;
import java.util.*;

public final class ConvertWriter {
    private final BufferedReader reader;
    private final Writer writer;
    private StringBuilder str;
    private StringBuilder paragraph;
    private int headerMd;
    private final String SHIELD_SYMBOL = "<>";

    private  Map<String, String> beginSymbol;
    private  Map<String, String> endSymbol;
    private  Map<String, String> symbol;

    public ConvertWriter(BufferedReader reader, Writer writer) {
        this.reader = reader;
        this.writer = writer;
       // headerMd = 0;
        makeMaps();
    }

    private void makeMaps() {
        symbol = new LinkedHashMap<>();
        symbol.put("&", "&amp;");
        symbol.put("<", "&lt;");
        symbol.put(">", "&gt;");

        beginSymbol = new LinkedHashMap<>();
        beginSymbol.put("++", "<u>");
        beginSymbol.put("__", "<strong>");
        beginSymbol.put("**", "<strong>");
        beginSymbol.put("`", "<code>");
        beginSymbol.put("--", "<s>");
        beginSymbol.put("_", "<em>");
        beginSymbol.put("*", "<em>");


        endSymbol = new LinkedHashMap<>();
        endSymbol.put("++", "</u>");
        endSymbol.put("__", "</strong>");
        endSymbol.put("**", "</strong>");
        endSymbol.put("`", "</code>");
        endSymbol.put("--", "</s>");
        endSymbol.put("_", "</em>");
        endSymbol.put("*", "</em>");

    }

    private boolean hasNextLine() {
        try {
            String string = reader.readLine();
            if (string == null) {
                return false;
            } else {
                str = new StringBuilder(string);
                return true;
            }
        } catch (IOException e) {
            return false;
        }
    }

    private boolean missWhitespace() {
        while (hasNextLine()) {
            if (!str.toString().isEmpty()) {
                return true;
            }
        }
        return false;
    }

    private StringBuilder convertParagraph() {
        if (!missWhitespace()) {
            return null;
        }

        paragraph = new StringBuilder();

        do {
            if (str.isEmpty()) {
                break;
            }
           paragraph.append(str);
           paragraph.append('\n');
        } while (hasNextLine());
        paragraph.deleteCharAt(paragraph.length() - 1);

        changeSymbols();
        shield();
        change();
        reShield();
        if (!header()) {
            paragraph();
        }
        return paragraph;
    }

    public boolean printConvertParagraph() throws IOException {
        if (convertParagraph() == null) {
            return false;
        }
        writer.write(paragraph.toString());
        return true;
    }
    private boolean header() {
        int index = 0;
        while (paragraph.charAt(index++) == '#') {}
        index--;
        if (index > 0) {
            if (paragraph.charAt(index) == ' ') {
                paragraph.delete(0, index + 1);
                paragraph.insert(0, "<h" + index + ">");
                paragraph.append("</h" + index + ">");
                //headerMd = index;
                return true;
            }
        }
        return false;
    }

    private void changeSymbols() {
        for (Map.Entry<String, String> entry : symbol.entrySet()) {
            int index = -1;
            while ((index = paragraph.indexOf(entry.getKey(), index + 1)) >= 0) {
                paragraph.deleteCharAt(index);
                paragraph.insert(index, entry.getValue());
            }
        }
    }

    private void shield() {
        int index = -1;
        while ((index = paragraph.indexOf("\\", index + 1) ) >= 0) {
            paragraph.delete(index, index + 1);
            paragraph.insert(index + 1, SHIELD_SYMBOL);
            paragraph.insert(index, SHIELD_SYMBOL);
        }
    }
    private void reShield() {
        int index = -1;
        while ((index = paragraph.indexOf(SHIELD_SYMBOL, index + 1) ) >= 0) {
            paragraph.delete(index, index + SHIELD_SYMBOL.length());
        }
    }

private void change() {
    for (Map.Entry<String, String> entry : beginSymbol.entrySet()) {
        int indexBegin = -1;
        int indexEnd;
        while ((indexBegin = paragraph.indexOf(entry.getKey(), indexBegin + 1)) >= 0) {
            if ((indexEnd = paragraph.indexOf(entry.getKey(), indexBegin + 1)) >= 0) {
                try {
                    if (!(paragraph.substring(indexBegin - SHIELD_SYMBOL.length(), indexBegin).equals(SHIELD_SYMBOL) &&
                            paragraph.substring(indexBegin + 1, indexBegin + 1 + SHIELD_SYMBOL.length()).equals(SHIELD_SYMBOL)) &&
                            !(paragraph.substring(indexEnd - SHIELD_SYMBOL.length(), indexEnd).equals(SHIELD_SYMBOL) &&
                                    paragraph.substring(indexEnd + 1, indexEnd + 1 + SHIELD_SYMBOL.length()).equals(SHIELD_SYMBOL))) {

                        paragraph.delete(indexEnd, indexEnd + entry.getKey().length());
                        paragraph.insert(indexEnd, endSymbol.get(entry.getKey()));
                        paragraph.delete(indexBegin, indexBegin + entry.getKey().length());
                        paragraph.insert(indexBegin, beginSymbol.get(entry.getKey()));
                    }
                } catch (StringIndexOutOfBoundsException e) {
                    paragraph.delete(indexEnd, indexEnd + entry.getKey().length());
                    paragraph.insert(indexEnd, endSymbol.get(entry.getKey()));
                    paragraph.delete(indexBegin, indexBegin + entry.getKey().length());
                    paragraph.insert(indexBegin, beginSymbol.get(entry.getKey()));
                }
            }
        }
    }
 }

    private void paragraph() {
        paragraph.insert(0, "<p>");
        paragraph.append("</p>");
    }

    private void replace() {

    }

    public boolean printConvertAll() throws IOException {
        if (convertParagraph() == null) {
            return false;
        }
        writer.write(paragraph.toString());
        while (convertParagraph() != null) {
            writer.write('\n');
            writer.write(paragraph.toString());
        }
        return true;
    }

    public void close() throws IOException{
        reader.close();
        writer.close();
    }

}
