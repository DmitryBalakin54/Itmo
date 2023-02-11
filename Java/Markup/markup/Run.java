package markup;


import java.util.List;

public class Run {
    public static void main(String[] args) {
        StringBuilder str = new StringBuilder();
        Paragraph paragraph = new Paragraph(new Strong(new Text("ggg")));
        paragraph.toTex(str);
        System.out.println(str);
    }
}
