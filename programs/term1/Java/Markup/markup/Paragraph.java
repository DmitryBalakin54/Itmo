package markup;

import java.util.*;

public class Paragraph implements TextElement{

    private final  List<TextElement> list;

    Paragraph(TextElement text) {
        this.list = new ArrayList<>(List.of(text));
    }

    public Paragraph(List<TextElement> list) {
        this.list = new ArrayList<>(list);
    }
    @Override
    public final void toMarkdown(StringBuilder str) {
        for (TextElement text : list) {
            text.toMarkdown(str);
        }
    }

    @Override
    public final void toTex(StringBuilder str) {
        for (TextElement text : list) {
            text.toTex(str);
        }
    }
}
