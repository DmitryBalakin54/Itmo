package markup;

import java.util.ArrayList;
import java.util.List;

public  abstract  class ChangedText implements TextElement {
    protected  String ch;
    protected String beginCh;
    protected String endCh;
    private final List<TextElement> list;

    protected ChangedText(TextElement text) {
        this.list = new ArrayList<>(List.of(text));
    }

    protected ChangedText(List<TextElement> list) {
        this.list = new ArrayList<>(list);
    }

    @Override
    public final void toMarkdown(StringBuilder str) {
        str.append(ch);
        for (TextElement text : list) {
            text.toMarkdown(str);
        }
        str.append(ch);
    }

    @Override
    public final void toTex(StringBuilder str) {
        str.append(beginCh);
        for (TextElement text : list) {
            text.toTex(str);
        }
        str.append(endCh);
    }
}
