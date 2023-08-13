package markup;

import java.util.*;

public class Strikeout extends ChangedText{
    public Strikeout(TextElement text) {
        super(text);
        ch = "~";
        beginCh = "\\textst{";
        endCh = "}";
    }

    public Strikeout(List<TextElement> list) {
        super(list);
        ch = "~";
        beginCh = "\\textst{";
        endCh = "}";
    }
}
