package markup;

import java.util.*;

public class Emphasis extends ChangedText {
    public Emphasis(TextElement text) {
        super(text);
        ch = "*";
        beginCh = "\\emph{";
        endCh = "}";
    }
    public Emphasis(List<TextElement> list) {
        super(list);
        ch = "*";
        beginCh = "\\emph{";
        endCh = "}";
    }
}
