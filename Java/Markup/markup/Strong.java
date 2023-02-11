package markup;

import java.util.*;

public class Strong extends ChangedText{
    public Strong(TextElement text) {
        super(text);
        ch = "__";
        beginCh = "\\textbf{";
        endCh = "}";
    }
    public Strong(List<TextElement> list) {
        super(list);
        ch = "__";
        beginCh = "\\textbf{";
        endCh = "}";
    }
}
