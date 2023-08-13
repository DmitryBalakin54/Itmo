package markup;

public class Text implements TextElement {

    private String string;

    public Text(String string) {
        this.string = string;
    }

    @Override
    public void toMarkdown(StringBuilder str) {
        str.append(string);
    }

    @Override
    public void toTex(StringBuilder str) {
        str.append(string);
    }
}
