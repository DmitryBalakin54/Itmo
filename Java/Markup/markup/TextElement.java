package markup;

public  interface TextElement  {
    void toMarkdown(StringBuilder str);

    void toTex(StringBuilder str);
}
