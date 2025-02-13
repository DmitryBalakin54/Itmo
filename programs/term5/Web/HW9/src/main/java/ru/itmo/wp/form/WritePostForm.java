package ru.itmo.wp.form;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

public class WritePostForm {

    @Pattern(regexp = "[a-zA-Z ]+", message = "Tags must only contain letters and spaces.")
    private String tagsString;


    @NotBlank
    @NotNull
    private String title;

    @NotBlank
    @NotNull
    private String text;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getTagsString() {
        return tagsString;
    }

    public void setTagsString(String tagsString) {
        this.tagsString = tagsString;
    }
}
