package ru.itmo.wp.form;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class PostForm {

    @NotNull
    @NotEmpty
    @NotBlank
    @Size(min = 1, max = 50)
    private String title;

    @NotNull
    @NotEmpty
    @NotBlank
    @Size(min = 1, max = 5000)
    private String text;

    @NotNull
    @NotEmpty
    @NotBlank
    private String userLogin;

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getUserLogin() {
        return userLogin;
    }

    public void setUserLogin(String userLogin) {
        this.userLogin = userLogin;
    }
}
