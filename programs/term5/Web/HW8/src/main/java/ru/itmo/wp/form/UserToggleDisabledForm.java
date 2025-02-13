package ru.itmo.wp.form;

import javax.validation.constraints.NotNull;

public class UserToggleDisabledForm {
    @NotNull
    private long id;

    @NotNull
    private boolean disabled;

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public boolean getDisabled() {
        return disabled;
    }

    public void setDisabled(boolean disabled) {
        this.disabled = disabled;
    }
}
