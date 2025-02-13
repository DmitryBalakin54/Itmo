package ru.itmo.web.hw4.model;

public class User {
    private long id;
    private String handle;
    private String name;
    private Color color;

    public enum Color {
        RED, GREEN, BLUE;
    }

    public User(long id, String handle, String name, Color color) {
        this.id = id;
        this.handle = handle;
        this.name = name;
        this.color = color;
    }


    public long getId() {
        return id;
    }

    public String getHandle() {
        return handle;
    }

    public String getName() {
        return name;
    }

    public Color getColor() {
        return color;
    }
}
