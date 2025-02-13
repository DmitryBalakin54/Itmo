package ru.itmo.wp.model.domain;

import java.time.LocalDateTime;
import java.util.Date;

public class Event {
    private Long id;
    private Long userId;
    private EventType type;
    private Date creationTime;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public EventType getType() {
        return type;
    }

    public String getTypeName() {
        return type.name();
    }

    public void setType(EventType type) {
        this.type = type;
    }

    public Date getCreationTime() {
        return creationTime;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }

    public enum EventType {
        ENTER,
        LOGOUT
    }
}
