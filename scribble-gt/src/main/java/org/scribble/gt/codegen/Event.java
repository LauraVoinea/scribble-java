package org.scribble.gt.codegen;

import java.util.List;
import java.util.Objects;

public class Event {
    private String name;
    private String role;
    private EventKind kind;
    private Boolean commiting;


    public Event(EventKind kind, String name) {
        this.name = name;
        this.kind = kind;
        this.commiting = false;
    }

    public Event(EventKind kind, String name, String role) {
        this.name = name;
        this.kind = kind;
        this.role = role;
        this.commiting = false;
    }


    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public EventKind getKind() {
        return kind;
    }

    public void setKind(EventKind kind) {
        this.kind = kind;
    }

    public String getRole() {
        return role;
    }

    public void setRole(String role) {
        this.role = role;
    }

    public Boolean getCommiting() {
        return commiting;
    }

    public void setCommiting(Boolean commiting) {
        this.commiting = commiting;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Event event = (Event) o;
        return Objects.equals(name, event.name) && Objects.equals(role, event.role) && kind == event.kind && Objects.equals(commiting, event.commiting);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, role, kind, commiting);
    }

    @Override
    public String toString() {
        return "Event{" +
                "name='" + name + '\'' +
                ", role='" + role + '\'' +
                ", kind=" + kind +
                ", commiting=" + commiting +
                '}';
    }
}
