package org.scribble.gt.codegen;

import java.util.Objects;

/**
 * Class representing an event in a transition.
 */
public class Event {
    private String name; // Is set to message label
    private String role;
    private EventKind kind;
    private Boolean committing;

    private int mc = 0;

    /**
     * Constructor for the Event class.
     *
     * @param kind The kind of the event.
     * @param name The name of the event.
     */
    public Event(EventKind kind, String name) {
        this.name = name;
        this.kind = kind;
        this.committing = false;
    }

    /**
     * Constructor for the Event class.
     *
     * @param kind The kind of the event.
     * @param name The name of the event.
     * @param role The role associated with the event.
     */
    public Event(EventKind kind, String name, String role) {
        this.name = name;
        this.kind = kind;
        this.role = role;
        this.committing = false;
    }

    public int getMc() {
        return mc;
    }

    public void setMc(int mc) {
        this.mc = mc;
    }

    /**
     * Returns the name of the event.
     *
     * @return The name of the event.
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name of the event.
     *
     * @param name The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Returns the kind of the event.
     *
     * @return The kind of the event.
     */
    public EventKind getKind() {
        return kind;
    }

    /**
     * Sets the kind of the event.
     *
     * @param kind The kind to set.
     */
    public void setKind(EventKind kind) {
        this.kind = kind;
    }

    /**
     * Returns the role associated with the event.
     *
     * @return The role associated with the event.
     */
    public String getRole() {
        return role;
    }

    /**
     * Sets the role associated with the event.
     *
     * @param role The role to set.
     */
    public void setRole(String role) {
        this.role = role;
    }

    /**
     * Returns whether the event is committing.
     *
     * @return Whether the event is committing.
     */
    public Boolean getCommitting() {
        return committing;
    }

    /**
     * Sets whether the event is committing.
     *
     * @param commiting Whether the event is committing.
     */
    public void setCommitting(Boolean commiting) {
        this.committing = commiting;
    }

    /**
     * Checks if the event is equal to another object.
     *
     * @param o The object to compare with.
     * @return True if the event is equal to the object, false otherwise.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Event event = (Event) o;
        return Objects.equals(name, event.name) && Objects.equals(role, event.role) &&
                kind == event.kind;
//        && Objects.equals(committing, event.committing);
    }

    /**
     * Returns the hash code of the event.
     *
     * @return The hash code of the event.
     */
    @Override
    public int hashCode() {
        return Objects.hash(name, role, kind, committing);
    }

    /**
     * Returns the string representation of the event.
     *
     * @return The string representation of the event.
     */
    @Override
    public String toString() {
        return "Event{" +
                "name='" + name + '\'' +
                ", role='" + role + '\'' +
                ", kind=" + kind +
                ", committing=" + committing +
                '}';
    }
}
