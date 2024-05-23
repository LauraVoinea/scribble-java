package org.scribble.gt.codegen;

import java.util.Objects;

public class Transition {

    private Event event; // Event triggering the transition
    private State current;
    private State next;

    public Transition(Event event, State current, State next) {
        this.event = event;
        this.current = current;
        this.next = next;
    }

    public Event getEvent() {
        return event;
    }

    public void setEvent(Event event) {
        this.event = event;
    }

    public State getNextState() {
        return next;
    }

    public void setNextState(State nextState) {
        this.next = nextState;
    }

    public State getCurrentState() {
        return current;
    }

    public void setCurrentState(State nextState) {
        this.current = nextState;
    }


    @Override
    public String toString() {
        return "Transition{" +
                "event='" + event + '\'' +
                ", current=" + current.toString() +
                ", next=" + next.toString() +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Transition that = (Transition) o;
        return Objects.equals(event, that.event) && Objects.equals(current, that.current) && Objects.equals(next, that.next);
    }

    @Override
    public int hashCode() {
        return Objects.hash(event, current, next);
    }
}
