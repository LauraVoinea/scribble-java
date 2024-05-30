package org.scribble.gt.codegen;

import java.util.Objects;


/**
 * Class representing a transition in a state machine.
 */
public class Transition {

    private Event event; // Event triggering the transition
    private State current;
    private State next;

    /**
     * Constructor for the Transition class.
     *
     * @param event The event triggering the transition.
     * @param current The current state.
     * @param next The next state.
     */
    public Transition(Event event, State current, State next) {
        this.event = event;
        this.current = current;
        this.next = next;
    }

    /**
     * Returns the event triggering the transition.
     *
     * @return The event triggering the transition.
     */
    public Event getEvent() {
        return event;
    }

    /**
     * Sets the event triggering the transition.
     *
     * @param event The event to set.
     */
    public void setEvent(Event event) {
        this.event = event;
    }

    /**
     * Returns the next state of the transition.
     *
     * @return The next state.
     */
    public State getNextState() {
        return next;
    }

    /**
     * Sets the next state of the transition.
     *
     * @param nextState The next state to set.
     */
    public void setNextState(State nextState) {
        this.next = nextState;
    }

    /**
     * Returns the current state of the transition.
     *
     * @return The current state.
     */
    public State getCurrentState() {
        return current;
    }

    /**
     * Sets the current state of the transition.
     *
     * @param nextState The current state to set.
     */
    public void setCurrentState(State nextState) {
        this.current = nextState;
    }

    /**
     * Returns a string representation of the transition.
     *
     * @return A string representation of the transition.
     */
    @Override
    public String toString() {
        return "Transition{" +
                "event='" + event + '\'' +
                ", current=" + current.toString() +
                ", next=" + next.toString() +
                '}';
    }

    /**
     * Checks if the transition is equal to another object.
     *
     * @param o The object to compare with.
     * @return True if the transition is equal to the object, false otherwise.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Transition that = (Transition) o;
        return Objects.equals(event, that.event) && Objects.equals(current, that.current) && Objects.equals(next, that.next);
    }

    /**
     * Returns the hash code of the transition.
     *
     * @return The hash code of the transition.
     */
    @Override
    public int hashCode() {
        return Objects.hash(event, current, next);
    }
}
