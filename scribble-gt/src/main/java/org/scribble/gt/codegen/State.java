package org.scribble.gt.codegen;

import java.util.ArrayList;
import java.util.List;


/**
 * Represents an internal state in the state machine.
 * This is typically used for SEND/SELECT operations.
 */
public class State {
    private String name;
    private List<Transition> transitions;
    private StateKind kind;
    private boolean committed;

    /**
     * Constructor for the State class.
     *
     * @param name The name of the state.
     * @param kind The kind of the state.
     */
    public State(String name, StateKind kind) {
        this.name = name;
        this.kind = kind;
        this.transitions = new ArrayList<>();
        this.committed = false;
    }

    /**
     * Returns the transitions of the state.
     *
     * @return A list of transitions.
     */
    public List<Transition> getTransitions() {
        return transitions;
    }

    /**
     * Returns the name of the state.
     *
     * @return The name of the state.
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name of the state.
     *
     * @param s The name to set.
     */
    public void setName(String s) {
        this.name = s;
    }

    /**
     * Returns the kind of the state.
     *
     * @return The kind of the state.
     */
    public StateKind getKind() {
        return kind;
    }

    /**
     * Sets the kind of the state.
     *
     * @param stateKind The kind to set.
     */
    public void setKind(StateKind stateKind) {
        this.kind = stateKind;
    }

    /**
     * Adds a transition to the state.
     *
     * @param transition The transition to be added.
     */
    public void addTransition(Transition transition){
        this.transitions.add(transition);
    }

    /**
     * Checks if the state is committed.
     *
     * @return True if the state is committed, false otherwise.
     */
    public boolean isCommitted() {
        return committed;
    }

    /**
     * Sets the committed status of the state.
     *
     * @param committed The committed status to set.
     */
    public void setCommitted(boolean committed) {
        this.committed = committed;
    }

    /**
     * Returns a string representation of the state.
     *
     * @return A string representation of the state.
     */
    @Override
    public String toString() {
        return "State{" +
                "name='" + name + '\'' +
//                ", transitions=" + transitions.toString() +
                ", kind=" + kind +
                '}';
    }
}