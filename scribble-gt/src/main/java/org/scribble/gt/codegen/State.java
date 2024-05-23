package org.scribble.gt.codegen;

import java.util.ArrayList;
import java.util.List;


// Class representing a state in the CFSM
public class State {
    private String name;
    private List<Transition> transitions;
    private StateKind kind;
    private boolean commited;

    public State(String name, StateKind kind) {
        this.name = name;
        this.kind = kind;
        this.transitions = new ArrayList<>();
        this.commited = false;
    }

    public List<Transition> getTransitions() {
        return transitions;
    }

    public String getName() {
        return name;
    }

    public StateKind getKind() {
        return kind;
    }

    public void addTransition(Transition transition){
        this.transitions.add(transition);
    }

    public boolean isCommited() {
        return commited;
    }

    public void setCommited(boolean commited) {
        this.commited = commited;
    }

    public void setKind(StateKind stateKind) {
        this.kind = stateKind;
    }


    @Override
    public String toString() {
        return "State{" +
                "name='" + name + '\'' +
//                ", transitions=" + transitions.toString() +
                ", kind=" + kind +
                '}';
    }

    public void setName(String s) {
        this.name = s;
    }
}