package org.scribble.gt.codegen;

import org.scribble.core.type.name.Op;
import org.scribble.ext.gt.core.type.session.local.*;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

public class FSM {
    private static final String NEWLINE = System.getProperty("line.separator");
    private String name;
    private List<State> states;
    private Map<String, State> recMap;

    private Map<State, State> mixedMap = new HashMap<State, State>();
    private int prevIndex;
    private List<State> endStates;
    private State currentState;
    private static Set<Transition> transitions;

    private Stack<State> rhs;

    private static ArrayList <State> etc;


    public static Set<Transition> getTransitions() {
        return transitions;
    }

    public FSM(String name){
        this.name = name;
        this.states = new ArrayList<State>();
        this.recMap = new HashMap<String, State>();
        this.prevIndex = 0;
        this.endStates = new ArrayList<State>();
        this.rhs = new Stack<>();
        this.etc = new ArrayList<>();
        this.transitions = new HashSet<>();
    }
    public int getPrevIndex() {
        return prevIndex;
    }

    public void setPrevIndex(int prevIndex) {
        this.prevIndex = prevIndex;
    }

    public String getName() {
        return name;
    }

    public State getCurrentState() {
        return currentState;
    }

    public void setCurrentState(State currentState) {
        this.currentState = currentState;
    }

    public void addState(State state) {
        this.states.add(state);
    }

    public List<State> getStates() {
        return this.states;
    }

    public void addRec(String recVar, State state){
        this.recMap.put(recVar, state);
    }

    public State getRec(String recVar){
        return this.recMap.get(recVar);
    }

    public boolean hasRec(String s) {
        return recMap.containsKey(s);
    }

    public List<State> getRhs() {
        return rhs;
    }

    public void setRhs(Stack<State> rhs) {
        this.rhs = rhs;
    }

    public void addEndState(State endState) {
        this.endStates.add(endState);
    }

    public List<State> getEndStates() {
        return endStates;
    }

    private boolean hasEndState() {
        return !this.endStates.isEmpty();
    }

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("FSM for role " + this.name + " : " ).append(NEWLINE);
        for (State state : states){
            s.append(state.getName() + " Kind: " + state.getKind() + " is committed "  + state.isCommited() +  " : ");
            for (Transition t : state.getTransitions()){
                s.append(" -- " + t.getEvent().getKind() + " -- " +t.getEvent().getName()
                        + "--" + t.getEvent().getCommiting() + " --> " + t.getNextState().getName() + " ; ");
            }
            s.append(NEWLINE);
        }
        return s.toString();
    }

    public void generateDOT(String outputDirectory, String dotFileName) {
        File directory = new File(outputDirectory);
        if (!directory.exists()) {
            directory.mkdirs();
        }

        try {
            FileWriter writer = new FileWriter(outputDirectory + File.separator + dotFileName);
            writer.write("digraph FSM {\n");

            // Write states
            for (State state : this.states) {
                writer.write(state.getName() + " [shape=circle];\n");
                // Write transitions
                for (Transition t : state.getTransitions()){
                    switch (t.getEvent().getKind()){
                        case SEND:
                            writer.write(t.getCurrentState().getName() + " -> " + t.getNextState().getName()
                                    + " [label=\" ! " + t.getEvent().getName() + " to " + t.getEvent().getRole() + "\"];\n");
                            break;
                        case RECEIVE:
                            writer.write(t.getCurrentState().getName() + " -> " + t.getNextState().getName()
                                    + " [label=\" ? " + t.getEvent().getName() + " from " + t.getEvent().getRole() + "\"];\n");
                            break;
                        default:
                            writer.write(t.getCurrentState().getName() + " -> " + t.getNextState().getName()
                                    + " [label=\"" + t.getEvent().getKind() + " " + t.getEvent().getName() + "\"];\n");
                    }
                }
            }

            writer.write("}\n");
            writer.flush();
            writer.close();
        } catch (IOException e) {
            System.err.println("Error generating DOT file: " + e.getMessage());
            e.printStackTrace();
        }
    }


public static void buildFsm(GTLType type, FSM fsm, Event e, Boolean RHS, Set<Op> committing){
    if (type instanceof GTLBranch){
        GTLBranch cast = (GTLBranch) type;
        State prev = fsm.getCurrentState();
        int index = fsm.getPrevIndex() + 1;
        State currentState = prev;

        // if the previous state is not committed and there are states in the rhs stack, add transitions to RHS states
        rhsTransitions(fsm, prev);

        if(!e.getKind().equals(EventKind.MIXED) && !e.getKind().equals(EventKind.REC)){
            currentState = new State("state" + index, StateKind.EXTERNAL);
            // add new transition from previous state to current state with event e
            if(e.getCommiting())
                currentState.setCommited(true);
            Transition transition = new Transition(e, prev, currentState);
            prev.addTransition(transition);
            transitions.add(transition);

            if (RHS){

                for (State s : fsm.getStates()){
                    if (!s.isCommited()) {
                        for (Transition t : s.getTransitions()) {
                            t.setNextState(currentState);
                        }
                    }
                }
            }
            fsm.addState(currentState);
            fsm.setCurrentState(currentState);
            fsm.setPrevIndex(index);
        }

        Map<Op, GTLType> cases = cast.cases;
        for(Map.Entry<Op, GTLType> entry : cases.entrySet()){
            //reset current state for each case
            fsm.setCurrentState(currentState);

            Event event = new Event(EventKind.RECEIVE, entry.getKey().toString(), cast.src.toString());
            // if the current action is in the committing set, set the event to committing
            if(committing.contains(entry.getKey()))
                event.setCommiting(true);
            //we've reached a recursive call, so we add a transition to the recursive state
            // add transitions to RHS states
            if (entry.getValue() instanceof GTLRecVar) {
                GTLRecVar recVar = (GTLRecVar) entry.getValue();
                State next = fsm.getRec(recVar.toString());
                Transition transition = new Transition(event, currentState, next);
                currentState.addTransition(transition);
                transitions.add(transition);
                //set commited for the next state according to the event
                next.setCommited(event.getCommiting());
                if(event.getCommiting()){
                    next.setCommited(true);
                    rhsTransitions(fsm, currentState);
//                    buildFsm(entry.getValue().unfoldAllOnce(), fsm, event, RHS, committing);
                    //TODO unfold if we're adding rhs transitions
                }
            } else if (entry.getValue() instanceof GTLRecursion){
                //special case for already unfolded recursion -- unfoldAll has the unfortunate effect of unfolding all
                buildFsm(entry.getValue(), fsm, event, RHS, committing);
            } else {
                buildFsm(entry.getValue().unfoldAllOnce(), fsm, event, RHS, committing);
            }
        }


    } else if (type instanceof GTLEnd) {
        State prev = fsm.getCurrentState();
        int index = fsm.getPrevIndex() + 1;
        State currentState;
        if (RHS){
            currentState = fsm.rhs.pop();
            Transition trans = new Transition(e, prev, currentState);
            prev.addTransition(trans);
            transitions.add(trans);
            currentState.setCommited(true);
            currentState.setName("state" + index);
            currentState.setKind(StateKind.TERMINAL);
            //renaming of transition events
            for (State s : etc){
                for (Transition t : s.getTransitions()){
                    if(t.getNextState().equals(currentState)){
                        t.setEvent(e);
                    }
                }
            }
        } else {
            currentState = new State("state" + index, StateKind.TERMINAL);
            Transition transition = new Transition(e, prev, currentState);
            prev.addTransition(transition);
            transitions.add(transition);

            rhsTransitions(fsm, prev);

            if(e.getCommiting())
                currentState.setCommited(true);
            // should this be here?
        }
        fsm.setCurrentState(currentState);
        fsm.addEndState(currentState);
        fsm.addState(currentState);
        fsm.setPrevIndex(index);
    } else if (type instanceof GTLMixedChoice) {
        GTLMixedChoice cast = (GTLMixedChoice) type;
        State prev = fsm.getCurrentState();
        State currentState = prev;
        rhsTransitions(fsm, prev);
        if(prev.getKind().equals(StateKind.REC))
            prev.setKind(StateKind.MIXED);

        Transition trans = new Transition(e, prev, currentState);
        prev.addTransition(trans);
        transitions.add(trans);

        if(!e.getKind().equals(EventKind.REC)) {
            int index = fsm.getPrevIndex() + 1;
            currentState = new State("state" + index, StateKind.MIXED);
            // add transition from previous state to current state with inherited event e
            if(e.getCommiting())
                currentState.setCommited(true);

            if (RHS){

                currentState = fsm.rhs.pop();
                currentState.setCommited(true);
                currentState.setName("state" + index);

                for (State s : fsm.rhs){
                    for (Transition t : s.getTransitions()){
                        t.setEvent(e);
                        t.setNextState(currentState);
                        currentState.addTransition(t);
                        transitions.add(t);
                    }
                }
            }
            fsm.setCurrentState(currentState);
            // filler event, will be overwritten
            e = new Event(EventKind.MIXED, "mixed");
            fsm.addState(currentState);
            fsm.setPrevIndex(index);

        }

        State bogus = new State("bogus" + cast.c, StateKind.MIXED);
        fsm.rhs.push(bogus);

        //traverse left
        buildFsm(cast.left.unfoldAllOnce().unfoldAllOnce(), fsm, e, false, committing);
        //traverse right
        fsm.setCurrentState(currentState);
        buildFsm(cast.right.unfoldAllOnce().unfoldAllOnce(), fsm, e, true, committing);

    } else if (type instanceof GTLRecursion){
        GTLRecursion cast = (GTLRecursion) type;
        State prev = fsm.getCurrentState();

        rhsTransitions(fsm, prev);

        if(!fsm.hasRec(String.valueOf(cast.var))) {
            int index = fsm.getPrevIndex() + 1;
            State state = new State("state" + index, StateKind.REC);

            Transition transition = new Transition(e, prev, state);
            prev.addTransition(transition);
            transitions.add(transition);

            if(e.getCommiting())
                state.setCommited(true);
            fsm.addState(state);
            fsm.setPrevIndex(index);

            if (RHS){
                for(State s : fsm.rhs){
                    for (Transition t : s.getTransitions()){
                        t.setEvent(e);
                        t.setNextState(state);
                        state.addTransition(t);
                        transitions.add(t);
                    }
                }
            }

            fsm.setCurrentState(state);
            fsm.addRec(String.valueOf(cast.var), state);

            // recursion
            // filler event
            buildFsm(cast.body.unfoldAllOnce(), fsm, new Event(EventKind.REC, cast.var.toString()), RHS, committing);
        }
    } else if (type instanceof GTLSelect) {
        GTLSelect cast = (GTLSelect) type;
        Map<Op, GTLType> cases = cast.cases;

        State prev = fsm.getCurrentState();
        int index = fsm.getPrevIndex() + 1;
        State currentState = prev;

        rhsTransitions(fsm, prev);

        // TODO Am I the observer?
        // if yes then set RHS to false

        if(!e.getKind().equals(EventKind.MIXED) && !e.getKind().equals(EventKind.REC)
                && !e.getKind().equals(EventKind.INIT))
        {
            currentState = new State("state" + index, StateKind.INTERNAL);
            Transition transition = new Transition(e, prev, currentState);
            prev.addTransition(transition);
            transitions.add(transition);

            if(e.getCommiting())
                currentState.setCommited(true);

            if (RHS){
                for (State s : fsm.rhs){
                    for (Transition t : s.getTransitions()) {
                        t.setEvent(e);
                        t.setNextState(currentState);
                        currentState.addTransition(t);
                    }
                }
            }

            fsm.addState(currentState);
            fsm.setPrevIndex(index);
        }

        for(Map.Entry<Op, GTLType> entry : cases.entrySet()){
            //reset current state for each case
            fsm.setCurrentState(currentState);

            Event event = new Event(EventKind.SEND, entry.getKey().toString(), cast.dst.toString());
            if (entry.getValue() instanceof GTLRecVar) {
                GTLRecVar recVar = (GTLRecVar) entry.getValue();
                State next = fsm.getRec(recVar.toString());
                Transition transition = new Transition(event, currentState, next);
                currentState.addTransition(transition);
                transitions.add(transition);
            } else if (entry.getValue() instanceof GTLRecursion){
                //special case for already unfolded recursion -- unfoldAll has the unfortunate effect of unfolding all
                buildFsm(entry.getValue(), fsm, event, RHS, committing);
            }
                else {
                buildFsm(entry.getValue().unfoldAllOnce(), fsm, event, RHS, committing);
            }
        }

    }
    }

    private static void rhsTransitions(FSM fsm, State prev) {
        if(!prev.isCommited() && !fsm.rhs.isEmpty()) {
            List<State> mixed;
            if (prev.getKind().equals(StateKind.MIXED)){
                mixed = fsm.rhs.subList(0, fsm.rhs.size()-1);
            }else {
                mixed = fsm.rhs;
            }
            for (State s : mixed){
                Transition t = new Transition(new Event(EventKind.MIXED, "RHS"), prev, s);
                if(!prev.getTransitions().contains(t)) {
                    prev.addTransition(t);
                    etc.add(prev);
                    transitions.add(t);
                }
            }
        }
    }


    public static FSM GTtoFsm(GTLType type, String role, Set<Op> committing) {
//        System.err.println("FSM for  " + role);

        FSM fsm = new FSM(role);
        State init = new State("init", StateKind.INIT);
        fsm.addState(init);
        fsm.setCurrentState(init);
        buildFsm(type, fsm, new Event(EventKind.INIT, "init"), false, committing);
        List<Transition> transitions = fsm.getStates().get(0).getTransitions();
        StateKind kind = null;
        for (Transition t :  transitions){
            switch (t.getEvent().getKind()){
                case SEND:
                    if(kind == StateKind.EXTERNAL)
                        kind = StateKind.MIXED;
                    else
                        kind = StateKind.INTERNAL;
                    break;
                case RECEIVE:
                    kind = StateKind.EXTERNAL;
                    if(kind == StateKind.INTERNAL)
                        kind = StateKind.MIXED;
                    else
                        kind = StateKind.EXTERNAL;
                    break;
                case MIXED:
                    fsm.getStates().get(0).setKind(StateKind.MIXED);
                    kind = StateKind.MIXED;
                    break;
                case REC:
                    if (kind == null)
                        kind = StateKind.REC;
                    break;
                case INIT:
                    if (kind == null)
                        kind = StateKind.INIT;
                    break;
            }
            
        }
        fsm.getStates().get(0).setKind(kind);

        return fsm;
    }

    public Map<State, State> getMixedMap() {
        return mixedMap;
    }

    public void setMixedMap(Map<State, State> mixedMap) {
        this.mixedMap = mixedMap;
    }
}
