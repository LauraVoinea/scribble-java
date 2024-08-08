package org.scribble.gt.codegen;

import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

/**
 * Represents a state machine for a protocol.
 */
public class StateM {
    private static int index = 0;
    private static Map<String, State> recMap = new HashMap<>();
    private String name;
    private Set<State> states;
    private State initState;
    private Set<State> nonCommitedStates;
    private Set<Transition> transitions;

    /**
     * Constructor for StateM.
     *
     * @param string The name of the state machine.
     */
    public StateM(String string) {
        this.name = string;
        this.states = new HashSet<>();
        this.nonCommitedStates = new HashSet<>();
        this.transitions = new HashSet<>();
    }

    /**
     * Returns the initial state of the state machine.
     *
     * @return The initial state.
     */
    private State getInitState() {
        return this.initState;
    }

    /**
     * Returns a list of non-committed states in the state machine.
     *
     * @return A list of non-committed states.
     */
    private Set<State> getNonCommitedStates() {
//        return this.nonCommitedStates;
        for (State s : this.states) {
            if (!s.isCommitted()) {
                this.nonCommitedStates.add(s);
            }
        }
        return this.nonCommitedStates;
    }

    /**
     * Adds a state to the state machine.
     *
     * @param state The state to be added.
     */
    private void addState(State state) {
        this.states.add(state);
    }

    /**
     * Sets the initial state of the state machine.
     *
     * @param state The state to be set as the initial state.
     */
    private void setInitState(State state) {
        this.initState = state;
    }

    /**
     * Adds a non-committed state to the state machine.
     *
     * @param state The state to be added.
     */
    private void addNonCommitedState(State state) {
        this.nonCommitedStates.add(state);
    }

    /**
     * Adds a list of non-committed states to the state machine.
     *
     * @param states The list of states to be added.
     */

    private void addNonCommitedStates(List states) {
        this.nonCommitedStates.addAll(states);
    }

    /**
     * Adds the states of one fsm to another fsm.
     *
     * @param fsm The fsm to be added.
     */

    private void addStateM(StateM fsm) {
        this.states.addAll(fsm.getStates());
        this.nonCommitedStates.addAll(fsm.getNonCommitedStates());
        this.transitions.addAll(fsm.getTransitions());
    }

    private Set<Transition> getTransitions() {
        return this.transitions;
    }

    /**
     * Generates a DOT file for the FSM.
     *
     * @param outputDirectory The directory to output the DOT file.
     * @param dotFileName The name of the DOT file.
     */
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

    /**
     * Translates a GTLType into a state machine.
     *
     * @param G          The GTLType to be translated.
     * @param R          The role for which the state machine is being generated.
     * @param committing
     * @return The generated state machine.
     */
    public static StateM translate(GTLType G, Role R, Set<Op> committing, Event e) {
        // Handle GTLBranch type
        if (G instanceof GTLBranch) {
            GTLBranch cast = (GTLBranch) G;
            List<StateM> fsms = new ArrayList<StateM>();
            List<Event> events = new ArrayList<Event>();
            StateM result = new StateM(R.toString());
            State root = new State("state" + index, StateKind.EXTERNAL);
            index++;

            for (Map.Entry<Op, GTLType> entry : cast.cases.entrySet()) {
                Event event = new Event(EventKind.RECEIVE, entry.getKey().toString(), cast.src.toString());
                fsms.add(translate(entry.getValue(), R, committing, event));
                events.add(event);
                if(committing.contains(entry.getKey()))
                    event.setCommitting(true);
                else{
                    event.setCommitting(false);
                }

            }


            for (StateM fsm : fsms) {
                Transition t = new Transition(events.get(fsms.indexOf(fsm)), root, fsm.getInitState());
                root.addTransition(t);
                result.getTransitions().add(t);
                if ( !events.get(fsms.indexOf(fsm)).getCommitting()) {
                    fsm.getInitState().setCommitted(false);
                } else {
                    fsm.getInitState().setCommitted(true);
                }
                result.addStateM(fsm);
            }

            result.addState(root);
            result.setInitState(root);
            System.out.println("Branch root " + root);
            System.out.println("-------------> ");

            System.out.println("Branch " + result);
            return result;
        }
        // Handle GTLMixedChoice type
        else if (G instanceof GTLMixedChoice) {
            GTLMixedChoice cast = (GTLMixedChoice) G;
            StateM leftFsm = translate(cast.left, R, committing, e);
            StateM rightFsm = translate(cast.right, R, committing, e);
            StateM result = new StateM(R.toString());

            State rootRight = rightFsm.getInitState();
            Optional<Transition> firstTransition = rootRight.getTransitions().stream().findFirst();
            rootRight = firstTransition.get().getNextState();

            Event rhsEvent = firstTransition.get().getEvent();

            if(rhsEvent.getKind().equals(EventKind.RECEIVE)) {
                for (State s : leftFsm.getNonCommitedStates()) {
                    Transition t = new Transition(rhsEvent, s, rootRight);
                    s.addTransition(t);
                    result.getTransitions().add(t);
                }
            }

            rightFsm.getStates().remove(rightFsm.getInitState());
            result.addStateM(leftFsm);
            result.addStateM(rightFsm);

            State rootLeft = leftFsm.getInitState();
            Transition t = new Transition(rhsEvent, rootLeft, rootRight);
            rootLeft.addTransition(t);
            result.setInitState(leftFsm.getInitState());
            result.getTransitions().add(t);
            return result;
        } else if (G instanceof GTLSelect) {
            GTLSelect cast = (GTLSelect) G;
            List<StateM> fsms = new ArrayList<StateM>();
            List<Event> events = new ArrayList<Event>();

            for (Map.Entry<Op, GTLType> entry : cast.cases.entrySet()) {
                Event event = new Event(EventKind.SEND, entry.getKey().toString(), cast.dst.toString());
                fsms.add(translate(entry.getValue(), R, committing, event));
                events.add(event);
            }

            StateM result = new StateM(R.toString());

            State root = new State("state" + index, StateKind.INTERNAL);
            index++;
            result.addState(root);
            result.setInitState(root);
            for (StateM fsm : fsms) {
                Transition t = new Transition(events.get(fsms.indexOf(fsm)), root, fsm.getInitState());
                root.addTransition(t);
                result.addStateM(fsm);
                result.getTransitions().add(t);

            }
            System.out.println("Select ?? " + result);
            return result;
        } else if (G instanceof GTLEnd) {
            StateM result = new StateM(R.toString());
            State endState = new State("state" + index, StateKind.TERMINAL);
            index++;
            result.addState(endState);
            result.setInitState(endState);
            return result;
        } else if (G instanceof GTLRecursion) {
            GTLRecursion cast = (GTLRecursion) G;
            StateM result = new StateM(R.toString());
            StateM fsm = translate(cast.body, R, committing, e);
//            if (e.getCommitting()){
//                fsm = translate(cast.body, R, committing, e);
//            } else {
//                fsm = translate(cast.unfoldAllOnce(), R, committing, e);
//            }

//            System.out.println("Recursion body: " + cast.body);
//            System.out.println("Recursion unfold: " + cast.unfoldAllOnce());

            if(recMap.containsKey(String.valueOf(cast.var))) {
                State recState = recMap.get(String.valueOf(cast.var));
//                System.out.println("Recursion ()()()(): recMap State" + recState + " init state " + fsm.getInitState());
                for (Transition t : fsm.getTransitions()){
                    if (t.getNextState().equals(recState)) {
                        t.setNextState(fsm.getInitState());
                    }
                }
                fsm.getStates().remove(recState);
            }
            result.setInitState(fsm.getInitState());
            result.addState(fsm.getInitState());
            result.addStateM(fsm);
            return result;
        } else if (G instanceof GTLRecVar) {
            GTLRecVar cast = (GTLRecVar) G;
            StateM result = new StateM(R.toString());
            State root = new State("state" + index, StateKind.REC);
            index++;
            recMap.put(String.valueOf(cast.var), root);
            result.addState(root);
            result.setInitState(root);
            return result;
        }
        return null;
    }

    private void addStates(List states) {
        this.states.addAll(states);
    }

    private Set<State> getStates() {
        return this.states;
    }

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("FSM for role ").append(this.name).append(" : ").append("\n");
        for (State state : this.states) {
            s.append(state.getName()).append(" Kind: ").append(state.getKind())
                    .append(" is committed ").append(state.isCommitted()).append(" : ");
            for (Transition t : state.getTransitions()) {
                s.append(" -- ").append(t.getEvent().getKind()).append(" -- ")
                        .append(t.getEvent().getName()).append("--").append(t.getEvent().getCommitting())
                        .append(" --> ").append(t.getNextState().getName()).append(" ; ");
            }
            s.append("\n");
        }
        return s.toString();
    }


    public static StateM stateRenaming(StateM fsm){
        int i = 1;
        for (State s : fsm.getStates()){
            s.setName("state" + i);
            i++;
        }
        return fsm;
    }
}