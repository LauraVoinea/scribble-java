package org.scribble.gt.codegen;

import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.stream.Stream;

/**
 * Represents a state machine for a protocol.
 */
public class StateM {
    private static int index;
    private static Map<String, State> recMap = new HashMap<>();
    private String name;
    private TreeSet<State> states;
    private State initState;
    private Set<State> nonCommitedStates;
    private LinkedHashSet<Transition> transitions;

    private static int mcCounter = 0;

    /**
     * Constructor for StateM.
     *
     * @param string The name of the state machine.
     */
    public StateM(String string, int index) {
        this.name = string;
        this.states = new TreeSet<>();
        this.nonCommitedStates = new HashSet<>();
        this.transitions = new LinkedHashSet<>();
        this.index = index;
    }

    public static Map<String, State> getRecMap() {
        return recMap;
    }

    /**
     * Returns the initial state of the state machine.
     *
     * @return The initial state.
     */
    State getInitState() {
        return this.initState;
    }

    /**
     * Returns a list of non-committed states in the state machine.
     *
     * @return A list of non-committed states.
     */
    private Set<State> getNonCommitedStates() {
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

    LinkedHashSet<Transition> getTransitions() {
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

        try (FileWriter writer = new FileWriter(outputDirectory + File.separator + dotFileName)) {
            writer.write("digraph FSM {\n");

            for (State state : this.states) {
                String shape = switch (state.getKind()) {
                    case TERMINAL -> "doublecircle";
                    case MIXED_EXTERNAL, MIXED_INTERNAL -> "Mcircle,style=filled";
                    default -> "circle";
                };
                writer.write(state.getName() + "[shape=" + shape + "];\n");

                for (Transition t : state.getTransitions()) {
                    String label = getTransitionLabel(t);
                    writer.write(t.getCurrentState().getName() + " -> " + t.getNextState().getName() + label + ";\n");
                }
            }

            writer.write("}\n");
        } catch (IOException e) {
            System.err.println("Error generating DOT file: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private String getTransitionLabel(Transition t) {
        return switch (t.getEvent().getKind()) {
            case SEND -> " [label=\"" + t.getEvent().getRole() + " ! " + t.getEvent().getName() + "\"]";
            case RECEIVE -> " [label=\"" + t.getEvent().getRole() + " ? " + t.getEvent().getName() + "\"]";
            case MIXED_RECEIVE -> " [style=dotted, label=\"" + t.getEvent().getRole() + " ? " + t.getEvent().getName() + "\"]";
            case MIXED_SEND -> " [style=dotted, label=\"" + t.getEvent().getRole() + " ! " + t.getEvent().getName() + "\"]";
            default -> " [label=\"" + t.getEvent().getName() + "\"]";
        };
    }

    /**
     * Translates a GTLType into a state machine.
     *
     * @param G          The GTLType to be translated.
     * @param R          The role for which the state machine is being generated.
     * @param committing
     * @return The generated state machine.
     */
    public static StateM translate(GTLType G, Role R, Set<Op> committing, Event e, int currentIndex) {

        // Handle GTLBranch type
        if (G instanceof GTLBranch) {
            GTLBranch cast = (GTLBranch) G;
            List<StateM> fsms = new ArrayList<>();
            List<Event> events = new ArrayList<>();
            StateM resultFsm = new StateM(R.toString(), currentIndex);
            State rootState = new State("state" + currentIndex, StateKind.EXTERNAL);

            // inherit committing property from the parent event
            if (e != null && !e.getCommitting()) {
                resultFsm.addNonCommitedState(rootState);
            }

            for (Map.Entry<Op, GTLType> entry : cast.cases.entrySet()) {
                Op label = entry.getKey();
                GTLType continuation = entry.getValue();
                Event event = new Event(EventKind.RECEIVE, label.toString(), cast.src.toString());
                events.add(event);
                if(committing.contains(label))
                    event.setCommitting(true);
                else{
                    event.setCommitting(false);
                }
//                rootState.addGcEvent(event);
                StateM fsm = translate(continuation, R, committing, event, currentIndex + 1);
                currentIndex = fsm.getIndex();
                fsms.add(fsm);

            }


            for (StateM fsm : fsms) {
                Transition t = new Transition(events.get(fsms.indexOf(fsm)), rootState, fsm.getInitState());
                rootState.addTransition(t);
                resultFsm.getTransitions().add(t);
                if ( !events.get(fsms.indexOf(fsm)).getCommitting()) {
                    fsm.getInitState().setCommitted(false);
                    //add the gc events of root state to the fsm init state
                    fsm.addNonCommitedState(fsm.getInitState());
                } else {
                    fsm.getInitState().setCommitted(true);

                    // the gc list should be empty for committed states
                    fsm.getInitState().getGcEvents().clear();
                }

                resultFsm.addStateM(fsm);
            }

            resultFsm.addState(rootState);
            resultFsm.setInitState(rootState);
            resultFsm.setIndex(currentIndex);
            return resultFsm;
        }
        // Handle GTLMixedChoice type
        // add gc events
        else if (G instanceof GTLMixedChoice) {
            GTLMixedChoice cast = (GTLMixedChoice) G;
            StateM leftFsm = translate(cast.left, R, committing, e, currentIndex);
            currentIndex = leftFsm.getIndex();
            StateM rightFsm = translate(cast.right, R, committing, e, currentIndex );
            currentIndex = rightFsm.getIndex();
            StateM result = new StateM(R.toString(), currentIndex);
            State rootRight = rightFsm.getInitState();
            Optional<Transition> firstTransition = rootRight.getTransitions().stream().findFirst();
            rootRight = firstTransition.get().getNextState();


            // Collect labels from both sides of the mixed choice
            Map<String, List<Event>> Labels = new HashMap<>();
            Labels.put("lhs", new ArrayList<>());
            Labels.put("rhs", new ArrayList<>());
            mcCounter++;
            collectLabels(leftFsm, Labels.get("lhs"), mcCounter);
            collectLabels(rightFsm, Labels.get("rhs"), mcCounter);

            Event rhsEvent = firstTransition.get().getEvent();
            // remove the initial state and the first transition from the right FSM
            rightFsm.getStates().remove(rightFsm.getInitState());
            rightFsm.getTransitions().remove(firstTransition.get());

            result.addStateM(leftFsm);
            result.addStateM(rightFsm);

            State rootLeft = leftFsm.getInitState();


            Transition t = new Transition(rhsEvent, rootLeft, rootRight);
            rootLeft.addTransition(t);
            result.getTransitions().add(t);

            if(rhsEvent.getKind().equals(EventKind.RECEIVE)) {
                for (State s : leftFsm.getNonCommitedStates()) {
                    Transition rhs = new Transition(rhsEvent, s, rootRight);
                    s.addTransition(rhs);
                    result.getTransitions().add(rhs);
                }

                Labels.get("rhs").remove(rhsEvent);

                rhsEvent.setKind(EventKind.MIXED_RECEIVE);
                rootLeft.setKind(StateKind.MIXED_EXTERNAL);


            } else {
                // if first event in rhs is a send event we're in the observer
                rhsEvent.setKind(EventKind.MIXED_SEND);
                rootLeft.setKind(StateKind.MIXED_INTERNAL);

            }

            // if rootLeft is committed, then the result fsm is committed.
            if(rootLeft.isCommitted()) {
                result.nonCommitedStates.clear();
                rootLeft.getGcEvents().clear();
            } else {
                result.addNonCommitedState(rootLeft);
                // add the gc events to all states in the left fsm
                for (State s : leftFsm.getStates()) {
                    s.setGcEvents(new LinkedHashSet<>(Labels.get("rhs")));
                }

            }
            for (State s : rightFsm.getStates()) {
                s.setGcEvents(new LinkedHashSet<>(Labels.get("lhs")));
            }

            result.setInitState(leftFsm.getInitState());
            result.setIndex(currentIndex);
            return result;
        } else if (G instanceof GTLSelect) {
            GTLSelect cast = (GTLSelect) G;
            List<StateM> fsms = new ArrayList<>();
            List<Event> events = new ArrayList<>();
            StateM result = new StateM(R.toString(), currentIndex);

            State root = new State("state" + currentIndex, StateKind.INTERNAL);
            result.addState(root);
            result.setInitState(root);

            for (Map.Entry<Op, GTLType> entry : cast.cases.entrySet()) {
                Event event = new Event(EventKind.SEND, entry.getKey().toString(), cast.dst.toString());
                StateM  fsm = translate(entry.getValue(), R, committing, event, currentIndex + 1);
                currentIndex = fsm.getIndex();
                fsms.add(fsm);
                events.add(event);
            }

            if (e != null && !e.getCommitting()) {
                result.addNonCommitedState(root);
            }

            for (StateM fsm : fsms) {
                Transition t = new Transition(events.get(fsms.indexOf(fsm)), root, fsm.getInitState());
                root.addTransition(t);
                result.addStateM(fsm);
                result.getTransitions().add(t);

            }
            result.setIndex(currentIndex);
            return result;
        } else if (G instanceof GTLEnd) {
            StateM result = new StateM(R.toString(), currentIndex);
            State endState = new State("state" + currentIndex, StateKind.TERMINAL);
            result.addState(endState);
            result.setInitState(endState);
            result.setIndex(currentIndex);
            return result;
        } else if (G instanceof GTLRecursion) {
            GTLRecursion cast = (GTLRecursion) G;
            StateM result = new StateM(R.toString(), currentIndex);
            StateM fsm = translate(cast.body, R, committing, e, currentIndex);

            if(recMap.containsKey(String.valueOf(cast.var))) {
                State recState = recMap.get(String.valueOf(cast.var));
                for (Transition t : fsm.getTransitions()){
                    if (t.getNextState().equals(recState)) {
                        t.setNextState(fsm.getInitState());
                    }
                }

                fsm.getStates().remove(recState);
                // add the gc events of the rec state to the fsm init state
                fsm.getInitState().getGcEvents().addAll(recState.getGcEvents());
                LinkedHashSet<Event> gcEvents = new LinkedHashSet<>();
                for (State s : fsm.getStates()) {
//                    s.setGcEvents(new LinkedHashSet<>(recState.getGcEvents()));
                    //collect all gc events from the fsm states
                    gcEvents.addAll(s.getGcEvents());
                }
                fsm.getInitState().setGcEvents(gcEvents);
                //add gc events to each fsm state that is External or Mixed_External or Mixed_Internal
                for (State s : fsm.getStates()) {
                        s.setGcEvents(gcEvents);

                }
            }
            result.setInitState(fsm.getInitState());
            result.addStateM(fsm);
            result.setIndex(currentIndex);
            return result;
        } else if (G instanceof GTLRecVar) {
            GTLRecVar cast = (GTLRecVar) G;
            StateM result = new StateM(R.toString(), currentIndex);
            State root = new State("state" + currentIndex, StateKind.REC);
            recMap.put(String.valueOf(cast.var), root);
            result.setInitState(root);
            result.setIndex(currentIndex);
            return result;
        }
        return null;
    }

    private void setIndex(int currentIndex) {
        this.index = currentIndex;
    }

    private int getIndex() {
        return this.index;
    }

    public Set<State> getStates() {
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
            s.append(" \n GC events: ").append(state.getGcEvents().size());
            for (Event e : state.getGcEvents()) {
                s.append(" \n GC:").append(e.getName()).append(" MC " + e.getMc()).append("; ");
            }
            s.append("\n");
        }
        return s.toString();
    }



    public String getName() {
        return this.name;
    }

    public StateM rename() {
        StateM result = new StateM(this.name, 0);
        State root = new State("state" + 0, StateKind.INIT);
        Transition t = new Transition(new Event(EventKind.INIT, "init", "init"), root, this.getInitState());
        result.setInitState(root);
        root.addTransition(t);
        result.addStateM(this);
        result.addState(root);
        mcCounter = 0;
        return result;
    }


    /**
     * Collects lhs and rhs labels from a mixed choice.
     *
     * @param G The GTLType to be checked.
     * @return A map containing lhs and rhs labels.
     */
    public static Map<String, List<Event>> collectMixedChoiceLabels(GTLType G, Set<Op> committing, int mcCounter) {
        Map<String, List<Event>> labels = new HashMap<>();
        labels.put("lhs", new ArrayList<>());
        labels.put("rhs", new ArrayList<>());

        if (G instanceof GTLMixedChoice) {
            GTLMixedChoice cast = (GTLMixedChoice) G;
            mcCounter++;
            collectLabels(cast.left, labels.get("lhs"), committing, mcCounter);
            collectLabels(cast.right, labels.get("rhs"), committing, mcCounter);
        }

        return labels;
    }

    private static void collectLabels(GTLType G, List<Event> labels, Set<Op> committing, int mcCounter) {
        if (G instanceof GTLBranch) {
            GTLBranch cast = (GTLBranch) G;
            for (Op label : cast.cases.keySet()) {
                Event event = new Event(EventKind.RECEIVE, label.toString(), cast.src.toString());
                event.setMc(mcCounter);
                labels.add(event);
                collectLabels(cast.cases.get(label), labels, committing, mcCounter);
            }
        } else if (G instanceof GTLSelect) {
            GTLSelect cast = (GTLSelect) G;
            for (Op label : cast.cases.keySet()) {
                //if label in committing then skip it
                if(!committing.contains(label))
                    collectLabels(cast.cases.get(label), labels, committing, mcCounter);
            }
        } else if (G instanceof GTLMixedChoice) {
            GTLMixedChoice cast = (GTLMixedChoice) G;
            collectLabels(cast.left, labels, committing, mcCounter + 1);
            collectLabels(cast.right, labels, committing, mcCounter + 1);
        } else if (G instanceof GTLRecursion) {
            GTLRecursion cast = (GTLRecursion) G;
            collectLabels(cast.body, labels, committing, mcCounter);
        }
    }

    private static void collectLabels(StateM fsm, List<Event> labels, int mcCounter) {
        for (State s : fsm.getStates()) {
            for (Transition t : s.getTransitions()){
                t.getEvent().setMc(mcCounter);
                if(t.getEvent().getKind().equals(EventKind.RECEIVE)
                        || t.getEvent().getKind().equals(EventKind.MIXED_RECEIVE)) {
//                    if (!t.getEvent().getCommitting()) {
                        labels.add(t.getEvent());
//                    }
                }
            }
        }
    }

    /**
     * Resets the MC counter to zero.
     */
    public static void resetMcCounter() {
        mcCounter = 0;
    }

}