package org.scribble.gt.codegen;

import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Represents a state machine for a protocol.
 */
public class StateM {
    private static int index = 0;
    private String name;
    private List<State> states;
    private State initState;
    private List<State> nonCommitedStates;

    /**
     * Constructor for StateM.
     *
     * @param string The name of the state machine.
     */
    public StateM(String string) {

    }

    /**
     * Returns the initial state of the state machine.
     *
     * @return The initial state.
     */
    private State getInitState() {
        return null;
    }

    /**
     * Returns a list of non-committed states in the state machine.
     *
     * @return A list of non-committed states.
     */
    private List<State> getNonCommitedStates() {
        return null;
    }

    /**
     * Adds a state to the state machine.
     *
     * @param root The state to be added.
     */
    private void addState(State root) {
    }

    /**
     * Translates a GTLType into a state machine.
     *
     * @param G The GTLType to be translated.
     * @param R The role for which the state machine is being generated.
     * @return The generated state machine.
     */
    public static StateM translate(GTLType G, Role R) {
        // Handle GTLBranch type
        if (G instanceof GTLBranch) {
            GTLBranch cast = (GTLBranch) G;
            List<StateM> aux = new ArrayList<StateM>();
            List<Event> events = new ArrayList<Event>();
            for (Map.Entry<Op, GTLType> entry : cast.cases.entrySet()) {
                aux.add(translate(entry.getValue(), R));
                events.add(new Event(EventKind.RECEIVE, entry.getKey().toString(), cast.src.toString()));
            }
            StateM result = new StateM(R.toString());
            State root = new State("arbitrary" + index, StateKind.EXTERNAL);
            index++;
            result.addState(root);
            for (StateM elem : aux) {
                root.addTransition(new Transition(events.get(aux.indexOf(elem)), root, elem.getInitState()));
            }
            return result;
        }
        // Handle GTLMixedChoice type
        else if (G instanceof GTLMixedChoice) {
            GTLMixedChoice cast = (GTLMixedChoice) G;
            StateM left = translate(cast.left, R);
            StateM right = translate(cast.right, R);
            StateM result = new StateM(R.toString());
            State rootRight = right.getInitState();
            Transition first = rootRight.getTransitions().get(0);
            rootRight = first.getNextState();
            Event alpha = first.getEvent();
            for (State s : left.getNonCommitedStates()) {
                s.addTransition(new Transition(alpha, s, rootRight));
            }
            return result;
        }
        return null;
    }
}