package org.scribble.gt.codegen;

import java.util.ArrayList;
import java.util.List;

public class CodeGenUtils {
    public static String indent(int level) {
        return " ".repeat(level * 4);
    }

    /**
     * Helper to lowercase the first letter of a string.
     */
    static String lowercaseFirstLetter(String str) {
        if (str == null || str.isEmpty()) return str;
        return str.substring(0, 1).toLowerCase() + str.substring(1);
    }

    /**
     * Helper function to return a list of all the state names in the FSM.
     */
    public static List<String> getAllStateNames(StateM fsm) {
        List<String> stateNames = new ArrayList<>();
        for (State state : fsm.getStates()) {
            stateNames.add(state.getName());
        }
        return stateNames;
    }
}
