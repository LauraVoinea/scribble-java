package org.scribble.gt.codegen;

/**
 * Enum representing the different kinds of states in a state machine.
 */
public enum StateKind {
        /**
         * Represents a terminal state in the state machine.
         */
        TERMINAL,
        /**
         * Represents an internal state in the state machine.
         * This is used for SEND/SELECT
         */
        INTERNAL,
        /**
         * Represents an external state in the state machine.
         * This is used for RECEIVE/BRANCH
         */
        EXTERNAL,
        /**
         * Represents a mixed state in the state machine.
         * This is used for MIXED CHOICE
         */
        MIXED,
        MIXED_INTERNAL,
        /**
         * Represents a mixed state in the state machine.
         * This is used for MIXED CHOICE
         */
        MIXED_EXTERNAL,
        /**
         * Represents a recursive state in the state machine.
         * This is used in the construction of the state machine.
         */
        /**
         * Represents a recursive state in the state machine.
         * This is used in the construction of the state machine.
         */
        REC,

        /**
         * Represents an initial state in the state machine.
         */
        INIT,

}
