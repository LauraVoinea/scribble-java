package org.scribble.gt.codegen;

/**
 * Enum representing the different kinds of events in a state machine.
 */
public enum EventKind {
    /**
     * Represents a send event in the state machine.
     */
    SEND,
    /**
     * Represents a receive event in the state machine.
     */
    RECEIVE,
    /**
     * Represents a recursive event in the state machine.
     */
    REC,
    /**
     * Represents the initial event in the state machine.
     */
    INIT,
    /**
     * Represents a mixed event in the state machine.
     */
    MIXED_SEND,
    MIXED_RECEIVE,
}
