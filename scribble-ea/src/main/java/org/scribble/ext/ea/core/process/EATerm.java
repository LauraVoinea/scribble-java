package org.scribble.ext.ea.core.process;

public interface EATerm {

    int VAR = 3779;
    int NAME = 3793;
    int UNIT = 3797;
    int LET = 3803;
    int RETURN = 3821;
    int SEND = 3823;
    int HANDLERS = 3833;

    boolean canEquals(Object o);
}
