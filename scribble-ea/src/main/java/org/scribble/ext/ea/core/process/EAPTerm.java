package org.scribble.ext.ea.core.process;

import java.util.Map;

public interface EAPTerm {

    int VAR = 3779;
    int NAME = 3793;
    int UNIT = 3797;
    int LET = 3803;
    int RETURN = 3821;
    int SEND = 3823;
    int HANDLERS = 3833;
    int SUSPEND = 3847;

    boolean canEquals(Object o);
}
