package org.scribble.ext.ea.core.process;

import java.util.Map;

public interface EAPTerm {

    int VAR = 3779;
    //int PROCESS_ID = 3793;
    int UNIT = 3797;
    int LET = 3803;
    int RETURN = 3821;
    int SEND = 3823;
    int HANDLERS = 3833;
    int SUSPEND = 3847;

    int IDLE = 3851;
    int ACTIVE_THREAD = 3853;
    int CONFIG = 3863;

    boolean canEquals(Object o);
}
