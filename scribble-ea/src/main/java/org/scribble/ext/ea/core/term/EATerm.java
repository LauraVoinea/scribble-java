package org.scribble.ext.ea.core.term;

// (Pure) Values/Expressions (V, W) and Computations (M, N)
public interface EATerm {

    int FUNC_NAME = 1609;  // TODO move

    int HANDLER = 1613;

    int VAR = 3779;
    //int PROCESS_ID = 3793;
    int UNIT = 3797;
    int INT = 28807;
    int BOOL = 28813;
    int HANDLERS = 3833;
    int BIN_OP = 51673;

    int LET = 3803;
    int RETURN = 3821;
    int SEND = 3823;
    int SUSPEND = 3847;
    int REC = 3851;
    int APP = 3853;
    int IF = 3863;

    // Move to EAConfig
    int IDLE = 4481;
    int ACTIVE_THREAD = 4483;
    int ACTOR = 4493;

    boolean canEquals(Object o);
}
