package org.scribble.ext.ea.core.process;

import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.util.Pair;

import java.util.Map;

// !!! "terms" vs. "term typing"
public interface EAPTerm {

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

    int IDLE = 4481;
    int ACTIVE_THREAD = 4483;
    int CONFIG = 4493;

    boolean canEquals(Object o);
}
