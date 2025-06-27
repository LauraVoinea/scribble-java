package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.GTVRecv;
import org.scribble.ext.gt.core.type.session.GTSessType;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

public interface GTLType extends GTSessType { //<Global, GSeq>, GNode {

    int END_HASH = 9851;
    int BRANCH_HASH = 9857;
    int SELECT_HASH = 9859;
    int MIXED_CHOICE_HASH = 9871;
    int REC_HASH = 9887;
    int RECVAR_HASH = 9901;


    /* ... */

    // this merge g -- should be symmetric
    Optional<? extends GTLType> merge(GTLType t);

    // cf. s param
    default GTEFSM construct(Role r, Map<Integer, Set<Op>> com, Map<Integer, Pair<GTVRecv, GTVState>> recvStars,
                             int c, GTVState s, GTVState end) {  // c == s.c on call
        throw new RuntimeException("Shouldn't get here: " + this);
    }


    /* ... */

    GTLType subs(RecVar rv, GTLType t);


    /* ... -- n.b. formal local LTS is config LTS (hence sigma, theta etc params below) */

    //int c_TOP = -1;
    int c_TOP = GTGType.c_TOP;
    int n_INIT = 1;
}
