package org.scribble.ext.ea.core.type.session.local;

import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.session.local.LSend;
import org.scribble.core.type.session.local.LType;
import org.scribble.ext.ea.core.type.EAType;

import java.util.Map;
import java.util.Optional;

public interface EALType extends EAType {

    int IO_HASH = 11003;
    int IN_HASH = 11027;
    int OUT_HASH = 11047;
    int REC_HASH = 11057;
    int END_HASH = 11059;
    int RECVAR_HASH = 11069;

    //boolean wellFormed();  // TODO bound rec labels, no self send -- operationally OK if async

    EALType concat(EALType t);

    EALType subs(Map<RecVar, EALRecType> map);

    EALType unfoldAllOnce();

    @Deprecated
    EALType unfold(RecVar rvar, EALType t);

    Optional<EALType> step(LType a);

    // !!! currently not used consistently (added ad hoc from testing)
    // Currently only unfolding -- so currently symmetric
    // found is expr, required is usually pre
    static void subtype(EALType found, EALType required) {
        if (!(found.equals(required) || found.unfoldAllOnce().equals(required.unfoldAllOnce()))) {
            throw new RuntimeException("Incompatible pre type:\n"
                    + "\tfound=" + found + ", required=" + required);
        }
    }
}
