package org.scribble.ext.ea.core.type.session.local;

import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.session.local.LSend;
import org.scribble.core.type.session.local.LType;
import org.scribble.ext.ea.core.type.EAType;

import java.util.Optional;

public interface EALType extends EAType {

    int IO_HASH = 11003;
    int IN_HASH = 11027;
    int OUT_HASH = 11047;
    int REC_HASH = 11057;
    int END_HASH = 11059;

    EALType concat(EALType t);
    EALType unfold(RecVar rvar, EALType t);

    Optional<EALType> step(LType a);
}
