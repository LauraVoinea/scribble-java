package org.scribble.ext.ea.core.type.session.local;

import org.scribble.core.type.session.local.LSend;
import org.scribble.core.type.session.local.LType;
import org.scribble.ext.ea.core.type.EAType;

import java.util.Optional;

public interface EALType extends EAType {

    int IO = 11003;
    int IN = 11027;
    int OUT = 11047;
    int END = 11057;

    EALType concat(EALType t);

    Optional<EALType> step(LType a);
}
