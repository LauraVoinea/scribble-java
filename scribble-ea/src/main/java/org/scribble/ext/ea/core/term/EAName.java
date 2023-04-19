package org.scribble.ext.ea.core.term;

import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVType;

public interface EAName {  // A term (has `type`)

    default EAVType type(Gamma gamma) {
        EAVType res = gamma.map.get(this);
        if (res == null) {
            throw new RuntimeException("Type error: " + gamma + " ,, " + this);
        }
        return res;
    }

    boolean canEquals(Object o);
}
