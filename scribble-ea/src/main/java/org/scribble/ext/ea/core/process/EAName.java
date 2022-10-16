package org.scribble.ext.ea.core.process;

import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.core.type.value.EAValTypeFactory;

public interface EAName {  // A term (has `type`)

    default EAValType type(Gamma gamma) {
        EAValType res = gamma.map.get(this);
        if (res == null) {
            throw new RuntimeException("Type error: " + gamma + " ,, " + this);
        }
        return res;
    }

    boolean canEquals(Object o);
}
