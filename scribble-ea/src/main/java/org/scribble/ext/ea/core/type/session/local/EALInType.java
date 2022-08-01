package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;

public class EALInType extends EALTypeIOBase {

    protected EALInType(@NotNull Role peer,
                     @NotNull LinkedHashMap<Op, Pair<EAValType, EALType>> cases) {
        super(peer, cases);
    }

    @Override
    public EALInType concat(EALType t) {
        throw new RuntimeException("Concat not defined for receive");
    }

    @Override
    public boolean isIn() {
        return true;
    }

    @Override
    public String symbol() {
        return "?";
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);  // Does class check and canEquals
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EALInType;
    }

    @Override
    public int hashCode() {
        int hash = EALType.IN;
        hash = 31 * hash + super.hashCode();
        return hash;
    }
}
