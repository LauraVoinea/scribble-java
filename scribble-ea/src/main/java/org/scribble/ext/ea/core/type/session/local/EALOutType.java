package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class EALOutType extends EALTypeIOBase {

    protected EALOutType(@NotNull Role peer,
                         @NotNull LinkedHashMap<Op, Pair<EAValType, EALType>> cases) {
        super(peer, cases);
    }

    @Override
    public EALOutType concat(EALType t) {
        if (this.cases.size() != 1) {
            throw new RuntimeException("Concat only defined for unary send");
        }
        Map.Entry<Op, Pair<EAValType, EALType>> e = this.cases.entrySet().iterator().next();
        LinkedHashMap<Op, Pair<EAValType, EALType>> cases1 = new LinkedHashMap<>();
        Pair<EAValType, EALType> v = e.getValue();
        cases1.put(e.getKey(), new EAPPair<>(v.left, v.right.concat(t)));
        return EALTypeFactory.factory.out(this.peer, cases1);
    }

    @Override
    public boolean isOut() {
        return true;
    }

    @Override
    public String symbol() {
        return "!";
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);  // Does class check and canEquals
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EALOutType;
    }

    @Override
    public int hashCode() {
        int hash = EALType.OUT;
        hash = 31 * hash + super.hashCode();
        return hash;
    }
}
