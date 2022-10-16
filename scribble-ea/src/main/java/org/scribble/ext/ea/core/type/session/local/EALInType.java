package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.kind.MsgIdKind;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.local.LRecv;
import org.scribble.core.type.session.local.LType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;

import java.util.LinkedHashMap;
import java.util.Optional;

public class EALInType extends EALTypeIOBase {

    protected EALInType(@NotNull Role peer,
                     @NotNull LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases) {
        super(peer, cases);
    }

    @Override
    public EALInType concat(EALType t) {
        throw new RuntimeException("Concat not defined for receive");
    }

    /*@Override
    public EALInType unfold() {
        return this;
    }*/

    @Override
    public EALType unfold(RecVar rvar, EALType t) {
        return new EALInType(this.peer, unfoldCases(rvar, t));
    }

    @Override
    public Optional<EALType> step(LType a) {
        if (!(a instanceof LRecv)) {
            return Optional.empty();
        }
        LRecv cast = (LRecv) a;
        MsgId<? extends MsgIdKind> id = cast.msg.getId();
        return EALOutType.stepId(this.cases, id);
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
        int hash = EALType.IN_HASH;
        hash = 31 * hash + super.hashCode();
        return hash;
    }
}
