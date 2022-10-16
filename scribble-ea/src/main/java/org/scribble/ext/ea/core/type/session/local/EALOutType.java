package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.kind.MsgIdKind;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.local.LSend;
import org.scribble.core.type.session.local.LType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

public class EALOutType extends EALTypeIOBase {

    protected EALOutType(@NotNull Role peer,
                         @NotNull LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases) {
        super(peer, cases);
    }

    @Override
    public EALOutType concat(EALType t) {
        if (this.cases.size() != 1) {
            throw new RuntimeException("Concat only defined for unary send");
        }
        Map.Entry<Op, EAPPair<EAValType, EALType>> e = this.cases.entrySet().iterator().next();
        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases1 = new LinkedHashMap<>();
        Pair<EAValType, EALType> v = e.getValue();
        cases1.put(e.getKey(), new EAPPair<>(v.left, v.right.concat(t)));
        return EALTypeFactory.factory.out(this.peer, cases1);
    }

    /*@Override
    public EALOutType unfold() {
        return this;
    }*/

    @Override
    public EALType unfold(RecVar rvar, EALType t) {
        return new EALOutType(this.peer, unfoldCases(rvar, t));
    }

    @Override
    public Optional<EALType> step(LType a) {
        if (!(a instanceof LSend)) {
            return Optional.empty();
        }
        LSend cast = (LSend) a;
        MsgId<? extends MsgIdKind> id = cast.msg.getId();
        return stepId(this.cases, id);
    }

    protected static Optional<EALType> stepId(
            Map<Op, EAPPair<EAValType, EALType>> cases, MsgId<?> id) {
        if (!(id instanceof Op)) {
            throw new RuntimeException("TODO: " + id);
        }
        Op op = (Op) id;
        Pair<EAValType, EALType> p = cases.get(op);
        if (p == null) {
            return Optional.empty();
        }
        return Optional.of(p.right);
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
        int hash = EALType.OUT_HASH;
        hash = 31 * hash + super.hashCode();
        return hash;
    }
}
