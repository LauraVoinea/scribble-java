package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.GTVRecv;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

// !!! No "fid"
public class GTLEnd implements GTLType {

    public static final GTLEnd END = new GTLEnd();

    protected GTLEnd() { }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        return t.equals(END) ? Optional.of(END) : Optional.empty();
    }

    @Override
    public GTEFSM construct(Role r, Map<Integer, Set<Op>> com, Map<Integer, Pair<GTVRecv, GTVState>> recvStars,
                            int c, GTVState s, GTVState end) {
        // CHECKME draw recvStars?
        return new GTEFSM(Set.of(end), end, Set.of(), Set.of(), Map.of());
    }


    /* ... */

    @Override
    public GTLType subs(RecVar rv, GTLType t) {
        return this;
    }

    @Override
    public String toString() {
        return "end";
    }


    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.END_HASH;
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLEnd)) { return false; }
        GTLEnd them = (GTLEnd) obj;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLEnd;
    }






}
