package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.MState;
import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.endpoint.actions.ERecv;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.type.name.RecVar;

import java.util.List;
import java.util.Set;

public class GTEMixedState extends GTEState {

    protected GTEMixedState(Set<RecVar> labs) {
        super(labs);
    }

    public EAction<StaticActionKind> getLeft() {
        return getDetActions().get(0);
    }

    public EAction<StaticActionKind> getRight() {
        return getDetActions().get(1);
    }

    @Override
    public GTEStateKind getStateKind() {
        List<EAction<StaticActionKind>> as = getDetActions();
        if (as.stream().filter(x -> !x.mid.toString().startsWith("*")).count() != 1
                || as.size() < 2) {
            throw new RuntimeException("CHECKME: " + as);
        }
        EAction<StaticActionKind> right = getRight();
        if (right instanceof ESend<?>) {
            if (getLeft() instanceof ERecv<?>) {
                return GTEStateKind.INTERNAL_MIXED;
            }
            throw new RuntimeException("CHECKME: " + as);
        } else if (right instanceof ERecv<?>) {
            //if (getLeft() instanceof ESend<?>) {
            return GTEStateKind.EXTERNAL_MIXED;  // !!! left either send/receive
            //}
        }
        throw new RuntimeException("CHECKME: " + as);
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 7187;
        hash = 31 * hash + super.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTEMixedState)) {
            return false;
        }
        return super.equals(o);  // Checks canEquals
    }

    @Override
    protected boolean canEquals(MState<?, ?, ?, ?> s) {
        return s instanceof GTEMixedState;
    }
}
