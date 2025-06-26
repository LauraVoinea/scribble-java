package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.MPrettyState;
import org.scribble.core.model.MState;
import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.name.RecVar;

import java.util.Map;
import java.util.Set;

public class GTEState extends MPrettyState<RecVar, EAction<StaticActionKind>, GTEState, Local> {  // GTEAction currently isn't an EAction

    protected GTEState(Set<RecVar> labs) {
        super(labs);
    }

    @Override
    public Map<Integer, GTEState> getReachableStates() {
        return getReachableStatesAux(this);
    }

    @Override
    public void addEdge(EAction<StaticActionKind> a, GTEState s) {
        super.addEdge(a, s);
    }

    @Override
    public int hashCode() {
        int hash = 1709;
        hash = 31 * hash + super.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTEState)) {
            return false;
        }
        return super.equals(o);  // Checks canEquals
    }

    @Override
    protected boolean canEquals(MState<?, ?, ?, ?> s) {
        return s instanceof GTEState;
    }

}
