package org.scribble.core.model.global;

import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;

public class SBuildState {

    public final SState state;
    public final SBuildStateHistory<?> history;

    public SBuildState(SState state, SBuildStateHistory<?> history) {
        this.state = state;
        this.history = history;
    }

    public SBuildState add(Role src, EAction<StaticActionKind> a, SState succ) {
        return new SBuildState(succ, this.history.add(src, a));
    }

    public SBuildState remove(Role dst, EAction<StaticActionKind> a, SState succ) {
        return new SBuildState(succ, this.history.remove(dst, a));
    }

    public SBuildState syncRemove(Role r1, EAction<StaticActionKind> a, SState succ) {
        return new SBuildState(succ, this.history.syncRemove(r1, a));
    }

    // Cf. sender with recursive "double" output but receiver with recursive "single" input...
    // ...(e.g., from unfold-all expansions, e.g., "unguarded choice-recs")...
    // ..."clear" is sound in some regards, but unsound in others (e.g., max buffer bounds)
    public SBuildState clear(Role dst, EAction<StaticActionKind> a, SState succ) {
        return new SBuildState(succ, this.history.clear(dst, a));
    }

    public SBuildState syncClear(Role r1, Role r2, SState succ) {
        return new SBuildState(succ, this.history.syncClear(r1, r2));
    }

    @Override
    public String toString() {
        return "(" + this.state.toString() + "::" + this.history + ")";
    }

    public boolean semanticEquals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SBuildState)) {
            return false;
        }
        SBuildState them = (SBuildState) o;
        return this.state.semanticEquals(them.state)  // !!!
                && this.history.equals(them.history);
    }

    /*public int semanticHash() {
        int hash = 31121;
        hash = 31 * hash + this.state.semanticHash();  // !!!
        hash = 31 * hash + this.history.hashCode();
        return hash;
    }*/

    @Override
    public int hashCode() {
        int hash = 31121;
        hash = 31 * hash + this.state.hashCode();
        hash = 31 * hash + this.history.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SBuildState)) {
            return false;
        }
        SBuildState them = (SBuildState) o;
        return this.state.equals(them.state) && this.history.equals(them.history);
    }
}
