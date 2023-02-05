package org.scribble.core.model.global;

import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;

public interface SBuildStateHistory<T> {

    //T getHistory();

    SBuildStateHistory<T> add(Role src, EAction<StaticActionKind> a);

    SSetHistory remove(Role dst, EAction<StaticActionKind> a);

    SSetHistory syncRemove(Role r1, EAction<StaticActionKind> a);

    // Cf. sender with recursive "double" output but receiver with recursive "single" input...
    // ...(e.g., from unfold-all expansions, e.g., "unguarded choice-recs")...
    // ..."clear" is sound in some regards, but unsound in others (e.g., max buffer bounds)
    SBuildStateHistory<T> clear(Role dst, EAction<StaticActionKind> a);

    SBuildStateHistory<T> syncClear(Role r1, Role r2);
}
