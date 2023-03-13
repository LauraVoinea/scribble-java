package org.scribble.core.model.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;

import java.util.*;


public class SSetScheduler implements SScheduler {

    public SSetScheduler() { }

    public SSetHistory newHistory() {
        return new SSetHistory(Collections.emptyMap());
    }

    // N.B. "eligible to schedule" -- not necessarily fireable
    public boolean canSchedule(
            SBuildStateHistory<?> history, Role src, EAction<StaticActionKind> a) {
        SSetHistory cast = (SSetHistory) history;
        if (!a.isSend()) {
            return true;
        }
        Map<Role, List<EAction<StaticActionKind>>> map = cast.map.get(src);
        if (map == null) {
            return true;
        }
        List<EAction<StaticActionKind>> as = map.get(a.peer);
        //return as == null || as.isEmpty();  // Corresponds to single cell buffers
        return as == null || !as.contains(a);
    }
}
