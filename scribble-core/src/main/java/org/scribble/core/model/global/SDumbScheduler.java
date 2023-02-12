package org.scribble.core.model.global;

import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;

import java.util.Collections;
import java.util.List;
import java.util.Map;


public class SDumbScheduler implements SScheduler {

    public SDumbScheduler() { }

    public SSetHistory newHistory() {
        return new SSetHistory(Collections.emptyMap());
    }

    // N.B. "eligible to schedule" -- not necessarily fireable
    public boolean canSchedule(
            SBuildStateHistory<?> history, Role src, EAction<StaticActionKind> a) {
        return true;
    }
}
