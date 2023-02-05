package org.scribble.core.model.global;

import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;

import java.util.Collections;


public interface SScheduler {

    SBuildStateHistory<?> newHistory();

    // N.B. "eligible to schedule" -- not necessarily fireable
    boolean canSchedule(SBuildStateHistory<?> history, Role src, EAction<StaticActionKind> a);
}
