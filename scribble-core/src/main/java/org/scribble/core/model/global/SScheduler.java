package org.scribble.core.model.global;

import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;

import java.util.*;


// HERE HERE generalise history to "action ids" (not just message queue non-empty, and also not just ops (cf. non det))


public class SScheduler {

    public SScheduler() { }

    // N.B. "eligible to schedule" -- not necessarily fireable
    public boolean canSchedule(
            Map<Role, Map<Role, List<EAction>>> history, Role src, EAction a) {
        //return true;
        //*
        if (!a.isSend()) {
            return true;
        }
        Map<Role, List<EAction>> map = history.get(src);
        if (map == null) {
            return true;
        }
        List<EAction> as = map.get(a.peer);
        return as == null || as.isEmpty();
        //*/
    }
}
