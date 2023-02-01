package org.scribble.core.model.global;

import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;

import java.util.*;
import java.util.stream.Collectors;

public class SBuildState {

    public final SState state;
    public final Map<Role, Map<Role, List<EAction>>> history;

    public SBuildState(SState state, Map<Role, Map<Role, List<EAction>>> history) {
        this.state = state;
        this.history = Collections.unmodifiableMap(cloneHistory(history));
    }

    private static Map<Role, Map<Role, List<EAction>>> cloneHistory(
            Map<Role, Map<Role, List<EAction>>> history) {
        return history.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> x.getValue().entrySet().stream().collect(Collectors.toMap(
                        Map.Entry::getKey,
                        y -> y.getValue().stream().collect(Collectors.toList())
                ))
        ));
    }

    public SBuildState add(Role src, EAction a, SState succ) {
        Map<Role, Map<Role, List<EAction>>> history = cloneHistory(this.history);  // TODO optimise, factor out clone with constructor
        Map<Role, List<EAction>> map = history.get(src);
        if (map == null) {
            map = new HashMap<>();
            history.put(src, map);
        }
        List<EAction> as = map.get(a.peer);
        if (as == null) {
            as = new LinkedList<>();
            map.put(a.peer, as);
        }
        as.add(a);
        return new SBuildState(succ, history);
    }

    public SBuildState clear(Role dst, EAction a, SState succ) {
        Map<Role, Map<Role, List<EAction>>> history = cloneHistory(this.history);  // TODO optimise, factor out clone with constructor
        Map<Role, List<EAction>> map = history.get(a.peer);
        if (map == null) {  // CHECKME Should be impossible?
            return new SBuildState(succ, this.history);
        }
        List<EAction> as = map.get(dst);
        if (as == null) {  // CHECKME Should be impossible?
            return new SBuildState(succ, this.history);
        }
        as.clear();
        return new SBuildState(succ, history);
    }

    public SBuildState syncClear(Role r1, Role r2, SState succ) {
        Map<Role, Map<Role, List<EAction>>> history = cloneHistory(this.history);  // TODO optimise, factor out clone with constructor
        Map<Role, List<EAction>> map = history.get(r1);
        if (map != null) {
            List<EAction> as = map.get(r2);
            if (as != null) {
                if (as.size() > 1) {
                    System.out.println("-----------------\n");
                }
                as.clear();
            }
        }
        Map<Role, List<EAction>> map2 = history.get(r2);
        if (map2 != null) {
            List<EAction> as2 = map2.get(r1);
            if (as2 != null) {
                if (as2.size() > 1) {
                    System.out.println("-----------------\n");
                }
                as2.clear();
            }
        }
        return new SBuildState(succ, history);
    }

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
