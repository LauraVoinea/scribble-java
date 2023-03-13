package org.scribble.core.model.global;

import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;

import java.util.*;
import java.util.stream.Collectors;

// TODO rename List -- not Set
public class SSetHistory implements SBuildStateHistory<Map<Role, Map<Role, List<EAction<StaticActionKind>>>>> {

    public final Map<Role, Map<Role, List<EAction<StaticActionKind>>>> map;

    public SSetHistory(Map<Role, Map<Role, List<EAction<StaticActionKind>>>> history) {
        this.map = Collections.unmodifiableMap(cloneHistory(history));
    }

    private static Map<Role, Map<Role, List<EAction<StaticActionKind>>>> cloneHistory(
            Map<Role, Map<Role, List<EAction<StaticActionKind>>>> history) {
        return history.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> x.getValue().entrySet().stream().collect(Collectors.toMap(
                        Map.Entry::getKey,
                        y -> y.getValue().stream().collect(Collectors.toList())
                ))
        ));
    }

    /*@Override
    public Map<Role, Map<Role, List<EAction<StaticActionKind>>>> getHistory() {
        return this.map;
    }*/

    @Override
    public SSetHistory add(Role src, EAction<StaticActionKind> a) {
        Map<Role, Map<Role, List<EAction<StaticActionKind>>>> copy = cloneHistory(this.map);  // TODO optimise, factor out clone with constructor
        Map<Role, List<EAction<StaticActionKind>>> get = copy.computeIfAbsent(src, k -> new HashMap<>());
        List<EAction<StaticActionKind>> as = get.computeIfAbsent(a.peer, k -> new LinkedList<>());
        as.add(a);
        return new SSetHistory(copy);
    }

    // a is, e.g., rcv action -- a.peer is sender
    @Override
    public SSetHistory remove(Role dst, EAction<StaticActionKind> a) {
        Map<Role, Map<Role, List<EAction<StaticActionKind>>>> copy = cloneHistory(this.map);  // TODO optimise, factor out clone with constructor
        Map<Role, List<EAction<StaticActionKind>>> get = copy.get(a.peer);
        if (get == null) {  // CHECKME Should be impossible?
            return this;
        }
        List<EAction<StaticActionKind>> as = get.get(dst);
        if (as == null) {  // CHECKME Should be impossible?
            return this;
        }
        //as.remove(a);
        List<EAction<StaticActionKind>> tmp = new LinkedList<>(as);
        for (EAction<StaticActionKind> b : tmp) {
            if (b.toDynamicDual(a.peer).equals(a.toDynamic())) {
                tmp.remove(b);
                get.put(dst, tmp);
                return new SSetHistory(copy);
            }
        }
        //return new SSetHistory(copy);
        return this;
    }

    @Override
    public SSetHistory syncRemove(Role r1, EAction<StaticActionKind> a) {
        Map<Role, Map<Role, List<EAction<StaticActionKind>>>> copy = cloneHistory(this.map);  // TODO optimise, factor out clone with constructor
        Map<Role, List<EAction<StaticActionKind>>> get1 = copy.get(r1);
        if (get1 != null) {
            List<EAction<StaticActionKind>> as = get1.get(a.peer);
            if (as != null) {
                // Remove a
                List<EAction<StaticActionKind>> tmp1 = new LinkedList<>(as);
                for (EAction<StaticActionKind> b : tmp1) {
                    if (b.toDynamic().equals(a.toDynamic())) {  // !!! first dynamic match, not static (syntactic) match -- pairwise FIFO so has to be eaten in order
                        tmp1.remove(b);
                        get1.put(a.peer, tmp1);
                        break;
                    }
                }
            }
        }
        Map<Role, List<EAction<StaticActionKind>>> get2 = copy.get(a.peer);
        if (get2 != null) {
            List<EAction<StaticActionKind>> as2 = get2.get(r1);
            if (as2 != null) {
                // Cf. remove(dst, a) -- remove the dual
                List<EAction<StaticActionKind>> tmp2 = new LinkedList<>(as2);
                for (EAction<StaticActionKind> b : tmp2) {
                    if (b.toDynamicDual(a.peer).equals(a.toDynamic())) {  // !!! first dynamic match, not static (syntactic) match -- pairwise FIFO so has to be eaten in order
                        tmp2.remove(b);
                        get2.put(a.peer, tmp2);
                        break;
                    }
                }
            }
        }
        return new SSetHistory(copy);
    }

    // Cf. sender with recursive "double" output but receiver with recursive "single" input...
    // ...(e.g., from unfold-all expansions, e.g., "unguarded choice-recs")...
    // ..."clear" is sound in some regards, but unsound in others (e.g., max buffer bounds)
    @Override
    public SSetHistory clear(Role dst, EAction<StaticActionKind> a) {
        Map<Role, Map<Role, List<EAction<StaticActionKind>>>> copy = cloneHistory(this.map);  // TODO optimise, factor out clone with constructor
        Map<Role, List<EAction<StaticActionKind>>> get = copy.get(a.peer);
        if (get == null) {  // CHECKME Should be impossible?
            return this;
        }
        List<EAction<StaticActionKind>> as = get.get(dst);
        if (as == null) {  // CHECKME Should be impossible?
            return this;
        }
        as.clear();
        return new SSetHistory(copy);
    }

    @Override
    public SSetHistory syncClear(Role r1, Role r2) {
        Map<Role, Map<Role, List<EAction<StaticActionKind>>>> copy = cloneHistory(this.map);  // TODO optimise, factor out clone with constructor
        Map<Role, List<EAction<StaticActionKind>>> get1 = copy.get(r1);
        if (get1 != null) {
            List<EAction<StaticActionKind>> as = get1.get(r2);
            if (as != null) {
                as.clear();
            }
        }
        Map<Role, List<EAction<StaticActionKind>>> get2 = copy.get(r2);
        if (get2 != null) {
            List<EAction<StaticActionKind>> as2 = get2.get(r1);
            if (as2 != null) {
                as2.clear();
            }
        }
        return new SSetHistory(copy);
    }

    @Override
    public String toString() {
        return this.map.toString();
    }

    @Override
    public int hashCode() {
        int hash = 31123;
        hash = 31 * hash + this.map.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SSetHistory)) {
            return false;
        }
        SSetHistory them = (SSetHistory) o;
        return this.map.equals(them.map);
    }
}
