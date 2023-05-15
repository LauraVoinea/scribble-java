package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.util.EAUtil;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class EAGlobalQueue {

    @NotNull public final EASid sid;
    @NotNull public final List<EAMsg> ms;

    public EAGlobalQueue(EASid sid) {
        this(sid, Collections.emptyList());
    }

    public EAGlobalQueue(EASid sid, List<EAMsg> ms) {
        this.sid = sid;
        this.ms = EAUtil.umodCopyOf(ms);
    }

    /* ... */

    public Optional<EAMsg> getFirst(Role snd, Role rcv) {
        return this.ms.stream()
                .filter(x -> x.snd.equals(snd) && x.rcv.equals(rcv)).findFirst();
    }

    public EAGlobalQueue append(EAMsg m) {
        return new EAGlobalQueue(this.sid, EAUtil.listOf(this.ms, m));
    }

    public EAGlobalQueue remove(EAMsg m) {
        boolean[] found = {false};
        List<EAMsg> filt = this.ms.stream().filter(x -> {
            if (!found[0] && x.snd.equals(m.snd) && x.rcv.equals(m.rcv)) {
                found[0] = true;
                return false;
            }
            return true;
        }).collect(Collectors.toList());
        if (!found[0]) {
            throw new RuntimeException("Not found " + m + " in: " + this);
        }
        return new EAGlobalQueue(this.sid, filt);
    }

    @Override
    public String toString() {
        return this.ms.toString();
    }

    /* equals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAGlobalQueue them = (EAGlobalQueue) o;
        return this.sid.equals(them.sid) && this.ms.equals(them.ms);
    }

    @Override
    public int hashCode() {
        int hash = 84991;
        hash = 31 * hash + this.sid.hashCode();
        hash = 31 * hash + this.ms.hashCode();
        return hash;
    }
}
