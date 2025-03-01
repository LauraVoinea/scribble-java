package org.scribble.ext.gt.core.model.efsm;

import org.scribble.core.type.name.RecVar;
import org.scribble.ext.gt.util.ConsoleColors;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

public class GTVState {

    private static int count = 1;

    public static final int NON_MIXED_ENTRY = -1;

    public final int id;
    public final int c;  // -1 for non-mixed entry
    public final Set<RecVar> recvars;

    public GTVState() {
        this(NON_MIXED_ENTRY, Set.of());
    }

    public GTVState(Set<RecVar> recvars) {
        this(NON_MIXED_ENTRY, recvars);
    }

    public GTVState(int c, Set<RecVar> recvars) {
        this.id = GTVState.count++;
        this.c = c;
        this.recvars = Collections.unmodifiableSet(new LinkedHashSet<>(recvars));
    }

    @Override
    public String toString() {
        return this.id + this.recvars.toString() + (this.c == NON_MIXED_ENTRY ? "" : (" " + ConsoleColors.WHITE_TRIANGLE + c));
    }

    @Override
    public int hashCode() {
        int hash = 15443;
        hash = 31 * hash + this.id;
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.recvars.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTVState cast)) {
            return false;
        }
        return this.id == cast.id && this.c == cast.c && this.recvars.equals(cast.recvars);
    }
}
