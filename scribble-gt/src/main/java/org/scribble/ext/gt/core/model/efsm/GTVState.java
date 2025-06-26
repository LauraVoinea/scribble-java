package org.scribble.ext.gt.core.model.efsm;

import org.scribble.core.type.name.RecVar;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.ConsoleColors;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

public class GTVState {

    private static int count = 1;

    //public static final int NON_MIXED_ENTRY = -1;
    public static final int TOP_SCOPE = GTLType.c_TOP;

    public final int id;
    public final boolean isEntry;
    public final int c;  // TOP_SCOPE or c of innermost MC scope
    public final Set<RecVar> recvars;

    public GTVState(int c) {
        this(c, Set.of());
    }

    public GTVState(int c, Set<RecVar> recvars) {
        this(false, c, recvars);
    }

    public GTVState(boolean isEntry, int c) {
        this(isEntry, c, Set.of());
    }

    public GTVState(boolean isEntry, int c, Set<RecVar> recvars) {
        this.id = GTVState.count++;
        this.isEntry = isEntry;
        this.c = c;
        this.recvars = Collections.unmodifiableSet(new LinkedHashSet<>(recvars));
    }

    @Override
    public String toString() {
        return this.id + this.recvars.toString() + " "
                + (this.isEntry ? ConsoleColors.WHITE_TRIANGLE : "")
                + this.c;
    }

    @Override
    public int hashCode() {
        int hash = 15443;
        hash = 31 * hash + this.id;
        hash = 31 * hash + Objects.hash(this.isEntry);
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
        return this.id == cast.id && this.isEntry == cast.isEntry
                && this.c == cast.c && this.recvars.equals(cast.recvars);
    }
}
