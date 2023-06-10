package org.scribble.ext.ea.core.type.value;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.EAType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.util.EAUtil;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class EAVAPType implements EAVType {

    @NotNull public final Map<Role, EALType> roles;

    protected EAVAPType(LinkedHashMap<Role, EALType> roles) {
        this.roles = EAUtil.umodCopyOf(roles);
    }

    /* Aux */

    @Override
    public String toString() {
        return "AP(" +
                this.roles.entrySet().stream()
                        .map(x -> x.getKey() + " -> " + x.getValue())
                        .collect(Collectors.joining(", "))
                + ")";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAVAPType them = (EAVAPType) o;
        return them.canEquals(this) && this.roles.equals(them.roles);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAVAPType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.AP;
        hash = 31 * hash + this.roles.hashCode();
        return hash;
    }
}
