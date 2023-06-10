package org.scribble.ext.ea.core.type.value;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;

import java.util.LinkedHashMap;

public class EAVTypeFactory {

    public static final EAVTypeFactory factory = new EAVTypeFactory();

    protected EAVTypeFactory() {

    }

    public EAVWildType wild() {
        return EAVWildType.WILD;
    }

    public EAVUnitType unit() {
        return EAVUnitType.UNIT;
    }

    public EAVIntType intt() {
        return EAVIntType.INT;
    }

    public EAVBoolType bool() {
        return EAVBoolType.BOOL;
    }

    public EAVAPType ap(LinkedHashMap<Role, EALType> roles) {
        return new EAVAPType(roles);
    }

    public EAVPidType pid() {
        return EAVPidType.PID;
    }

    public EAVHandlersType handlers(@NotNull EALInType S, @NotNull EAVType T) {
        return new EAVHandlersType(S, T);
    }

    public EAVFuncType func(@NotNull EAVType A, @NotNull EALType S,
                            @NotNull EALType T, @NotNull EAVType B) {
        return new EAVFuncType(A, S, T, B);
    }

}
