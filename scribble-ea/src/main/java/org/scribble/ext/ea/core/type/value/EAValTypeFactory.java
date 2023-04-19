package org.scribble.ext.ea.core.type.value;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;

public class EAValTypeFactory {

    public static final EAValTypeFactory factory = new EAValTypeFactory();

    protected EAValTypeFactory() {

    }

    public EAUnitType unit() {
        return EAUnitType.UNIT;
    }

    public EAIntType intt() {
        return EAIntType.INT;
    }

    public EABoolType bool() {
        return EABoolType.BOOL;
    }

    public EAPidType pid() {
        return EAPidType.PID;
    }

    public EAHandlersType handlers(@NotNull EALInType S, @NotNull EAValType T) {
        return new EAHandlersType(S, T);
    }

    public EAFuncType func(@NotNull EAValType A, @NotNull EALType S,
                           @NotNull EALType T, @NotNull EAValType B) {
        return new EAFuncType(A, S, T, B);
    }

}
