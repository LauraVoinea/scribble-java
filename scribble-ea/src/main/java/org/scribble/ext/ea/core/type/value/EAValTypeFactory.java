package org.scribble.ext.ea.core.type.value;

import org.scribble.ext.ea.core.type.session.local.EALInType;

public class EAValTypeFactory {

    public static final EAValTypeFactory factory = new EAValTypeFactory();

    protected EAValTypeFactory() {

    }

    public EAUnitType unit() {
        return EAUnitType.UNIT;
    }

    public EAPidType pid() {
        return EAPidType.PID;
    }

    public EAHandlersType handlers(EALInType S) {
        return new EAHandlersType(S);
    }
}
