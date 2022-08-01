package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;

public class EALTypeFactory {

    public static final EALTypeFactory factory = new EALTypeFactory();

    protected EALTypeFactory() {

    }

    public EALEndType end() {
        return EALEndType.END;
    }

    public EALInType in(@NotNull Role peer,
                @NotNull LinkedHashMap<Op, Pair<EAValType, EALType>> cases) {
        return new EALInType(peer, cases);
    }

    public EALOutType out(@NotNull Role peer,
                @NotNull LinkedHashMap<Op, Pair<EAValType, EALType>> cases) {
        return new EALOutType(peer, cases);
    }
}
