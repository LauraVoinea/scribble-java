package org.scribble.ext.ea.core.type;

import org.scribble.ext.ea.core.type.session.local.EALTypeFactory;
import org.scribble.ext.ea.core.type.value.EAVTypeFactory;

public class EATypeFactory {

    public static final EATypeFactory factory = new EATypeFactory();

    public final EALTypeFactory local = EALTypeFactory.factory;
    public final EAVTypeFactory val = EAVTypeFactory.factory;

    protected EATypeFactory() {

    }
}
