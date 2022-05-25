package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.util.Map;

public class GTGTypeFactory {

    public static final GTGTypeFactory FACTORY = new GTGTypeFactory();

    protected GTGTypeFactory() {
    }

    public GTGChoice choice(Role src, Role dst, Map<Op, GTGType> cases) {
        return new GTGChoice(src, dst, cases);
    }

    public GTGEnd end() {
        return GTGEnd.END;
    }
}
