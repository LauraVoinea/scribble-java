package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.util.LinkedHashMap;

public class GTGTypeFactory {

    public static final GTGTypeFactory FACTORY = new GTGTypeFactory();

    protected GTGTypeFactory() {
    }

    public GTGInteraction choice(Role src, Role dst, LinkedHashMap<Op, GTGType> cases) {
        return new GTGInteraction(src, dst, cases);
    }

    public GTGWiggly wiggly(Role src, Role dst, Op op, LinkedHashMap<Op, GTGType> cases) {
        return new GTGWiggly(src, dst, op, cases);
    }

    public GTGEnd end() {
        return GTGEnd.END;
    }
}
