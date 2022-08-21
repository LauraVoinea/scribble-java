package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.util.LinkedHashMap;

public class GTLTypeFactory {

    public static final GTLTypeFactory FACTORY = new GTLTypeFactory();

    protected GTLTypeFactory() {
    }

    public GTLBranch branch(Role src, LinkedHashMap<Op, GTLType> cases) {
        return new GTLBranch(src, cases);
    }

    public GTLSelect select(Role dst, LinkedHashMap<Op, GTLType> cases) {
        return new GTLSelect(dst, cases);
    }

    public GTLMixedChoice mixedChoice(GTLType left, GTLType right) {
        return new GTLMixedChoice(left, right);
    }

    public GTLEnd end() {
        return GTLEnd.END;
    }
}
