package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

import java.util.LinkedHashMap;

public class GTGTypeFactory {

    public static final GTGTypeFactory FACTORY = new GTGTypeFactory();

    protected GTGTypeFactory() {
    }

    public GTGInteraction choice(Role src, Role dst, LinkedHashMap<Op, Payload> pays, LinkedHashMap<Op, GTGType> cases) {
        return new GTGInteraction(src, dst, pays, cases);
    }

    public GTGRecursion recursion(RecVar var, GTGType body) {
        return new GTGRecursion(var, body);
    }

    public GTGRecVar recVar(RecVar var) {
        return new GTGRecVar(var);
    }

    public GTGMixedChoice mixedChoice(
            int c, GTGType left, GTGType right, Role other, Role observer, boolean hasFailedAnnot) {  // other->observer |> observer->other
        return new GTGMixedChoice(c, left, right, other, observer, hasFailedAnnot);
    }

    public GTGEnd end() {
        return GTGEnd.END;
    }
}
