package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.type.session.global.runtime.GTGMixedActive;
import org.scribble.ext.gt.core.type.session.global.runtime.GTGWiggly;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;

public class GTGTypeFactory {

    public static final GTGTypeFactory FACTORY = new GTGTypeFactory();

    protected GTGTypeFactory() {
    }

    public GTGInteraction choice(Role src, Role dst, LinkedHashMap<Op, Payload> pays, LinkedHashMap<Op, GTGType> cases) {
        return new GTGInteraction(src, dst, pays, cases);
    }

    public GTGWiggly wiggly(Role src, Role dst, Op op, LinkedHashMap<Op, Payload> pays, LinkedHashMap<Op, GTGType> cases) {
        return new GTGWiggly(src, dst, op, pays, cases);
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

    public GTGMixedActive activeMixedChoice(
            int c, int n, GTGType left, GTGType right, Role other, Role observer,  // other->observer |> observer->other
            LinkedHashSet<Role> committedLeft, LinkedHashSet<Role> committedRight) {
        return new GTGMixedActive(c, n, left, right, other, observer, committedLeft, committedRight);
    }

    //public GTGMixedChoice mixedChoice(int c, GTGType left, GTGType right, Role sec, Role pri, ) {

    public GTGEnd end() {
        return GTGEnd.END;
    }
}
