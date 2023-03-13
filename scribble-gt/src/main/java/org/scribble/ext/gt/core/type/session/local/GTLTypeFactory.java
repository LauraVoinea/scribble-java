package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.type.session.global.GTGRecVar;
import org.scribble.ext.gt.core.type.session.global.GTGRecursion;
import org.scribble.ext.gt.core.type.session.global.GTGType;

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

    public GTLRecursion recursion(RecVar var, GTLType body) {
        return new GTLRecursion(var, body);
    }

    public GTLRecVar recVar(RecVar var) {
        return new GTLRecVar(var);
    }


    public GTLMixedChoice mixedChoice(int c, GTLType left, GTLType right) {
        return new GTLMixedChoice(c, left, right);
    }

    public GTLMixedActive mixedActive(int c, int n, GTLType left, GTLType right) {
        return new GTLMixedActive(c, n, left, right);
    }

    public GTLEnd end() {
        return GTLEnd.END;
    }
}
