package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

import java.util.LinkedHashMap;

public class GTLTypeFactory {

    public static final GTLTypeFactory FACTORY = new GTLTypeFactory();

    protected GTLTypeFactory() {
    }

    public GTLBranch branch(Role src, LinkedHashMap<Op, Payload> pays, LinkedHashMap<Op, GTLType> cases) {
        return new GTLBranch(src, pays, cases);
    }

    public GTLSelect select(Role dst, LinkedHashMap<Op, Payload> pays, LinkedHashMap<Op, GTLType> cases) {
        return new GTLSelect(dst, pays, cases);
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

    public GTLEnd end() {
        return GTLEnd.END;
    }
}
