package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;

import java.util.LinkedHashMap;

public class GTLTypeFactory {

    public static final GTLTypeFactory FACTORY = new GTLTypeFactory();

    protected GTLTypeFactory() {
    }

    public GTLBranch branch(Role src, LinkedHashMap<Op, DataName> pays, LinkedHashMap<Op, GTLType> cases) {
        return new GTLBranch(src, pays, cases);
    }

    public GTLSelect select(Role dst, LinkedHashMap<Op, DataName> pays, LinkedHashMap<Op, GTLType> cases) {
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

    public GTLMixedActive mixedActive(int c, int n, GTLType left, GTLType right) {
        return new GTLMixedActive(c, n, left, right);
    }

    public GTLMixedCommitted mixedCommitted(int c, int n, GTLType type, Side side) {
        return new GTLMixedCommitted(c, n, type, side);
    }

    public GTLEnd end() {
        return GTLEnd.END;
    }
}
