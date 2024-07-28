package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.EModelFactoryImpl;
import org.scribble.core.model.endpoint.actions.ERecv;
import org.scribble.core.type.name.Op;
import org.scribble.ext.gt.core.type.session.local.*;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class GTFsmConstructor {

    public static final GTEModelFactory mf = (GTEModelFactory) GTEModelFactoryImpl.FACTORY.local;

    public GTEState construct(Set<Op> com, GTLType t) {
        return construct(com, new HashMap<>(), t);
    }

    protected GTEState construct(Set<Op> com, Map<GTLRecVar, GTEState> recs, GTLType t) {
        if (t instanceof GTLBranch) {
            return constructBranch(com, recs, (GTLBranch) t);
        } else if (t instanceof GTLSelect) {

        } else if (t instanceof GTLRecursion) {

        } else if (t instanceof GTLRecVar) {
        } else if (t instanceof GTLMixedChoice) {
        } else if (t instanceof GTLEnd) {

        }
        throw new RuntimeException("CHECKME: " + t.getClass());
    }

    protected GTEState constructBranch(Set<Op> com, Map<GTLRecVar, GTEState> recs, GTLBranch t) {
        GTEState s = new GTEState(Collections.emptySet());
        for (Map.Entry<Op, GTLType> e : t.cases.entrySet()) {
            ERecv<StaticActionKind> a = mf.StaticERecv(t.src, e.getKey(), null);  // HERE HERE payloads
            GTEState succ = construct(com, recs, e.getValue());
            s.addEdge(a, succ);
        }
        return s;
    }
}
