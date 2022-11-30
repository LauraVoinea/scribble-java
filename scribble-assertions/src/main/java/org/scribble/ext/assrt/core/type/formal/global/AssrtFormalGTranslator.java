package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalFactory;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.global.*;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class AssrtFormalGTranslator {

    protected final AssrtFormalGFactory gf = AssrtFormalFactory.factory.global;

    // Pre: g is "inlined"
    public AssrtFormalGType translate(AssrtGType g) {
        if (g instanceof AssrtGEnd) {
            return this.gf.end();
        } else if (g instanceof AssrtGChoice) {
            return translate((AssrtGChoice) g);
        } else if (g instanceof AssrtGRec) {
            return translate((AssrtGRec) g);
        } else if (g instanceof AssrtGRecVar) {

            // !!! XXX TODO

            return this.gf.end();
        } else {
            throw new RuntimeException("TODO: " + g.getClass() + "\n\t");
        }
    }

    protected AssrtFormalGChoice translate(AssrtGChoice g) {
        LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalGType>> collect =
                g.cases.entrySet().stream().collect(Collectors.toMap(
                    x -> x.getKey().op,
                    x -> new Pair<>(x.getKey(), translate(x.getValue())),
                        (x, y) -> null, LinkedHashMap::new));
        return this.gf.branch(g.src, g.dst, collect);
    }

    protected AssrtFormalGRec translate(AssrtGRec g) {
        LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> svars
                = new LinkedHashMap<>();

        System.out.println("111: " + g.statevars + " ,, " + g.located);

        for (Map.Entry<AssrtVar, AssrtAFormula> e : g.statevars.entrySet()) {
            AssrtVar k = e.getKey();
            Set<Role> r = new HashSet();  // !!! current syntax requires distinct statevar names per located role
            r.add(g.located.get(k));
            svars.put(k, new Triple<>(r, new DataName("int"), e.getValue()));
        }
        return this.gf.rec(g.recvar, translate(g.body), svars, g.assertion);
    }
}
