package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalFactory;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.global.*;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class AssrtFormalGTranslator {

    protected final AssrtFormalGFactory gf = AssrtFormalFactory.factory.global;

    // Pre: g is "inlined"
    public AssrtFormalGType translate(AssrtGType g) {
        return translate(new HashMap<>(), g);
    }

    public AssrtFormalGType translate(Map<RecVar, List<AssrtVar>> m, AssrtGType g) {
        if (g instanceof AssrtGEnd) {
            return this.gf.end();
        } else if (g instanceof AssrtGChoice) {
            return translate(m, (AssrtGChoice) g);
        } else if (g instanceof AssrtGRec) {
            return translate(m, (AssrtGRec) g);
        } else if (g instanceof AssrtGRecVar) {
            return translate(m, (AssrtGRecVar) g);
        } else {
            throw new RuntimeException("TODO: " + g.getClass() + "\n\t");
        }
    }

    protected AssrtFormalGChoice translate(Map<RecVar, List<AssrtVar>> m, AssrtGChoice g) {
        LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalGType>> collect =
                g.cases.entrySet().stream().collect(Collectors.toMap(
                    x -> x.getKey().op,
                    x -> new Pair<>(x.getKey(), translate(m, x.getValue())),
                        (x, y) -> null, LinkedHashMap::new));
        return this.gf.branch(g.src, g.dst, collect);
    }

    /*protected AssrtMsg insertUnit(AssrtMsg m) {
        switch (m.pay.size()) {
            case 0: return new AssrtMsg(m.op, ...unit..., m.ass, m.phantAss);
            case 1: return m;
            default: throw new RuntimeException("TODO: " + m);
        }
    }*/

    protected AssrtFormalGRec translate(Map<RecVar, List<AssrtVar>> m, AssrtGRec g) {
        LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> svars
                = new LinkedHashMap<>();
        for (Map.Entry<AssrtVar, AssrtAFormula> e : g.statevars.entrySet()) {
            AssrtVar k = e.getKey();
            Set<Role> r = new HashSet();  // !!! current syntax requires distinct statevar names per located role
            r.add(g.located.get(k));
            svars.put(k, new Triple<>(r, new DataName("int"), e.getValue()));
        }
        Map<RecVar, List<AssrtVar>> tmp = new HashMap<>(m);
        tmp.put(g.recvar, g.statevars.keySet().stream().collect(Collectors.toList()));  // LinkedHashMap keyset ordered
        return this.gf.rec(g.recvar, translate(tmp, g.body), svars, g.assertion);
    }

    protected AssrtFormalGRecVar translate(Map<RecVar, List<AssrtVar>> m, AssrtGRecVar g) {
        List<AssrtVar> vs = m.get(g.recvar);
        if (g.stateexprs.size() != vs.size()) {
            throw new RuntimeException("Shouldn't get here? " + g);
        }
        LinkedHashMap<AssrtVar, AssrtAFormula> tmp = new LinkedHashMap<>();
        Iterator<AssrtAFormula> it = g.stateexprs.iterator();
        vs.forEach(x -> tmp.put(x, it.next()));
        return this.gf.recvar(g.recvar, tmp);
    }
}
