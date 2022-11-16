package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.Op;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalFactory;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.global.AssrtGChoice;
import org.scribble.ext.assrt.core.type.session.global.AssrtGEnd;
import org.scribble.ext.assrt.core.type.session.global.AssrtGType;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.stream.Collectors;

public class AssrtFormalGTranslator {

    protected final AssrtFormalGFactory gf = AssrtFormalFactory.factory.global;

    // Pre: g is "inlined"
    public AssrtFormalGType translate(AssrtGType g) {
        if (g instanceof AssrtGEnd) {
            return this.gf.end();
        } else if (g instanceof AssrtGChoice) {
            return translate((AssrtGChoice) g);
        } else {
            throw new RuntimeException("TODO: " + g);
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
}
