package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.SigLit;
import org.scribble.core.type.session.global.GChoice;
import org.scribble.core.type.session.global.GMessageTransfer;
import org.scribble.core.type.session.global.GSeq;
import org.scribble.core.type.session.global.GType;

import java.util.*;
import java.util.stream.Collectors;

@Deprecated
public class GTGTypeTranslator {

    private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    public GTGType translate(GType g) {
        if (g instanceof GSeq) {
            return translateGSeq((GSeq) g);
        } else if (g instanceof GMessageTransfer) {
            return translateGMessageTransfer((GMessageTransfer) g, fact.end());
        } else if (g instanceof GChoice) {
            return translateGChoice((GChoice) g);
        } else {
            throw new RuntimeException("TODO: " + g);
        }
    }

    protected GTGType translateGSeq(GSeq g) {
        return translateGSeq(g.getElements());
    }

    protected GTGType translateGSeq(List<GType> es) {
        int len = es.size();
        if (len == 0) {
            return fact.end();
        } else {
            GType fst = es.get(0);
            if (len == 1) {
                if (fst instanceof GMessageTransfer) {
                    return translateGMessageTransfer((GMessageTransfer) fst, fact.end());
                } else if (fst instanceof GChoice) {
                    return translateGChoice((GChoice) fst);
                } else {
                    throw new RuntimeException("TODO: " + es);
                }
            } else {
                if (!(fst instanceof GMessageTransfer)) {
                    throw new RuntimeException("TODO: " + es);
                }
                GTGType cont = translateGSeq(es.subList(1, len));
                return translateGMessageTransfer((GMessageTransfer) fst, cont);
            }
        }
    }

    protected GTGInteraction translateGMessageTransfer(GMessageTransfer g, GTGType cont) {
        if (!(g.msg instanceof SigLit)) {
            throw new RuntimeException("TODO: " + g.msg);
        }
        Op op = ((SigLit) g.msg).op;
        //Map<Op, GTGType> cs = Collections.singletonMap(op, cont);
        LinkedHashMap<Op, GTGType> cs = new LinkedHashMap<>();
        cs.put(op, cont);
        return this.fact.choice(g.src, g.dst, cs);
    }

    // Pre: role enabling OK (choice subj = first senders)
    protected GTGInteraction translateGChoice(GChoice g) {
        List<GTGType> cs = g.blocks.stream().map(x -> translate(x))
                .collect(Collectors.toUnmodifiableList());  // cs.len > 0
        LinkedHashMap<Op, GTGType> ds = new LinkedHashMap<>();
        Role dst = null;
        for (GTGType c : cs) {
            if (!(c instanceof GTGInteraction)) {  // !!! (all) end not currently allowed
                throw new RuntimeException("TODO: cs");
            }
            GTGInteraction cast = (GTGInteraction) c;
            if (dst == null) {
                dst = cast.dst;
            } else if (!dst.equals(cast.dst)) {
                throw new RuntimeException("Non-directed choice: " + g);
            }
            ds.putAll(cast.cases);
        }
        return this.fact.choice(g.subj, dst, ds);
    }
}
