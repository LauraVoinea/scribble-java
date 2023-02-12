package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Msg;
import org.scribble.core.type.session.SigLit;
import org.scribble.core.type.session.global.GChoice;
import org.scribble.core.type.session.global.GMessageTransfer;
import org.scribble.core.type.session.global.GSeq;
import org.scribble.core.type.session.global.GType;
import org.scribble.ext.gt.ast.GTMixed;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

// org.scribble.core.type.session -> org.scribble.ext.gt.core.type.session
// i.e., from inlined
public class GTGTypeTranslator2 {

    private static int counter = 1;

    private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;


    public GTGType translate(GType g) {
        if (g instanceof GSeq) {
            return translateGSeq((GSeq) g);
        } else if (g instanceof GMessageTransfer) {
            return translateGMessageTransfer((GMessageTransfer) g, fact.end());
        } else if (g instanceof GChoice) {
            return translateGChoice((GChoice) g);
        } /*else if (g instanceof GProtoBlock) {  // XXX CHECKME why needed?
            return translateGSeq(((GProtoBlock) g).getInteractSeqChild());
        }*/ else {
            throw new RuntimeException("TODO: " + g.getClass() + "\n" + g);
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
                } else if (fst instanceof GTMixed) {
                    return translateGMixed((org.scribble.core.type.session.global.GTGMixedChoice) fst);
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

        //... HERE: update translation from raw AST nodes

        Msg m = g.msg;
        if (!m.isSigLit()) {
            throw new RuntimeException("TODO: " + m);
        }
        Op op = ((SigLit) m).op;
        //Map<Op, GTGType> cs = Collections.singletonMap(op, cont);
        LinkedHashMap<Op, GTGType> cs = new LinkedHashMap<>();
        cs.put(op, cont);
        Role src = g.src;
        Role dst = g.dst;
        return this.fact.choice(src, dst, cs);
    }

    // Pre: role enabling OK (choice subj = first senders)
    protected GTGInteraction translateGChoice(GChoice g) {
        List<GSeq> bs = g.getBlocks();
        List<GTGType> cs = bs.stream().map(x -> translate(x))
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
        Role subj = g.getSubject();
        return this.fact.choice(subj, dst, ds);
    }

    protected GTGMixedChoice translateGMixed(org.scribble.core.type.session.global.GTGMixedChoice g) {
        GTGType left = translateGSeq(g.left);
        GTGType right = translateGSeq(g.right);
        /*LinkedHashSet<Role> committedLeft = new LinkedHashSet<>(g.getLeftRoleListChild().getRoles());
        LinkedHashSet<Role> committedRight = new LinkedHashSet<>(g.getRightRoleListChild().getRoles());*/
        return this.fact.mixedChoice(this.counter++, left, right, g.other, g.observer);//, committedLeft, committedRight);
    }
}
