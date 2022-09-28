package org.scribble.ext.gt.core.type.session.global;

import org.scribble.ast.MsgNode;
import org.scribble.ast.SigLitNode;
import org.scribble.ast.global.*;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.ast.GTMixed;
import org.scribble.ext.gt.ast.global.GTGMixed;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Collectors;

public class GTGTypeTranslator2 {

    private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    public GTGType translate(GScribNode g) {
        if (g instanceof GInteractionSeq) {
            return translateGSeq((GInteractionSeq) g);
        } else if (g instanceof GMsgTransfer) {
            return translateGMessageTransfer((GMsgTransfer) g, fact.end());
        } else if (g instanceof GChoice) {
            return translateGChoice((GChoice) g);
        } else if (g instanceof GProtoBlock) {  // XXX CHECKME why needed?
            return translateGSeq(((GProtoBlock) g).getInteractSeqChild());
        } else {
            throw new RuntimeException("TODO: " + g.getClass() + "\n" + g);
        }
    }

    protected GTGType translateGSeq(GInteractionSeq g) {
        return translateGSeq(g.getInteractionChildren());
    }

    protected GTGType translateGSeq(List<GSessionNode> es) {
        int len = es.size();
        if (len == 0) {
            return fact.end();
        } else {
            GSessionNode fst = es.get(0);
            if (len == 1) {
                if (fst instanceof GMsgTransfer) {
                    return translateGMessageTransfer((GMsgTransfer) fst, fact.end());
                } else if (fst instanceof GChoice) {
                    return translateGChoice((GChoice) fst);
                } else if (fst instanceof GTMixed) {
                    return translateGMixed((GTGMixed) fst);
                } else {
                    throw new RuntimeException("TODO: " + es);
                }
            } else {
                if (!(fst instanceof GMsgTransfer)) {
                    throw new RuntimeException("TODO: " + es);
                }
                GTGType cont = translateGSeq(es.subList(1, len));
                return translateGMessageTransfer((GMsgTransfer) fst, cont);
            }
        }
    }

    protected GTGInteraction translateGMessageTransfer(GMsgTransfer g, GTGType cont) {

        //... HERE: update translation from raw AST nodes

        MsgNode m = g.getMessageNodeChild();
        if (!(m instanceof SigLitNode)) {
            throw new RuntimeException("TODO: " + m);
        }
        Op op = ((SigLitNode) m).getOpChild().toName();
        //Map<Op, GTGType> cs = Collections.singletonMap(op, cont);
        LinkedHashMap<Op, GTGType> cs = new LinkedHashMap<>();
        cs.put(op, cont);
        Role src = g.getSourceChild().toName();
        List<Role> dsts = g.getDestinationRoles();
        if (dsts.size() != 1) {
            throw new RuntimeException("TODO: " + g);
        }
        Role dst = dsts.get(0);
        return this.fact.choice(src, dst, cs);
    }

    // Pre: role enabling OK (choice subj = first senders)
    protected GTGInteraction translateGChoice(GChoice g) {
        List<GProtoBlock> bs = g.getBlockChildren();
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
        Role subj = g.getSubjectChild().toName();
        return this.fact.choice(subj, dst, ds);
    }

    protected GTGMixedChoice translateGMixed(GTGMixed g) {
        GTGType left = translateGSeq(g.getLeftBlockChild().getInteractSeqChild());
        GTGType right = translateGSeq(g.getRightBlockChild().getInteractSeqChild());
        Role sec = g.getSecondaryChild().toName();
        Role pri = g.getPrimaryChild().toName();
        LinkedHashSet<Role> committedLeft = new LinkedHashSet<>(g.getLeftRoleListChild().getRoles());
        LinkedHashSet<Role> committedRight = new LinkedHashSet<>(g.getRightRoleListChild().getRoles());
        return this.fact.mixedChoice(left, right, sec, pri, committedLeft, committedRight);
    }
}
