package org.scribble.ext.gt.core.type.session.global;

import org.scribble.ast.MsgNode;
import org.scribble.ast.RoleArgList;
import org.scribble.ast.SigLitNode;
import org.scribble.ast.global.*;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.ast.global.GTGMixed;
import org.scribble.ext.gt.core.type.session.local.GTLType;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

// org.scribble.ast -> org.scribble.ext.gt.core.type.session
// i.e., from parsed, pre inlining
public class GTGTypeTranslator3 {

    private static int counter = GTLType.c_TOP + 1;  // !!!

    private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;


    // N.B. top level -- GTGMixedChoice case in GSeq below
    public GTGType translate(GScribNode g) {
        if (g instanceof GInteractionSeq) {
            return translateGSeq((GInteractionSeq) g);
        } else if (g instanceof GMsgTransfer) {
            return translateGMessageTransfer((GMsgTransfer) g, fact.end());
        } else if (g instanceof GChoice) {
            return translateGChoice((GChoice) g);
        } else if (g instanceof GProtoBlock) {  // For choice with multiple blocks
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
                } else if (fst instanceof GRecursion) {
                    return translateGRecursion((GRecursion) fst);
                } else if (fst instanceof GContinue) {  // cf. GDo
                    return translateGContinue((GContinue) fst);
                } else if (fst instanceof GTGMixed) {
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

    protected GTGRecursion translateGRecursion(GRecursion g) {
        GTGType body = translateGSeq(g.getBlockChild().getInteractSeqChild());
        return this.fact.recursion(g.getRecVarChild().toName(), body);
    }

    protected GTGRecVar translateGContinue(GContinue g) {
        return this.fact.recVar(g.getRecVarChild().toName());
    }

    protected GTGMixedChoice translateGMixed(GTGMixed g) {
        GTGType left = translateGSeq(g.getLeftBlockChild().getInteractSeqChild());
        GTGType right = translateGSeq(g.getRightBlockChild().getInteractSeqChild());
        Role other = g.getOtherChild().toName();
        Role observer = g.getObserverChild().toName();
        List<Role> leftCommitted = g.getLeftRoleListChild().getRoles();  // TODO remove committed from Scribble syntax?
        List<Role> rightCommitted = g.getRightRoleListChild().getRoles();
        if (!leftCommitted.isEmpty() || !rightCommitted.isEmpty()) {
            throw new RuntimeException("TODO deprecated: " + g);
        }
        return this.fact.mixedChoice(this.counter++, left, right, other, observer);//, committedLeft, committedRight);
    }
}
