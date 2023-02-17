package org.scribble.ext.ea.cli;


import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.config.EAPRuntimeFactory;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.EATypeFactory;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class ASTBuilder {
    static EAPFactory pf = EAPFactory.factory;
    static EAPRuntimeFactory rf = EAPRuntimeFactory.factory;
    static EATypeFactory tf = EATypeFactory.factory;

    public EAPTerm visit(CommonTree n) {  // Entry
        return visitM(n);
    }

    public EAPExpr visitM(CommonTree n) {
        switch (n.getText()) {
            case "M_SEND": return visitSend(n);
            case "M_RETURN": return visitReturn(n);
        }
        throw new RuntimeException("Unknown node kind: " + n.getText());
    }

    public EAPSend visitSend(CommonTree n) {
        Role dst = visitRole((CommonTree) n.getChild(0));
        Op op = visitOp((CommonTree) n.getChild(1));
        EAPVal V = visitV((CommonTree) n.getChild(2));
        return pf.send(dst, op, V);
    }

    public EAPReturn visitReturn(CommonTree n) {
        EAPVal V = visitV((CommonTree) n.getChild(0));
        return pf.returnn(V);
    }

    public EAPVal visitV(CommonTree n) {
        String txt = n.getText();
        switch (txt) {
            case "V_HANDLERS": {
                List<CommonTree> cs = ((List<Object>) n.getChildren()).stream()
                        .map(x -> (CommonTree) x).collect(Collectors.toList());
                Role r = visitRole(cs.get(0));
                LinkedHashMap<Op, EAPHandler> hs = visitHandlers(cs.subList(1, cs.size()));
                return pf.handlers(r, hs);
            }
            case "V_UNIT": return pf.unit();
            default: return pf.var(txt);
        }
    }

    public LinkedHashMap<Op, EAPHandler> visitHandlers(List<CommonTree> n) {
        return n.stream().map(x -> visitHandler(x)).collect(Collectors.toMap(
                x -> x.op,
                x -> x,
                (x, y) -> null,
                LinkedHashMap::new
        ));
    }

    public EAPHandler visitHandler(CommonTree n) {
        Op op = visitOp((CommonTree) n.getChild(0));
        EAPVar var = (EAPVar) visitV((CommonTree) n.getChild(1));
        EAPExpr expr = visitM((CommonTree) n.getChild(2));
        return pf.handler(op, var, tf.val.unit(), expr, tf.local.end());
    }

    public Role visitRole(CommonTree n) {
        return new Role(n.getText());
    }

    public Op visitOp(CommonTree n) {
        return new Op(n.getText());
    }
}
