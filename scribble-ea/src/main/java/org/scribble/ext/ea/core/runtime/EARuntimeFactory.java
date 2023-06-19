package org.scribble.ext.ea.core.runtime;

import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.local.LTypeFactory;
import org.scribble.ext.ea.core.runtime.config.EACActor;
import org.scribble.ext.ea.core.term.expr.EAEAPName;
import org.scribble.ext.ea.core.term.expr.EAEHandlers;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.comp.EAComp;
import org.scribble.ext.ea.core.type.session.local.AsyncDelta;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class EARuntimeFactory {

    public static final EARuntimeFactory factory = new EARuntimeFactory();

    protected EARuntimeFactory() {
    }

    public EAPid pid(String id) {
        return new EAPid(id);
    }

    public EASid sid(String id) {
        return new EASid(id);
    }

    public EAAPid ap(String id) {
        return new EAAPid(id);
    }

    public EAIota iota(String id) {
        return new EAIota(id);
    }

    public EACActor actor(EAPid pid, EAThread T,
                          LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigma,
                          LinkedHashMap<EAIota, EAComp> rho,
                          //LinkedHashMap<Pair<EAPSid, Role>, Integer> state) {
                          EAExpr state) {
        return new EACActor(pid, T, sigma, rho, state);
    }

    public EATIdle idleThread() {
        return EATIdle.IDLE;
    }

    public EATSession sessionThread(EAComp expr, EASid sid, Role role) {
        return new EATSession(expr, sid, role);
    }

    public EATNoSession noSessionThread(EAComp expr) {
        return new EATNoSession(expr);
    }

    public EASystem system(LTypeFactory lf, Delta annots,
                           LinkedHashMap<EAPid, EACActor> cs) {
        return new EASystem(lf, annots, cs);
    }

    public EAAsyncSystem asyncSystem(LTypeFactory lf, //Delta annots,
                                     LinkedHashMap<EAPid, EACActor> cs,
                                     LinkedHashMap<EASid, EAGlobalQueue> queues,
                                     LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access,
                                     AsyncDelta adelta) {
        return new EAAsyncSystem(lf, cs, queues, access, adelta);
    }
}
