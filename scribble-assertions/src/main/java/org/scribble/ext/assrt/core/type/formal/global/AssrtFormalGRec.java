package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.global.action.AssrtFormalGComm;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// !!! Don't really need to separate silent and non-silent rec and recvar constructors -- and multiple state vars can be done in one step (cf. comm pay vars), the step is the rec(var), not the svars
public class AssrtFormalGRec extends AssrtFormalTypeBase implements AssrtFormalGType {

    public final RecVar recvar;
    public final AssrtFormalGType body;

    public final Map<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> statevars;  // var -> (roles, pay, init exprs)
    public final AssrtBFormula assertion;  // consolidated refinement

    protected AssrtFormalGRec(RecVar recvar, AssrtFormalGType body,
                              LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> svars,
                              AssrtBFormula ass) {
        this.recvar = recvar;
        this.statevars = Collections.unmodifiableMap(new LinkedHashMap<>(svars));
        this.body = body;
        this.assertion = ass;
    }

    @Override
    public AssrtFormalGType unfoldEnv(Map<RecVar, AssrtFormalGRec> env) {
        Map<RecVar, AssrtFormalGRec> tmp = new HashMap<>(env);
        tmp.put(this.recvar, this);
        return this.body.unfoldEnv(tmp);
    }

    @Override
    public Set<AssrtFormalGComm> getActions(AssrtGamma gamma, Set<Role> blocked) {
        Optional<AssrtGamma> tmp = makeGamma(gamma, this.body);
        for (Map.Entry<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> e : this.statevars.entrySet()) {
            AssrtVar v = e.getKey();
            Triple<Set<Role>, DataName, AssrtAFormula> p = e.getValue();
            tmp = tmp.flatMap(x -> x.addNohat(v, p.left, p.middle));
        }
        return tmp.map(x -> this.body.getActions(x, blocked)).get();  // unfold puts this.recvar in env
    }

    public static Optional<AssrtGamma> makeGamma(AssrtGamma gamma, AssrtFormalGType body) {
        Set<AssrtVar> vs = body.getVars();
        Optional<AssrtGamma> tmp = Optional.of(new AssrtGamma());
        for (Map.Entry<AssrtVar, Pair<Set<Role>, DataName>> e : gamma.nohat.entrySet()) {
            AssrtVar v = e.getKey();
            Pair<Set<Role>, DataName> p = e.getValue();
            if (!vs.contains(v)) {
                tmp = tmp.flatMap(x -> x.addNohat(v, p.left, p.right));
            }
        }
        for (Map.Entry<AssrtVar, Pair<Set<Role>, DataName>> e : gamma.hat.entrySet()) {
            AssrtVar v = e.getKey();
            Pair<Set<Role>, DataName> p = e.getValue();
            if (!vs.contains(v)) {
                tmp = tmp.flatMap(x -> x.addHat(v, p.left, p.right));
            }
        }
        return tmp;
    }

    @Override
    public Optional<Pair<AssrtGamma, AssrtFormalGType>> step(AssrtGamma gamma, AssrtFormalGComm a) {
        Optional<AssrtGamma> tmp = makeGamma(gamma, this.body);
        for (Map.Entry<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> e : this.statevars.entrySet()) {
            AssrtVar v = e.getKey();
            Triple<Set<Role>, DataName, AssrtAFormula> p = e.getValue();
            tmp = tmp.flatMap(x -> x.addNohat(v, p.left, p.middle));
        }
        return tmp.flatMap(x -> unfold().step(x, a));
    }

    @Override
    public AssrtFormalLType project(AssrtFormalLFactory lf, Role r, AssrtPhi phi) {
		/*if (this.statevars.size() != 1) {
			throw new RuntimeException("TODO: " + this);
		}*/

        Map<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> qs = new LinkedHashMap<>();

        LinkedHashMap<AssrtVar, Triple<Multiplicity, DataName, AssrtAFormula>> svars = new LinkedHashMap<>();
        for (Map.Entry<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> e : this.statevars.entrySet()) {
            AssrtVar k = e.getKey();
            Triple<Set<Role>, DataName, AssrtAFormula> p = e.getValue();
            Set<Role> rs = this.body.getRoles();
            Multiplicity multip = p.left.contains(r) && rs.contains(r) ? Multiplicity.OMEGA : Multiplicity.ZERO;
            svars.put(k, new Triple<>(multip, p.middle, p.right));

            AssrtVar svar = k;
            //Triple<Set<Role>, DataName, AssrtAFormula> p = this.statevars.get(svar);

            qs.put(svar, new Triple<>(p.left, p.middle, this.assertion));  // !!! maybe only put assertion on "last" var?
        }

        Optional<AssrtPhi> comma = phi.comma(this.recvar, qs);

        //AssrtVar svar = this.statevars.keySet().iterator().next();

        if (!comma.isPresent()) {
            throw new RuntimeException("Shouldn't get here? " + this + " ,, " + r);
        }

        AssrtFormalLType proj = this.body.project(lf, r, comma.get());
        return lf.rec(this.recvar, proj, svars, this.assertion);
    }

    @Override
    public Set<Role> getRoles() {
        return this.body.getRoles();
    }

    @Override
    public Set<AssrtVar> getVars() {
        return Stream.concat(
                this.statevars.keySet().stream(),
                this.body.getVars().stream()
        ).collect(Collectors.toSet());
    }

    @Override
    public String toString() {
        return "mu " + this.recvar + "("
                + this.statevars.entrySet().stream()
                .map(x -> {
                    Triple<Set<Role>, DataName, AssrtAFormula> p = x.getValue();
                    return x.getKey() + "^" + p.left + ":" + p.middle +
                            (p.right == null ? "" : " := " + p.right);
                }).collect(Collectors.joining(", "))
                + "){" + this.assertion + "}." + this.body;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AssrtFormalGRec)) {
            return false;
        }
        AssrtFormalGRec them = (AssrtFormalGRec) o;
        return super.equals(o)  // Checks canEquals -- implicitly checks kind
                && this.recvar.equals(them.recvar)
                && this.body.equals(them.body)
                && this.statevars.equals(them.statevars)
                && this.assertion.equals(them.assertion);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof AssrtFormalGRec;
    }

    @Override
    public int hashCode() {
        int hash = AssrtFormalGType.REC_HASH;
        hash = 31 * hash + this.recvar.hashCode();
        hash = 31 * hash + this.body.hashCode();
        hash = 31 * hash + this.statevars.hashCode();
        hash = 31 * hash + this.assertion.hashCode();
        return hash;
    }
}
