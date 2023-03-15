package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.global.action.AssrtFormalGComm;
import org.scribble.ext.assrt.core.type.formal.local.*;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.Set;


public class AssrtFormalGEnd implements AssrtFormalGType {
    public static final AssrtFormalGEnd END = new AssrtFormalGEnd();

    private AssrtFormalGEnd() {

    }

    @Override
    public AssrtFormalGType unfoldEnv(Map<RecVar, AssrtFormalGRec> env) {
        return this;
    }

    @Override
    public Set<AssrtFormalGComm> getActions(AssrtGamma gamma, Set<Role> blocked) {
        return Collections.emptySet();
    }

    @Override
    public Optional<Pair<AssrtGamma, AssrtFormalGType>> step(AssrtGamma gamma, AssrtFormalGComm a) {
        return Optional.empty();
    }

    @Override
    public AssrtFormalLEnd project(AssrtFormalLFactory lf, Role r, AssrtPhi phi) {
        return lf.end();
    }

    @Override
    public Set<Role> getRoles() {
        return Collections.emptySet();
    }

    @Override
    public Set<AssrtVar> getVars() {
        return Collections.emptySet();
    }

	/*
	@Override
	public Set<AssrtLAction> getSteppable(AssrtLambda lambda) {
		return Collections.emptySet();
	}

	@Override
	public Optional<Pair<AssrtLambda, AssrtFormalLocal>> step(AssrtLambda lambda, AssrtLAction a) {
		return Optional.empty();
	}

	@Override
	public Set<AssrtLAction> getDerivSteppable(AssrtLambda lambda) {
		return Collections.emptySet();
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLocal, Rho>> dstep(AssrtLambda lambda, Rho rho, AssrtLAction a) {
		return Optional.empty();
	}
	 */

    @Override
    public String toString() {
        return "end";
    }

    /* Aux */

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof AssrtFormalGEnd)) {
            return false;
        }
        return super.equals(obj);  // Checks canEquals
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof AssrtFormalGEnd;
    }

    @Override
    public int hashCode() {
        int hash = AssrtFormalGType.END_HASH;
        hash = 31 * hash;
        return hash;
    }
}
