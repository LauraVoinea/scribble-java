package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalType;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;


public interface AssrtFormalGType extends AssrtFormalType
{
    public final int CHOICE_HASH = 26947;
    public final int END_HASH = 26951;

    AssrtFormalLType project(AssrtFormalLFactory lf, Role r);  // TODO: add \Phi for recursion

    /*
    Set<AssrtLAction> getSteppable(AssrtLambda lambda);
    Optional<Pair<AssrtLambda, AssrtFormalGlobal>> step(AssrtLambda lambda, AssrtLAction a);

    Set<AssrtLAction> getDerivSteppable(AssrtLambda lambda);
    Optional<Triple<AssrtLambda, AssrtFormalGlobal, Rho>> dstep(AssrtLambda lambda, Rho rho, AssrtLAction a);
     */

}
