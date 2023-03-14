package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalType;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;

import java.util.Set;


public interface AssrtFormalGType extends AssrtFormalType
{
    int CHOICE_HASH = 26947;
    int END_HASH = 26951;
    int REC_HASH = 26953;
    int RECVAR_HASH = 26959;

    AssrtFormalLType project(AssrtFormalLFactory lf, Role r, AssrtPhi phi);

    Set<Role> getRoles();

    /*
    Set<AssrtLAction> getSteppable(AssrtLambda lambda);
    Optional<Pair<AssrtLambda, AssrtFormalGlobal>> step(AssrtLambda lambda, AssrtLAction a);

    Set<AssrtLAction> getDerivSteppable(AssrtLambda lambda);
    Optional<Triple<AssrtLambda, AssrtFormalGlobal, Rho>> dstep(AssrtLambda lambda, Rho rho, AssrtLAction a);
     */

}
