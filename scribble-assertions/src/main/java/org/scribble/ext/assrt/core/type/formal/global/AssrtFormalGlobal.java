package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.ext.assrt.core.type.formal.AssrtFormalType;
import org.scribble.ext.assrt.core.type.formal.local.AssrtLambda;
import org.scribble.ext.assrt.core.type.formal.local.Rho;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.Optional;
import java.util.Set;


public interface AssrtFormalGlobal extends AssrtFormalType
{
    public final int CHOICE_HASH = 26947;
    public final int END_HASH = 26951;

    /*
    Set<AssrtLAction> getSteppable(AssrtLambda lambda);
    Optional<Pair<AssrtLambda, AssrtFormalGlobal>> step(AssrtLambda lambda, AssrtLAction a);

    Set<AssrtLAction> getDerivSteppable(AssrtLambda lambda);
    Optional<Triple<AssrtLambda, AssrtFormalGlobal, Rho>> dstep(AssrtLambda lambda, Rho rho, AssrtLAction a);
     */

}
