package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.ext.assrt.core.type.formal.AssrtFormalType;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.Optional;
import java.util.Set;


public interface AssrtFormalLocal extends AssrtFormalType
{
    public final int SELECT_HASH = 14771;
    public final int BRANCH_HASH = 14779;
    public final int CHOICE_HASH = 14783;
    public final int SILENT_HASH = 14797;
    public final int END_HASH = 14813;

    @Deprecated
    public final int TRANSFER_HASH = 10987;

    public final int EPSILON_HASH = 10993;
    public final int COMM_HASH = 11047;
    public final int SEND_HASH = 11003;
    public final int RECEIVE_HASH = 11027;

    Set<AssrtLAction> getSteppable(AssrtLambda lambda);
    Optional<Pair<AssrtLambda, AssrtFormalLocal>> step(AssrtLambda lambda, AssrtLAction a);

    Set<AssrtLAction> getInterSteppable(AssrtLambda lambda);
    Optional<Pair<AssrtLambda, AssrtFormalLocal>> istep(AssrtLambda lambda, AssrtLAction a);

    // Below AssrtLActions are concrete, i.e., not silent
    Set<AssrtLAction> getDerivSteppable(AssrtLambda lambda, AssrtRho rho);
    Optional<Triple<AssrtLambda, AssrtFormalLocal, AssrtRho>> dstep(AssrtLambda lambda, AssrtRho rho, AssrtLAction a);

}
