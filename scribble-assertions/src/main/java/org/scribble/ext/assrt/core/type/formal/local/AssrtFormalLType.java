package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.ext.assrt.core.type.formal.AssrtFormalType;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLAction;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.Optional;
import java.util.Set;


public interface AssrtFormalLType extends AssrtFormalType
{
    public final int SELECT_HASH = 14771;
    public final int BRANCH_HASH = 14779;
    public final int CHOICE_HASH = 14783;
    public final int SILENT_HASH = 14797;
    public final int END_HASH = 14813;
    public final int REC_HASH = 14821;
    public final int RECVAR_HASH = 14827;

    @Deprecated
    public final int TRANSFER_HASH = 10987;

    public final int EPSILON_HASH = 10993;
    public final int SEND_HASH = 11003;
    public final int RECEIVE_HASH = 11027;
    public final int COMM_HASH = 11047;
    public final int UNFOLD_HASH = 11057;
    public final int CONTINUE_HASH = 11059;

    Set<AssrtFormalLAction> getSteppable(AssrtLambda lambda);
    Optional<Pair<AssrtLambda, AssrtFormalLType>> step(AssrtLambda lambda, AssrtFormalLAction a);

    Set<AssrtFormalLAction> getInterSteppable(AssrtLambda lambda, AssrtRho rho);
    Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep(AssrtLambda lambda, AssrtFormalLAction a, AssrtRho rho);

    // Below AssrtLActions are concrete, i.e., not silent
    Set<AssrtFormalLAction> getDerivSteppable(AssrtLambda lambda, AssrtRho rho);
    Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> dstep(AssrtLambda lambda, AssrtRho rho, AssrtFormalLAction a);

}
