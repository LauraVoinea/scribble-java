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
    public final int ENTER_HASH = 11057;
    public final int CONTINUE_HASH = 11059;

    // Formal defs
    Set<AssrtFormalLAction> getFormalSteppable(AssrtLambda lambda);
    Optional<Pair<AssrtLambda, AssrtFormalLType>> fstep(AssrtLambda lambda, AssrtFormalLAction a);

    // With explicit, finite "recursion actions" instead of unbounded implicit unfolding
    Set<AssrtFormalLAction> getIntermedSteppable(AssrtLambda lambda, AssrtRho rho);
    Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep(AssrtLambda lambda, AssrtFormalLAction a, AssrtRho rho);

    // With epsilons squashed (called "intermed LTS" in draft) -- all below AssrtLActions are concrete, i.e., not silent
    Set<AssrtFormalLAction> getExplicitSteppable(AssrtLambda lambda, AssrtRho rho);
    Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> estep(AssrtLambda lambda, AssrtRho rho, AssrtFormalLAction a);

}
