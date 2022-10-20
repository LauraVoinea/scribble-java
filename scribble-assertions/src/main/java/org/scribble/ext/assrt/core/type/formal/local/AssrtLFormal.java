package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.ext.assrt.core.type.formal.AssrtFormalType;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.Optional;
import java.util.Set;


public interface AssrtLFormal extends AssrtFormalType
{
    public final int SELECT_HASH = 14771;
    public final int BRANCH_HASH = 14779;
    public final int CHOICE_HASH = 14783;
    public final int SILENT_HASH = 14797;

    public final int TRANSFER_HASH = 10987;
    public final int EPSILON_HASH = 10993;
    public final int SEND_HASH = 11003;
    public final int RECEIVE_HASH = 11027;

    Set<AssrtLAction> getSteppable();
    Optional<Pair<AssrtLambda, AssrtLFormal>> step(AssrtLambda lambda, AssrtLAction a);

    Optional<Triple<AssrtLambda, AssrtLFormal, Rho>> dstep(AssrtLambda lambda, Rho rho, AssrtLAction a);

}
