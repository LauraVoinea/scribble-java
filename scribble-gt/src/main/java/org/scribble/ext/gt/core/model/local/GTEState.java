package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.MPrettyState;
import org.scribble.core.model.MState;
import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.EStateKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.name.RecVar;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class GTEState extends MPrettyState<RecVar, EAction<StaticActionKind>, GTEState, Local> {  // GTEAction currently isn't an EAction

    protected GTEState(Set<RecVar> labs) {
        super(labs);
    }

    @Override
    public Map<Integer, GTEState> getReachableStates() {
        return getReachableStatesAux(this);
    }

    @Override
    public void addEdge(EAction<StaticActionKind> a, GTEState s) {
        super.addEdge(a, s);
    }

    @Override
    public int hashCode() {
        int hash = 1709;
        hash = 31 * hash + super.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTEState)) {
            return false;
        }
        return super.equals(o);  // Checks canEquals
    }

    @Override
    protected boolean canEquals(MState<?, ?, ?, ?> s) {
        return s instanceof GTEState;
    }


    /* ... */

    // Dup from EState
    public EStateKind getStateKind() {
        List<EAction<StaticActionKind>> as = this.getActions();
        if (as.size() == 0) {
            return EStateKind.TERMINAL;
        } else {
            if (as.stream()
                  .allMatch(a -> a.isSend())) // || a.isRequest() || a.isClientWrap()))  // ClientWrap should be unary?
            {
                return EStateKind.OUTPUT;
            } else if (as.stream().allMatch(EAction<StaticActionKind>::isReceive)) {
                return (as.size() == 1) ? EStateKind.UNARY_RECEIVE : EStateKind.POLY_RECIEVE;
            }

            /*else if (as.size() == 2) {
                // mixed choice -- two actions, one I one O
            }*/

            /*else if (as.stream().allMatch(EAction<StaticActionKind>::isAccept)) {
                return EStateKind.ACCEPT;  // Distinguish unary for API gen?  cf. receive
            } else if (as.size() == 1 && as.get(0).isDisconnect()) {
                return EStateKind.OUTPUT;
            } else if (as.size() == 1 && as.get(0).isServerWrap()) {
                return EStateKind.SERVER_WRAP;
            }*/
            else {
                throw new RuntimeException("TODO: " + as);
            }
        }
    }

}
