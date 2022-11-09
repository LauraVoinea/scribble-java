package org.scribble.ext.assrt.core.model.formal.endpoint;

import org.scribble.ext.assrt.core.model.endpoint.AssrtEState;
import org.scribble.ext.assrt.core.type.formal.local.AssrtLambda;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.util.AssrtUtil;
import org.scribble.util.Pair;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class RCA {
    public final Set<RCAState> S;
    public final Map<RCAState, Pair<AssrtLAction, RCAState>> delta;
    public final Map<RCAState, AssrtLambda> sigma;

    public RCA() {
        this.S = new HashSet<>();
        this.delta = new LinkedHashMap<>();
        this.sigma = new LinkedHashMap<>();
    }

    @Override
    public String toString() {
        return "(S=" + this.S + "; delta=" + "[" +
                this.delta.entrySet().stream().map(x -> x.getKey() + "->" + AssrtUtil.pairToString(x.getValue())).collect(Collectors.joining(", ")) + "]" +
                "; sigma=" + this.sigma + ")";
    }

    @Override
    public int hashCode()
    {
        int hash = 23071;
        hash = 31 * hash + this.S.hashCode();
        hash = 31 * hash + this.delta.hashCode();
        hash = 31 * hash + this.sigma.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (!(o instanceof RCA))
        {
            return false;
        }
        RCA them = (RCA) o;
        return this.S.equals(them.S) &&
                this.delta.equals(them.delta) && this.sigma.equals(them.sigma);
    }
}
