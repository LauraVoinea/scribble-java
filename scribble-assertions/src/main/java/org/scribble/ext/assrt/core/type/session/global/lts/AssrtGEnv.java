package org.scribble.ext.assrt.core.type.session.global.lts;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBinBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtFormulaFactory;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

import java.util.AbstractMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// Gamma
public class AssrtGEnv {

    // Invar: know.keySet().equals(sorts.keySet())
    public final Map<AssrtVar, Set<Role>> know;
    public final Map<AssrtVar, DataName> sorts;
    public final AssrtBFormula ass;

    public AssrtGEnv(Map<AssrtVar, Set<Role>> know,
                     Map<AssrtVar, DataName> sorts,
                     AssrtBFormula ass) {
        this.know = know.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().stream().collect(Collectors.toSet())));
        this.sorts = sorts.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        this.ass = ass;
    }

    public AssrtGEnv extend(AssrtVar x, Set<Role> ps, DataName t, AssrtBFormula ass) {
        if (this.know.keySet().contains(x)) {
            Set<Role> roles = this.know.get(x);
            if (this.sorts.get(x).equals(t)) {  // !!! FIXME need to compare this.ass and ass w.r.t. x -- XXX
                if (roles.isEmpty()) {
                    Map<AssrtVar, Set<Role>> know_ = Stream.concat(
                            this.know.entrySet().stream().filter(y -> !y.equals(x)),
                            Stream.of(new AbstractMap.SimpleEntry<>(x, ps))
                    ).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
                    AssrtBFormula ass_ = AssrtFormulaFactory.AssrtBinBool(
                            AssrtBinBFormula.Op.And, this.ass, ass);
                    return new AssrtGEnv(know_, this.sorts, ass_);
                } else if (roles.equals(ps)) {
                    return this;
                } else {
                    throw new RuntimeException("Undefined:\n\tthis=" + toString()
                            + "\n\tx=" + x + ", ps=" + ps + ", t=" + t + ", ass=" + ass);
                }
            } else {
                throw new RuntimeException("Undefined:\n\tthis=" + toString()
                        + "\n\tt=" + t + ", ass=" + ass);
            }
        } else {
            // TODO factor out `update` with earlier case
            Map<AssrtVar, Set<Role>> know_ = Stream.concat(
                    this.know.entrySet().stream(),
                    Stream.of(new AbstractMap.SimpleEntry<>(x, ps))
            ).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
            Map<AssrtVar, DataName> sorts_ = Stream.concat(
                    this.sorts.entrySet().stream(),
                    Stream.of(new AbstractMap.SimpleEntry<>(x, t))
            ).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
            AssrtBFormula ass_ = AssrtFormulaFactory.AssrtBinBool(
                    AssrtBinBFormula.Op.And, this.ass, ass);
            return new AssrtGEnv(know_, sorts_, ass_);
        }
    }


    @Override
    public int hashCode()
    {
        int hash = 7603;
        hash = 31*hash + this.know.hashCode();
        hash = 31*hash + this.sorts.hashCode();
        hash = 31*hash + this.ass.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (!(o instanceof AssrtGEnv))
        {
            return false;
        }
        AssrtGEnv them = (AssrtGEnv) o;
        return this.know.equals(them.know) && this.sorts.equals((them.sorts))
                && this.ass.equals(them.ass);
    }
}
