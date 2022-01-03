package org.scribble.ext.assrt.core.type.session.global.lts;

import org.scribble.ext.assrt.core.model.global.action.AssrtSSend;
import org.scribble.ext.assrt.core.type.session.global.AssrtGChoice;
import org.scribble.ext.assrt.core.type.session.global.AssrtGType;
import org.scribble.ext.assrt.core.type.session.global.AssrtGTypeFactory;

import java.util.Optional;

public class AssrtGConfig {

    public final AssrtGEnv gamma;
    public final AssrtGType type;

    public AssrtGConfig(AssrtGEnv gamma, AssrtGType gtype) {
        this.gamma = gamma;
        this.type = gtype;
    }

    public AssrtGConfig step(AssrtGTypeFactory gf, AssrtSSend action) {
        Optional<AssrtGConfig> step = this.type.step(gf, this.gamma, action);
        if (!step.isPresent()) {
            throw new RuntimeException("Invalid step:\n\tglobal=" + this.type
                    + "\n\taction=" + action);
        }
        return step.get();
    }

    @Override
    public String toString()
    {
        return "(" + this.gamma + ", type=" + this.type + ")";
    }

    @Override
    public int hashCode()
    {
        int hash = 7607;
        hash = 31 * hash + this.gamma.hashCode();
        hash = 31 * hash + this.type.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
        {
            return true;
        }
        if (!(obj instanceof AssrtGConfig))
        {
            return false;
        }
        AssrtGConfig them = (AssrtGConfig) obj;
        return this.gamma.equals(them.gamma)
                && this.type.equals(them.type);
    }
}
