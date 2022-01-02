package org.scribble.ext.assrt.core.type.session.global.lts;

import org.scribble.ext.assrt.core.model.global.action.AssrtSSend;
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
}
