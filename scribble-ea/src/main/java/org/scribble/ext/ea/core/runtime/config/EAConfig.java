package org.scribble.ext.ea.core.runtime.config;

import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;

// Configs (C, D) -- hiding, parallel, configs
public interface EAConfig {  // !!! "syntax" already contains "runtime names"

    int PARALLEL = 2161;
    int SESSION_ID = 2203;
    int PROCESS_ID = 2207;
    int RESTRICTION = 2213;
    int AP_ID = 2221;
    int IOTA_ID = 2237;

    Either<Exception, Tree<String>> type(Gamma gamma, Delta delta);

    default String toJudgementString(Gamma gamma, Delta delta) {
        return gamma + "; " + delta + " \u22a2 " + this;
    }

    boolean canEquals(Object o);
}
