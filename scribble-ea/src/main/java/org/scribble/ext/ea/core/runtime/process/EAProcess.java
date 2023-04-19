package org.scribble.ext.ea.core.runtime.process;

// Configs (C, D) -- hiding, parallel, configs
public interface EAProcess {  // !!! "syntax" already contains "runtime names"

    int PARALLEL = 2161;
    int SESSION_ID = 2203;
    int PROCESS_ID = 2207;
    int RESTRICTION = 2213;

    boolean canEquals(Object o);
}
