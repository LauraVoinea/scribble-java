package org.scribble.ext.ea.core.type;

public interface EAType {

    int UNIT = 10529;
    int PID = 10531;
    int HANDLERS = 10559;
    int AP = 10567;

    int INT = 19867;
    int BOOL = 19889;
    int WILD = 19891;

    boolean canEquals(Object o);
}
