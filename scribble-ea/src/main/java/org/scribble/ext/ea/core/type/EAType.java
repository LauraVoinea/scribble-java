package org.scribble.ext.ea.core.type;

public interface EAType {

    int UNIT = 10529;
    int PID = 10531;
    int HANDLERS = 10559;

    boolean canEquals(Object o);
}
