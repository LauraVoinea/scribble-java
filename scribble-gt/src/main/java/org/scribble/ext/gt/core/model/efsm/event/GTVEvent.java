package org.scribble.ext.gt.core.model.efsm.event;

public interface GTVEvent {

    enum Kind {INTERNAL, EXTERNAL}

    Kind getKind();
}
