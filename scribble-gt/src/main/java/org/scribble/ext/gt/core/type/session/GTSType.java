package org.scribble.ext.gt.core.type.session;

import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.session.SType;
import org.scribble.core.type.session.Seq;

public interface GTSType { //<K extends ProtoKind, B extends Seq<K, B>> extends SType<K, B> {

    boolean canEquals(Object o);
}
