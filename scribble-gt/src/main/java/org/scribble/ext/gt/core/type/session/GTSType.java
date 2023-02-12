package org.scribble.ext.gt.core.type.session;

import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.session.SType;
import org.scribble.core.type.session.Seq;
import org.scribble.ext.gt.core.type.session.global.GTGType;

import java.util.Collections;

public interface GTSType { //<K extends ProtoKind, B extends Seq<K, B>> extends SType<K, B> {

    GTSType unfold();

    boolean canEquals(Object o);

}
