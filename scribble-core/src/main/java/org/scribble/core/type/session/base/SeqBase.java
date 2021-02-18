package org.scribble.core.type.session.base;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.session.SType;
import org.scribble.core.type.session.Seq;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public abstract class SeqBase<K extends ProtoKind, B extends Seq<K, B>>
        extends STypeBase<K, B> implements Seq<K, B> {

    // GType or LType -- could make SType subclasses take themself as another param, but not worth it
    protected final List<SType<K, B>> elems;

    public SeqBase(CommonTree source, List<? extends SType<K, B>> elems) {
        super(source);
        this.elems = Collections.unmodifiableList(elems);
    }

    @Override
    public String toString() {
        return this.elems.stream().map(x -> x.toString())
                .collect(Collectors.joining("\n"));
    }

    @Override
    public int hashCode() {
        int hash = 1483;
        hash = 31 * hash + super.hashCode();
        hash = 31 * hash + this.elems.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SeqBase)) {
            return false;
        }
        SeqBase<?, ?> them = (SeqBase<?, ?>) o;
        return super.equals(this)  // Does canEquals
                && this.elems.equals(them.elems);
    }
}
