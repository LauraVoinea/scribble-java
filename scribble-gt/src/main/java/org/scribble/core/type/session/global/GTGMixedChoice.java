/*
 * Copyright 2008 The Scribble Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scribble.core.type.session.global;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.GTMixedChoice;
import org.scribble.core.type.session.SType;
import org.scribble.core.type.session.SVisitable;
import org.scribble.core.visit.STypeAgg;
import org.scribble.core.visit.STypeAggNoThrow;
import org.scribble.core.visit.Substitutor;
import org.scribble.core.visit.global.GTypeInliner;
import org.scribble.util.ScribException;

public class GTGMixedChoice extends GTMixedChoice<Global, GSeq> implements GType {

    public GTGMixedChoice(
            CommonTree source, Role other, Role observer, GSeq left, GSeq right) {
        super(source, other, observer, left, right);
    }

    @Override
    public GTGMixedChoice reconstruct(
            CommonTree source, Role other, Role observer,
            GSeq left, GSeq right) {
        return new GTGMixedChoice(source, other, observer, left, right);
    }

    // Adapted from base visitor -- would be there alongside base AST cases
    protected SType<Global, GSeq> visitMixedChoice(STypeAgg<Global, GSeq, SType<Global, GSeq>> v)
            throws ScribException {
        GSeq left = (GSeq) v.visitSeq(this.left);
        GSeq right = (GSeq) v.visitSeq(this.right);
        return reconstruct(getSource(), this.other, this.observer, left, right);
    }

    @Override
    public <T> T accept(STypeAgg<Global, GSeq, T> v) throws ScribException {
        //return v.visitChoice(this);
        throw new RuntimeException("TODO: " + v.getClass() + " ,, " + this);
    }

    // Adapted from base visitor -- would be there alongside base AST cases
    protected SType<Global, GSeq> visitNoThrowMixedChoice(STypeAggNoThrow<Global, GSeq, SVisitable<Global, GSeq>> v) {
        GSeq left = (GSeq) v.visitSeq(this.left);
        GSeq right = (GSeq) v.visitSeq(this.right);
        return reconstruct(getSource(), this.other, this.observer, left, right);
    }

    @Override
    public <T> T acceptNoThrow(STypeAggNoThrow<Global, GSeq, T> v) {

        // !!! XXX adding base visitor overrids for this new AST node on demand

        if (v instanceof Substitutor<?, ?>) {
            Substitutor<Global, GSeq> cast = (Substitutor<Global, GSeq>) v;
            Role other = cast.subs.subsRole(this.other, cast.passive);  // TODO refactor (subs/passive fields, base framework)
            Role observer = cast.subs.subsRole(this.observer, cast.passive);
            GSeq left = (GSeq) this.left.acceptNoThrow(cast);
            GSeq right = (GSeq) this.right.acceptNoThrow(cast);
            return (T) reconstruct(getSource(), other, observer, left, right);  // T = SType<Global, GSeq> implied by Subs/Visitor instantiation of T in Agg
            // CHECKME refactor to better satisfy <T> outside of Visitor class? as here
        } else if (v instanceof GTypeInliner) {
            return (T) visitNoThrowMixedChoice((GTypeInliner) v);  // T = SType<Global, GSeq>
        } else {
            //return v.visitChoice(this);
            throw new RuntimeException("TODO: " + v.getClass() + " ,, " + this);
        }
    }

    @Override
    public int hashCode() {
        int hash = 61487;
        hash = 31 * hash + super.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTGMixedChoice)) {
            return false;
        }
        return super.equals(o);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGMixedChoice;
    }
}




