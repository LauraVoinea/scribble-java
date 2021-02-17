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

package org.scribble.core.type.session.base;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.session.Continue;
import org.scribble.core.type.session.Seq;

public abstract class ContinueBase<K extends ProtoKind, B extends Seq<K, B>>
        extends STypeBase<K, B> implements Continue<K, B> {

    protected final RecVar recvar;

    // Due to inlining, source may be do -> continue
    public ContinueBase(CommonTree source, RecVar recvar) {
        super(source);
        this.recvar = recvar;
    }

    @Override
    public RecVar getRecVar() {
        return this.recvar;
    }

    @Override
    public String toString() {
        return "continue " + this.recvar + ";";
    }

    @Override
    public int hashCode() {
        int hash = 3217;
        hash = 31 * hash + super.hashCode();
        hash = 31 * hash + this.recvar.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ContinueBase)) {
            return false;
        }
        ContinueBase<?, ?> them = (ContinueBase<?, ?>) o;
        return super.equals(o)  // Does canEquals
                && this.recvar.equals(them.recvar);
    }
}
