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

package org.scribble.core.type.session;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.session.base.RecursionBase;
import org.scribble.core.visit.STypeAgg;
import org.scribble.core.visit.STypeAggNoThrow;
import org.scribble.util.ScribException;

import java.util.function.Function;
import java.util.stream.Stream;

public interface Recursion<K extends ProtoKind, B extends Seq<K, B>>
        extends SType<K, B> {

    RecVar getRecVar();

    B getBody();

    // Corresponds to all getters (incl. super)
    RecursionBase<K, B> reconstruct(
            CommonTree source, RecVar recvar, B body);

    @Override
    default <T> T visitWith(STypeAgg<K, B, T> v) throws ScribException {
        return v.visitRecursion(this);
    }

    @Override
    default <T> T visitWithNoThrow(STypeAggNoThrow<K, B, T> v) {
        return v.visitRecursion(this);
    }

    @Override
    default <T> Stream<T> gather(Function<SType<K, B>, Stream<T>> f) {
        return Stream.concat(f.apply(this), getBody().gather(f));
    }
}
