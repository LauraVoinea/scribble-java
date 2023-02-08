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
import org.scribble.core.type.name.Role;
import org.scribble.core.visit.STypeAgg;
import org.scribble.core.visit.STypeAggNoThrow;
import org.scribble.util.ScribException;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

public interface Choice<K extends ProtoKind, B extends Seq<K, B>>
        extends SType<K, B> {

    Role getSubject();

    List<B> getBlocks();

    // Corresponds to all getters (incl. super)
    Choice<K, B> reconstruct(CommonTree source, Role subj, List<B> blocks); //List<? extends Seq<K, B>> blocks);

    @Override
    default <T> T visitWith(STypeAgg<K, B, T> v) throws ScribException {
        return v.visitChoice(this);
    }

    @Override
    default <T> T visitWithNoThrow(STypeAggNoThrow<K, B, T> v) {
        return v.visitChoice(this);
    }

    /*@Override
    default <T> Stream<T> gather(Function<SVisitable<K, B>, Stream<T>> f) {
        return Stream.concat(f.apply(this),
                getBlocks().stream().flatMap(x -> x.gather(f)));
    }*/
}

