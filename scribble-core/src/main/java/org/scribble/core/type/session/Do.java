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
import org.scribble.core.job.Core;
import org.scribble.core.lang.Protocol;
import org.scribble.core.type.kind.NonRoleParamKind;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.core.visit.STypeAgg;
import org.scribble.core.visit.STypeAggNoThrow;
import org.scribble.util.ScribException;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

public interface Do<K extends ProtoKind, B extends Seq<K, B>>
        extends SType<K, B> {

    // Not so useful to override concrete return: calling on Do<K, B> doesn't give the overridden return
    ProtoName<K> getProto();  // Currently disamb'd to fullname by GTypeTranslator (see GDoDel::translate)

    List<Role> getRoles();  // Ordered role args; pre: size > 2

    List<Arg<? extends NonRoleParamKind>> getArgs();

    Protocol<K, ?, B> getTarget(Core core);  // CHECK: "?"

    // Corresponds to all getters (incl. super)
    Do<K, B> reconstruct(
            CommonTree source, ProtoName<K> proto,
            List<Role> roles, List<Arg<? extends NonRoleParamKind>> args);

    @Override
    default <T> T visitWith(STypeAgg<K, B, T> v) throws ScribException {
        return v.visitDo(this);
    }

    @Override
    default <T> T visitWithNoThrow(STypeAggNoThrow<K, B, T> v) {
        return v.visitDo(this);
    }

    /*@Override
    default <T> Stream<T> gather(Function<SVisitable<K, B>, Stream<T>> f) {
        return f.apply(this);
    }*/
}

