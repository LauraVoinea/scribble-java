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

package org.scribble.core.visit;

import org.scribble.core.type.kind.NonRoleParamKind;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.*;
import org.scribble.core.type.session.*;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class Substitutor<K extends ProtoKind, B extends Seq<K, B>>
        extends STypeVisitorNoThrow<K, B> {
    private Substitutions subs;
    private boolean passive;

    protected Substitutor(List<Role> rold, List<Role> rnew,
                          List<MemberName<? extends NonRoleParamKind>> aold,
                          List<Arg<? extends NonRoleParamKind>> anew, boolean passive) {
        this.subs = new Substitutions(rold, rnew, aold, anew);
        this.passive = passive;
    }

    @Override
    public SVisitable<K, B> visitChoice(Choice<K, B> n) {

        //List<B> blocks = n.getBlocks().stream().map(x -> visitSeq(x))
        List<B> blocks = n.getBlocks().stream().map(x -> (B) (Seq<K, B>) x.acceptNoThrow(this))  // FIXME unchecked cast -- B cast falsely subsumes Seq

                .collect(Collectors.toList());
        return n.reconstruct(n.getSource(),
                this.subs.subsRole(n.getSubject(), this.passive), blocks);
    }

    @Override
    public SVisitable<K, B> visitDirectedInteraction(DirectedInteraction<K, B> n) {
        Msg msg = n.msg;
        if (msg instanceof MemberName) {
            MemberName<?> name = (MemberName<?>) msg;
            if (this.subs.hasArg(name)) {
                msg = (Msg) this.subs.subsArg(name, this.passive);
            }
        }
        return n.reconstruct(n.getSource(), msg,
                this.subs.subsRole(n.src, this.passive),
                this.subs.subsRole(n.dst, this.passive));
    }

    @Override
    public SVisitable<K, B> visitDisconnect(DisconnectAction<K, B> n) {
        return n.reconstruct(n.getSource(),
                this.subs.subsRole(n.left, this.passive),
                this.subs.subsRole(n.right, this.passive));
    }

    @Override
    public SVisitable<K, B> visitDo(Do<K, B> n) {
        List<Role> roles = n.getRoles().stream()
                .map(x -> this.subs.subsRole(x, this.passive))
                .collect(Collectors.toList());
        List<Arg<? extends NonRoleParamKind>> args = new LinkedList<>();
        for (Arg<? extends NonRoleParamKind> a : n.getArgs()) {
            if (a instanceof MemberName<?> && this.subs.hasArg((MemberName<?>) a)) {
                if (a instanceof DataName) {
                    a = this.subs.subsArg((DataName) a, this.passive);
                } else if (a instanceof SigName) {
                    a = this.subs.subsArg((SigName) a, this.passive);
                }
            }
            args.add(a);
        }
        return n.reconstruct(n.getSource(), n.getProto(), roles, args);
    }
}
