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
package org.scribble.core.model;

import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

// SAction never Dynamic
public interface MAction<K extends ProtoKind, A extends ActionKind> {

    MAction<K, DynamicActionKind> toDynamic();

    Role getObject();

    MsgId<?> getMid();

    Payload getPayload();

    // Used by toAut
    String toStringWithMsgIdHack();

    // Maybem move protected to MActionBase
    String getCommSymbol();

    default boolean isSend() {
        return false;
    }

    default boolean isReceive() {
        return false;
    }

    default boolean isRequest() {
        return false;
    }

    default boolean isDisconnect() {
        return false;
    }

    default boolean isAccept() {
        return false;
    }

    default boolean isClientWrap() {
        return false;
    }

    default boolean isServerWrap() {
        return false;
    }

    boolean canEquals(Object o);
}
