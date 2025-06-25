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
package org.scribble.ext.gt.ast.global;

import org.antlr.runtime.Token;
import org.scribble.ast.name.simple.RoleNode;
import org.scribble.core.type.name.Role;
import org.scribble.del.DelFactory;
import org.w3c.dom.Node;

public class GTAnnotNode extends RoleNode {
    //public static final RoleNode SELF = ...;  // No: no token info

    // Scribble.g, IDENTIFIER<...Node>[$IDENTIFIER]
    // N.B. ttype (an "imaginary node" type) is discarded, t is a ScribbleParser.ID token type
    public GTAnnotNode(Token t) {
        super(-1, t);
    }

    public GTAnnotNode(RoleNode n) {
        super(n);
    }

    @Override
    public GTAnnotNode dupNode() {
        return new GTAnnotNode(this);
    }

    @Override
    public void decorateDel(DelFactory df) {
        df.RoleNode(this);
    }

    @Override
    public Role toName() {
        return new Role(getText());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTAnnotNode)) {
            return false;
        }
        return ((GTAnnotNode) o).canEquals(this) && super.equals(o);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTAnnotNode;
    }

    @Override
    public int hashCode() {
        int hash = 353;
        hash = 31 * super.hashCode();
        return hash;
    }
}
