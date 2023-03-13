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
package org.scribble.core.model.endpoint;

import org.scribble.core.model.MPrettyPrint;

import java.util.Collections;
import java.util.Map;

public class EGraph implements MPrettyPrint {

    // Pre: states are "finalised"
    public final EState init;
    public final EState term;  // null for no term -- cf. EGraphBuilderUtil

    // Not used in hashCode/equals -- all states should be immut, so reachable constant
    protected Map<Integer, EState> cachedReachable = null;

    //protected EGraph(EState init, EState term)  // Use factory?
    public EGraph(EState init, EState term) {
        this.init = init;
        this.term = term;
    }

    public EFsm toFsm()  // CHECKME: refactor to mf?
    {
        return new EFsm(this);
    }

    // TODO replace any this.init.getReachable(+ added init) by this method
    public Map<Integer, EState> getReachable() {
        if (this.cachedReachable == null) {
            Map<Integer, EState> tmp = this.init.getReachableStates();
            tmp.put(this.init.id, this.init);
            this.cachedReachable = Collections.unmodifiableMap(tmp);
        }
        return this.cachedReachable;

    }

    @Override
    public String toDot() {
        return this.init.toDot();
    }

    @Override
    public String toAut() {
        return this.init.toAut();
    }

    @Override
    public String toString() {
        return this.init.toString();
    }

    @Override
    public final int hashCode() {
        int hash = 1051;
        //hash = 31 * hash + this.init.hashCode();  // Use init state only, OK since state IDs globally unique
        hash = 31 * hash + this.init.id;  // Use init state only, OK since state IDs globally unique
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof EGraph)) {
            return false;
        }
        EGraph them = (EGraph) o;
        return this.init.equals(them.init);
        // && this.term.equals(them.term);  // N.B. EState.equals checks state ID only, but OK because EStates have globally unique IDs -- any need to do a proper graph equality?
    }
}
