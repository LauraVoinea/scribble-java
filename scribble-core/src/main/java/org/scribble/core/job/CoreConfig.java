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
package org.scribble.core.job;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.scribble.core.model.ModelFactory;
import org.scribble.core.type.name.ModuleName;
import org.scribble.core.type.session.STypeFactory;
import org.scribble.core.visit.STypeVisitorFactory;
import org.scribble.util.RuntimeScribException;

// The "static" (constant) info for Jobs -- cf. JobContext "dynamic" state
public class CoreConfig {
   
    public final ModuleName main;  // Full name

    private final Map<CoreArgs, Boolean> args;  // CHECKME: verbose/debug printing parameter ?

    public final STypeFactory tf;
    public final STypeVisitorFactory vf;
    public final ModelFactory mf;

    // N.B. MainContext is in a different non-visible (by Maven) package
    public CoreConfig(ModuleName main, Map<CoreArgs, Boolean> args,
                      STypeFactory tf, STypeVisitorFactory vf, ModelFactory mf) {
        this.main = main;
        Map<CoreArgs, Boolean> copy = new HashMap<>(args);
        checkCoreArgs(copy);
        this.args = Collections.unmodifiableMap(copy);
        this.tf = tf;
        this.vf = vf;
        this.mf = mf;
    }

    public boolean hasFlag(CoreArgs a) {
        return isTrue(this.args.get(a));
    }

    protected void checkCoreArgs(Map<CoreArgs, Boolean> args) {
        if (isTrue(args.get(CoreArgs.SCRIBBLE_SINGLE_BUFFERS)) && isTrue(args.get(CoreArgs.SCRIBBLE_UNBOUNDED_BUFFERS))) {
            throw new RuntimeScribException("Arg clash: " + CoreArgs.SCRIBBLE_SINGLE_BUFFERS + ", " + CoreArgs.SCRIBBLE_UNBOUNDED_BUFFERS);
        }
        if (!isTrue(args.get(CoreArgs.SCRIBBLE_SINGLE_BUFFERS)) && !isTrue(args.get(CoreArgs.SCRIBBLE_UNBOUNDED_BUFFERS))) {
            args.put(CoreArgs.SCRIBBLE_SINGLE_BUFFERS, true);  // Default
        }
    }

    private static final boolean isTrue(Boolean b) { return b != null && b; }
}
