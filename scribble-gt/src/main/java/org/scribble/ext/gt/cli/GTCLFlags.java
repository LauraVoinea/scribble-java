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
package org.scribble.ext.gt.cli;

import org.scribble.cli.CLFlag;
import org.scribble.cli.CLFlags;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;


// Flag Strings must start with "-", e.g., "-project"
// A Scribble extension should override getFlags
public class GTCLFlags extends CLFlags {

    // Unique flags
    public static final String NO_CORRESPONDENCE = "-nocorr";

    // Non-unique flags
    // ...

    public GTCLFlags() {
        super();
    }

    // Return a map of flag Strings to flag objects
    // A Scribble extension should override getFlags -- e.g., call super, then put any additional
    @Override
    protected Map<String, CLFlag> getFlags() {
        Map<String, CLFlag> flags = super.getFlags();

        // Unique; barrier irrelevant
        flags.put(NO_CORRESPONDENCE, new CLFlag(NO_CORRESPONDENCE,
                0, true, false, false, "Duplicate flag: "));

        return flags;
    }
}
