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

import java.util.Map;


// Flag Strings must start with "-", e.g., "-project"
// A Scribble extension should override getFlags
public class GTCLFlags extends CLFlags {

    // Unique flags
    @Deprecated
    public static final String NO_CORRESPONDENCE = "-nocorr";

    public static final String GT_NO_CORRESPONDENCE_FLAG = "-gt-no-corr";
    public static final String GT_JAVA_API_GEN_FLAG = "-gt-api-gen";

    public static final String GT_ED_FSM_GEN_FLAG = "-gt-gen-efsm";
    public static final String GT_ERLANG_API_GEN_FLAG = "-gt-gen-erlang";
    public static final String GT_ERLANG_ROLE_GEN_FLAG = "-gt-gen-erlang-role";

    public static final String GT_EXPLICIT_OBSERVER_LEFT_COMMITS = "-gt-explicit-observer-left-commits";

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
        flags.put(GT_NO_CORRESPONDENCE_FLAG, new CLFlag(GT_NO_CORRESPONDENCE_FLAG,
                0, true, false, false, "Duplicate flag: "));
        flags.put(GT_EXPLICIT_OBSERVER_LEFT_COMMITS, new CLFlag(GT_EXPLICIT_OBSERVER_LEFT_COMMITS,
                0, true, false, false, "Duplicate flag: "));

        flags.put(GT_JAVA_API_GEN_FLAG, new CLFlag(GT_JAVA_API_GEN_FLAG,
                0, true, false, false, "Duplicate flag: "));


        // // Non-unique, barrier
        /*flags.put(GT_API_GEN_FLAG,
                new CLFlag(GT_API_GEN_FLAG, 2, false, true, true,
                        "Missing protocol/role arguments: "));*/
        flags.put(GT_ED_FSM_GEN_FLAG,
                new CLFlag(GT_ED_FSM_GEN_FLAG, 2, false, true, true,
                        "Missing protocol/role arguments: "));
        flags.put(GT_ERLANG_API_GEN_FLAG,
                new CLFlag(GT_ERLANG_API_GEN_FLAG, 1, false, true, true,
                        "Missing protocol arguments: "));
        flags.put(GT_ERLANG_ROLE_GEN_FLAG,
                new CLFlag(GT_ERLANG_API_GEN_FLAG, 2, false, true, true,
                        "Missing protocol/role arguments: "));

        return flags;
    }
}
