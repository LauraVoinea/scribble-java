/**
 * Copyright 2008 The Scribble Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.scribble.ext.gt.del.global;

import org.scribble.ast.ScribNode;
import org.scribble.core.lang.global.GNode;
import org.scribble.del.global.GCompoundSessionNodeDel;
import org.scribble.ext.gt.del.GTMixedDel;
import org.scribble.visit.GTypeTranslator;

public class GTGMixedDel extends GTMixedDel implements GCompoundSessionNodeDel {

    @Override
    public GNode translate(ScribNode n, GTypeTranslator t) {
        return null;  // XXX cf. GInteractionSeqDel  // Currently Core not used -- GTCommandLine takes JobConfig parsed (post disamb)
        ////throw new RuntimeException("TODO");  // in base core MixedChoice, override visit methods to do general homomorphic visit of mixed choice

        /*
        GTGMixed cast = (GTGMixed) n;
        CommonTree src = cast.getSource();
        Role other = cast.getOtherChild().toName();
        Role observer = cast.getObserverChild().toName();
        GSeq left = (GSeq) cast.getLeftBlockChild().visitWith(t);
        GSeq right = (GSeq) cast.getRightBlockChild().visitWith(t);
        return ((GTGTypeFactoryImpl) t.tf.global).GTGMixedChoice(
                src, left, right, other, observer);
        //*/

    }
}
