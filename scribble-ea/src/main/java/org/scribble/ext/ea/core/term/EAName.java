package org.scribble.ext.ea.core.term;

import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

public interface EAName {  // A term (has `type`)

    /*// TODO leave abstract
    //default EAVType type(GammaState gamma) {
    default Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        EAVType res = gamma.gamma.map.get(this);
        if (res == null) {
            //throw new RuntimeException("Type error: " + gamma + " ,, " + this);
            return Either.left(new Exception("Type error: " + gamma + " ,, " + this));
        }
        //return res;
        return Either.right(new Pair<>(res, new Tree<>("..name..")));
    }*/

    boolean canEquals(Object o);
}
