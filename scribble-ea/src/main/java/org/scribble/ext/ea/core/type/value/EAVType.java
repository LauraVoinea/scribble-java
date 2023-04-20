package org.scribble.ext.ea.core.type.value;

import org.scribble.ext.ea.core.type.EAType;

import java.util.Optional;

public interface EAVType extends EAType {

    //EAValType type(Gamma gamma);

    static Optional<EAVType> unify(EAVType A, EAVType B) {
        return A.equals(B) || A.equals(EAVWildType.WILD)
                ? Optional.of(B)
                : B.equals(EAVWildType.WILD)
                        ? Optional.of(A)
                        : Optional.empty();
    }
}
