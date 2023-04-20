package org.scribble.ext.ea.core.type;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EAName;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.type.value.EAVFuncType;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.LinkedHashMap;

public class GammaState {

    @NotNull public Gamma gamma;
    //@NotNull public final EAName svar;  // this.map contains [svar:svarType] if non-null
    @NotNull public final EAVType svarType;  // Config infers local state for handler typing

    public GammaState(EAVType svarType) {
        this(new LinkedHashMap<>(), new LinkedHashMap<>(), svarType);
    }

    public GammaState(@NotNull LinkedHashMap<EAName, EAVType> map,
                      @NotNull LinkedHashMap<EAEFuncName, EAVFuncType> fmap,
                      //@NotNull EAName svar,
                      @NotNull EAVType svarType) {
        this.gamma = new Gamma(map, fmap);
        //this.svar = svar;
        this.svarType = svarType;
    }

    @Override
    public String toString() {
        return this.gamma + " ,, " //+ this.svar
                + ": " + this.svarType;
    }

    /* equals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GammaState them = (GammaState) o;
        return this.gamma.equals(them.gamma)
                //&& this.svar.equals(them.svar)
                && this.svarType.equals(them.svarType);
    }

    @Override
    public int hashCode() {
        int hash = 3137;
        hash = 31 * hash + this.gamma.hashCode();
        //hash = 31 * hash + this.svar.hashCode();
        hash = 31 * hash + this.svarType.hashCode();
        return hash;
    }
}
