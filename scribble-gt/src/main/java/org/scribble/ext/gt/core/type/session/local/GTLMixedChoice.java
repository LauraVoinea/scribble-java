package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// HERE extend ANTLR -- copy frontend stuff from scrib-assrt
public class GTLMixedChoice implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final GTLType left;
    public final GTLType right;

    protected GTLMixedChoice(
            GTLType left, GTLType right) {
        this.left = left;
        this.right = right;
    }

    // Pre: a in getActs
    // Deterministic w.r.t. a -- CHECKME: recursion
    // !!! TODO if all roles committed, can drop either l or r?
    @Override
    public Optional<GTLType> step(EAction a) {
        /*LinkedHashSet<Role> cl = new LinkedHashSet<>(this.committedLeft);
        LinkedHashSet<Role> cr = new LinkedHashSet<>(this.committedRight);
        Optional<GTLType> opt = this.left.step(a);
        if (opt.isPresent()) {
            GTLType get = opt.get();
            if (a.isReceive()) {
                cl.add(a.subj);  // !!! l* problem -- but why not always commit as in [lcrv] ?  [rrcv] will "correct" -- invariant: in l xor r, not both
            } else if (!a.isSend()) {
                throw new RuntimeException("TODO: " + a);
            }
            return Optional.of(this.fact.mixedChoice(get, this.right, cl, cr));
        } else {
            GTLType get = this.right.step(a).get();  // Pre: a in getActs, so non-empty
            if (a.isSend()) {
                cr.add(a.subj);
            } else if (a.isReceive()) {
                cl.remove(a.subj);
                cr.add(a.subj);
            } else {
                throw new RuntimeException("TODO: " + a);
            }
            return Optional.of(this.fact.mixedChoice(this.left, get, cl, cr));
        }*/
        throw new RuntimeException("TODO");
    }

    @Override
    public LinkedHashSet<EAction> getActs(EModelFactory mf, Set<Role> blocked) {
        /*Set<Role> bLeft = Stream.concat(blocked.stream(),
                this.committedRight.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction> aLeft = this.left.getActs(mf, bLeft);
        Set<Role> bRight = Stream.concat(blocked.stream(),
                this.committedLeft.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction> aRight = this.right.getActs(mf, bRight);
        aLeft.addAll(aRight);
        return aLeft;*/
        throw new RuntimeException("TODO");
    }

    /* Aux */

    @Override
    public String toString() {
        return this.left + " |> " + this.right;
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.MIXED_CHOICE_HASH;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLMixedChoice)) return false;
        GTLMixedChoice them = (GTLMixedChoice) obj;
        return them.canEquals(this)
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLMixedChoice;
    }
}
