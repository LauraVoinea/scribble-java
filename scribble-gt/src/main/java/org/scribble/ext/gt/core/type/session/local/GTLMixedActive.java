package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTENewTimeout;
import org.scribble.ext.gt.util.*;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class GTLMixedActive implements GTLType {

    protected final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    // TODO Just embed GTMixedChoice?
    public final int c;
    public final GTLType left;
    public final GTLType right;

    public final int n;

    protected GTLMixedActive(int c, int n, GTLType left, GTLType right) {
        this.c = c;
        this.n = n;
        this.left = left;
        this.right = right;
    }

    @Override
    public GTLMixedActive unfoldContext(Map<RecVar, GTLType> env) {
        GTLType left = this.left.unfoldContext(env);
        GTLType right = this.left.unfoldContext(env);
        return this.fact.mixedActive(this.c, this.n, left, right);
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLMixedActive)) {
            return Optional.empty();
        }
        GTLMixedActive cast = (GTLMixedActive) t;
        if (this.c != cast.c || this.n != cast.n
                || !this.left.equals(cast.left) || !this.right.equals(cast.right)) {
            return Optional.empty();
        }
        Optional<? extends GTLType> opt_l = this.left.merge(cast.left);
        Optional<? extends GTLType> opt_r = this.right.merge(cast.right);
        return opt_l.flatMap(x -> opt_r.map(y ->
                this.fact.mixedActive(this.c, this.n, x, y)));
    }

    // Pre: a in getActs
    @Override
    public Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> step(
            Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {

        /*if (!a.peer.equals(self)) {  // ...cf. "context" rule?
            return Optional.empty();
        }*/
        Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> optl =
                this.left.step(self, a, sigma, theta, this.c, this.n);
        Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> optr =
                this.right.step(self, a, sigma, theta, this.c, this.n);

        if (optl.isRight() && optr.isRight()) {
            throw new RuntimeException("TODO: " + optl.getRight() + " ,, " + optr.getRight());

        } else if (optl.isRight()) {
            Quad<GTLType, Sigma, Theta, Tree<String>> get = optl.getRight();
            if (a.isSend()) {
                // [LSnd]
                throw new RuntimeException("TODO");
            } else if (a.isReceive()) {
                // [LRcv1] or [LRcv2]
                throw new RuntimeException("TODO");
            } else if (a instanceof GTENewTimeout) {
                // [...ctx...] -- needed ?
                throw new RuntimeException("TODO");
            } else {
                throw new RuntimeException("TODO");
            }

        } else if (optr.isRight()) {
            Quad<GTLType, Sigma, Theta, Tree<String>> get = optr.getRight();
            if (a.isSend()) {
                // [RSnd]
                throw new RuntimeException("TODO");
            } else if (a.isReceive()) {
                // [RRcv]
                throw new RuntimeException("TODO");
            } else if (a instanceof GTENewTimeout) {
                // [...ctx...] -- needed ?
                throw new RuntimeException("TODO");
            } else {
                throw new RuntimeException("TODO");
            }

        } else {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
    }

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {  // XXX outer still OK to reduce if inner is fully ended?

        // TODO remove blocked

        LinkedHashSet<EAction<DynamicActionKind>> aLeft = this.left.getActs(mf, self, blocked, sigma, theta, this.c, this.n);
        LinkedHashSet<EAction<DynamicActionKind>> aRight = this.right.getActs(mf, self, blocked, sigma, theta, this.c, this.n);
        aLeft.addAll(aRight);
        return aLeft;
    }

    /* Aux */

    @Override
    public String toString() {
        return "(" + this.left + " " + ConsoleColors.BLACK_TRIANGLE
                + this.c + "," + this.n
                + " " + this.right + ")";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.MIXED_CHOICE_ACTIVE_HASH;
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.n;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLMixedActive)) return false;
        GTLMixedActive them = (GTLMixedActive) obj;
        return them.canEquals(this)
                && this.c == them.c
                && this.n == them.n
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLMixedActive;
    }
}
