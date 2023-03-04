package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLAction;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLComm;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLDerivedAction;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLEpsilon;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class AssrtFormalLSilent extends AssrtFormalTypeBase
		implements AssrtFormalLType {

	public final Map<Op, Pair<AssrtMsg, AssrtFormalLType>> cases;

	// Pre: cases.size() > 1
	protected AssrtFormalLSilent(LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLType>> cases) {
		this.cases = Collections.unmodifiableMap(new LinkedHashMap<>(cases));
	}

	@Override
	public Set<AssrtFormalLAction> getFormalSteppable(AssrtLambda lambda) {
		LinkedHashSet<AssrtFormalLAction> res = new LinkedHashSet<>();
		for (Pair<AssrtMsg, AssrtFormalLType> x : this.cases.values()) {
			List<AssrtAnnotDataName> pay = x.left.pay;
			int size = pay.size();
			/*if (size > 1) {
				throw new RuntimeException("Shouldn't get here: " + pay);
			} else if (size == 1) {*/
			if (size >= 1) {  // Now subsumes 0 case
				Optional<AssrtLambda> tmp = Optional.of(lambda);
				for (AssrtAnnotDataName d : pay) {
					 tmp = tmp.get().add(d.var, Multiplicity.ZERO, d.data);
					 if (!tmp.isPresent()) {
						 return Collections.emptySet();
					 }
				}
				res.add(new AssrtFormalLEpsilon(x.left));
			} else { // size == 0
				res.add(new AssrtFormalLEpsilon(x.left));
			}
		}

		return res;
	}

	@Override
	public Optional<Pair<AssrtLambda, AssrtFormalLType>> fstep(
			AssrtLambda lambda, AssrtFormalLAction a) {
		if (!(a instanceof AssrtFormalLEpsilon)) {
			return Optional.empty();
		}
		AssrtFormalLEpsilon cast = (AssrtFormalLEpsilon) a;
		if (!this.cases.containsKey(cast.msg.op)) {
			return Optional.empty();
		}
		List<AssrtAnnotDataName> pay = cast.msg.pay;
		int size = pay.size();
		/*if (size > 1) {
			throw new RuntimeException("TODO " + this + " ,, " + a);
		} else(*/ {
			AssrtLambda next;
			//if (size == 1) {
				if (size >= 1) {  // Now probably subsumes 0 case below
				//AssrtAnnotDataName d = pay.get(0);
				//Optional<AssrtLambda> add = lambda.add(d.var, Multiplicity.ZERO, d.data);
				Optional<AssrtLambda> add = Optional.of(lambda);
				for (AssrtAnnotDataName d1 : pay) {
					add = add.get().add(d1.var, Multiplicity.ZERO, d1.data);
					if (!add.isPresent()) {
						//throw new RuntimeException("Precondition violated: " + a);
						return Optional.empty();
					}
				}
				next = add.get();
			} else {
				next = lambda;
			}
			return Optional.of(new Pair<>(next, this.cases.get(cast.msg.op).right));
		}
	}

	@Override
	public Set<AssrtFormalLAction> getIntermedSteppable(AssrtLambda lambda, AssrtRho rho) {
		return getFormalSteppable(lambda);
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep(
			AssrtLambda lambda, AssrtFormalLAction a, AssrtRho rho) {
		Optional<Pair<AssrtLambda, AssrtFormalLType>> step = fstep(lambda, a);
		if (!step.isPresent()) {
			return Optional.empty();
		}
		Pair<AssrtLambda, AssrtFormalLType> res = step.get();
		return Optional.of(new Triple<>(res.left, res.right, rho));
	}

	// Pre: no infinite epsilon-only cycles -- CHECKME filtered by projection?
	@Override
	public Set<AssrtFormalLAction> getExplicitSteppable(AssrtLambda lambda, AssrtRho rho) {
		LinkedHashSet<AssrtFormalLAction> res = new LinkedHashSet();
		for (AssrtFormalLAction a : getIntermedSteppable(lambda, rho)) {  // Basically FormalSteppable (rho ignored)
			AssrtFormalLEpsilon cast = (AssrtFormalLEpsilon) a;
			Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep =
					istep(lambda, cast, rho);
			if (!istep.isPresent()) {
				throw new RuntimeException("Shouldn't get here " + cast);
			}
			Triple<AssrtLambda, AssrtFormalLType, AssrtRho> p = istep.get();
			Set<AssrtFormalLAction> ds = p.middle.getExplicitSteppable(p.left, p.right);
			res.addAll(ds.stream().map(x -> ((AssrtFormalLDerivedAction) x).prependSilent(cast.msg)).collect(Collectors.toList()));
		}
		return res;
	}

	@Override
	public Set<Triple<AssrtLambda, AssrtFormalLType, Set<RecVar>>> fastforwardEnters(AssrtLambda lambda, AssrtRho rho) {
		LinkedHashSet<Triple<AssrtLambda, AssrtFormalLType, Set<RecVar>>> res = new LinkedHashSet();
		for (AssrtFormalLAction a : getIntermedSteppable(lambda, rho)) {  // Basically FormalSteppable (rho ignored)
			AssrtFormalLEpsilon cast = (AssrtFormalLEpsilon) a;
			Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep =
					istep(lambda, cast, rho);
			if (!istep.isPresent()) {
				throw new RuntimeException("Shouldn't get here " + cast);
			}
			Triple<AssrtLambda, AssrtFormalLType, AssrtRho> p = istep.get();
			res.addAll(p.middle.fastforwardEnters(p.left, p.right));  // "silents" dropped (but collected via lambda), cf. getExplicitSteppable
		}
		return res;
	}

	/*@Override
	public Pair<AssrtLambda, AssrtFormalLChoice> bootstrap() {
		// ...bootstrap each L_i case -> gives lam_i and choice_i with concrete a's
		// ...prepend all of lam_i and op_i to each of a's
		// ...put all new a's in one big concrete choice
		// ...return empty lam and new big choice

		Map<Op, Pair<AssrtLambda, AssrtFormalLChoice>> bs
				= this.cases.entrySet().stream().collect(Collectors.toMap(
						Map.Entry::getKey,
						x -> x.getValue().right.bootstrap(),
						(x, y) -> null,
						LinkedHashMap::new));

		Map<Op, Pair<AssrtMsg, AssrtFormalLType>> cases = new LinkedHashMap<>();

		for (Map.Entry<Op, Pair<AssrtLambda, AssrtFormalLChoice>> e : bs.entrySet()) {
			Op op = e.getKey();
			Pair<AssrtLambda, AssrtFormalLChoice> v = e.getValue();
			// XXX action prepending is not a syntax thing
		}
	}*/

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> estep(
			AssrtLambda lambda, AssrtRho rho, AssrtFormalLAction a) {
		AssrtFormalLType t = this;
		AssrtFormalLDerivedAction cast = (AssrtFormalLDerivedAction) a;
		AssrtFormalLFactory lf = AssrtFormalLFactory.factory;
		for (AssrtMsg m : cast.getSilent()) {
			Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep =
					t.istep(lambda, lf.epsilon(m), rho);
			if (!istep.isPresent()) {
				throw new RuntimeException("Shouldn't get here: " + lambda + " ,, " + t + " ,, " + m);
			}
			Triple<AssrtLambda, AssrtFormalLType, AssrtRho> get = istep.get();  // No recursion -- rho unchanged
			lambda = get.left;
			t = get.middle;
			rho = get.right;
		}
		return t.estep(lambda, rho, ((AssrtFormalLDerivedAction) a).drop());  // Could be recvar, so dstep (for rho)
	}

	@Override
	public String toString() {
		String m = AssrtFormalLChoice.casesToString(this.cases);
		return m.startsWith(":") ? m.substring(1) : m;
	}
	
	@Override
	public int hashCode()
	{
		int hash = SILENT_HASH;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.cases.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (!(obj instanceof AssrtFormalLSilent))
		{
			return false;
		}
		AssrtFormalLSilent them = (AssrtFormalLSilent) obj;
		return super.equals(obj)  // Checks canEquals
			&& this.cases.equals(them.cases);
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtFormalLSilent;
	}
}
