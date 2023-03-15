package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.global.action.AssrtFormalGComm;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class AssrtFormalGChoice extends AssrtFormalTypeBase
        implements AssrtFormalGType {

    public final Role sender;
    public final Role receiver;
    public final Map<Op, Pair<AssrtMsg, AssrtFormalGType>> cases;  // Invariant: op.equals(assrtMsg)

    // Pre: cases.size() > 1
    protected AssrtFormalGChoice(Role sender, Role receiver,
                                 LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalGType>> cases) {
        this.sender = sender;
        this.receiver = receiver;
        this.cases = Collections.unmodifiableMap(new LinkedHashMap<>(cases));
    }

    @Override
    public AssrtFormalGType unfoldEnv(Map<RecVar, AssrtFormalGRec> env) {
        LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalGType>> cases =
                this.cases.entrySet().stream().collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> {
                            Pair<AssrtMsg, AssrtFormalGType> p = x.getValue();
                            return new Pair<>(p.left, p.right.unfoldEnv(env));
                        },
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return new AssrtFormalGChoice(this.sender, this.receiver, cases);  // FIXME factory
    }

    @Override
    public Set<AssrtFormalGComm> getActions(AssrtGamma gamma, Set<Role> blocked) {
        Set<AssrtFormalGComm> res = new HashSet<>();
        Set<Role> rs = Stream.of(this.sender, this.receiver).collect(Collectors.toSet());

        if (!blocked.contains(this.sender) && !blocked.contains(this.receiver)) {
            for (Map.Entry<Op, Pair<AssrtMsg, AssrtFormalGType>> e : this.cases.entrySet()) {
                Op op = e.getKey();
                Pair<AssrtMsg, AssrtFormalGType> p = e.getValue();
                Optional<AssrtGamma> tmp = Optional.of(gamma);
                for (AssrtAnnotDataName d : p.left.pay) {  // Not assuming can check canAdd seaprately... (overly cautious)
                    tmp = tmp.flatMap(x -> x.addNohat(d.var, rs, d.data));
                }
                if (tmp.isPresent()) {
                    AssrtMsg msg = new AssrtMsg(op, p.left.pay, p.left.ass, null, null);  // !!! null phantoms
                    res.add(new AssrtFormalGComm(this.sender, this.receiver, msg));// FIXME use factory
                }
            }
        }

        HashSet<Role> bs = new HashSet<>(blocked);
        bs.add(this.sender);
        bs.add(this.receiver);
        if (this.cases.size() == 1) { // !!!
            for (Pair<AssrtMsg, AssrtFormalGType> p : this.cases.values()) {
                Optional<AssrtGamma> tmp = Optional.of(gamma);
                for (AssrtAnnotDataName d : p.left.pay) {  // Not assuming can check canAdd separately... (overly cautious)
                    tmp = tmp.flatMap(x -> x.addHat(d.var, rs, d.data));
                }
                tmp.map(x -> res.addAll(p.right.getActions(x, bs)));
            }
        }

        return res;
    }

    @Override
    public Optional<Pair<AssrtGamma, AssrtFormalGType>> step(AssrtGamma gamma, AssrtFormalGComm a) {
        System.out.println("2222222: " + this + " ,, " + a);
        if (a.src.equals(this.sender)) {
            System.out.println("333333: " + a.msg + " ,, " + this.cases.get(a.msg.op).left);
            if (a.dst.equals(this.receiver) && Objects.equals(a.msg, this.cases.get(a.msg.op).left)) {
                System.out.println("444444444");
                Set<Role> rs = Stream.of(this.sender, this.receiver).collect(Collectors.toSet());
                Optional<AssrtGamma> tmp = Optional.of(gamma);
                for (AssrtAnnotDataName d : a.msg.pay) {
                    tmp = tmp.flatMap(x -> x.addHat(d.var, rs, d.data));
                }
                return tmp.map(x -> new Pair<>(x, this.cases.get(a.msg.op).right));
            } else {
                return Optional.empty();
            }
        } else if (a.dst.equals(this.receiver)) {
            return Optional.empty();
        } else if (this.cases.size() == 1) {
            Map.Entry<Op, Pair<AssrtMsg, AssrtFormalGType>> e = this.cases.entrySet().iterator().next();
            Op op = e.getKey();
            Pair<AssrtMsg, AssrtFormalGType> p = e.getValue();
            Optional<Pair<AssrtGamma, AssrtFormalGType>> opt = p.right.step(gamma, a);
            if (!opt.isPresent()) {
                return Optional.empty();
            }
            Pair<AssrtGamma, AssrtFormalGType> step = opt.get();
            LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalGType>> tmp =
                    new LinkedHashMap<>(this.cases);
            tmp.put(op, new Pair<>(p.left, step.right));
            return Optional.of(new Pair<>(step.left, new AssrtFormalGChoice(this.sender, this.receiver, tmp)));
        } else {
            return Optional.empty();
        }
    }

    @Override
    public AssrtFormalLType project(AssrtFormalLFactory lf, Role r, AssrtPhi phi) {
        LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLType>> cases =
                this.cases.entrySet().stream().collect(Collectors.toMap(
                        x -> x.getKey(),
                        x -> {
                            Pair<AssrtMsg, AssrtFormalGType> v = x.getValue();
                            return new Pair<>(v.left, v.right.project(lf, r, phi));
                        },
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        if (this.sender.equals(r)) {
            return lf.select(this.receiver, cases);
        } else if (this.receiver.equals(r)) {
            return lf.branch(this.sender, cases);
        } else {
            return lf.silent(cases);
        }
    }

    @Override
    public Set<Role> getRoles() {
        Set<Role> rs = new HashSet<>();
        rs.add(this.sender);
        rs.add(this.receiver);
        rs.addAll(this.cases.values().stream()
                .flatMap(x -> x.right.getRoles().stream()).collect(Collectors.toSet()));
        return rs;
    }

	/*@Override
	public Optional<Pair<AssrtLambda, AssrtLFormal>> step(
			AssrtLambda lambda, AssrtLAction a) {
		if (!(a instanceof AssrtLTransfer)) {
			return Optional.empty();
		}
		AssrtLTransfer cast = (AssrtLTransfer) a;
		if (!cast.receiver.equals(this.peer)
				|| !this.cases.containsKey(cast.msg.op)) {
			return Optional.empty();
		}
		List<AssrtAnnotDataName> pay = cast.msg.pay;
		if (pay.size() != 1) {
			throw new RuntimeException("TODO " + this + " ,, " + a);
		}
		AssrtAnnotDataName d = pay.get(0);
		Optional<AssrtLambda> add = lambda.add(d.var, Multiplicity.OMEGA, d.data);
		if (!add.isPresent()) {
			return Optional.empty();
		}
		return Optional.of(new Pair<>(add.get(), this.cases.get(cast.msg.op).right));
	}

	@Override
	public Set<AssrtLAction> getSteppable() {
		return this.cases.values().stream()
				.map(x -> new AssrtLTransfer(null, this.peer, x.left))
				.collect(Collectors.toSet());
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtLFormal, Rho>> dstep(
			AssrtLambda lambda, Rho rho, AssrtLAction a) {

		throw new RuntimeException("TODO");

	}*/

    @Override
    public String toString() {
        return this.sender + " -> " + this.receiver + " " +
                AssrtFormalGChoice.casesToString(this.cases);
    }

    @Override
    public int hashCode() {
        int hash = CHOICE_HASH;
        hash = 31 * hash + super.hashCode();
        hash = 31 * hash + this.sender.hashCode();
        hash = 31 * hash + this.receiver.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof AssrtFormalGChoice)) {
            return false;
        }
        AssrtFormalGChoice them = (AssrtFormalGChoice) obj;
        return super.equals(obj)  // Checks canEquals
                && this.sender.equals(them.sender)
                && this.receiver.equals(them.receiver)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof AssrtFormalGChoice;
    }

    // Duplicated from AssrtFormalLChoice
    protected static String casesToString(Map<Op, Pair<AssrtMsg, AssrtFormalGType>> cases) {
        String m = cases.values().stream()
                .map(e -> msgToString(e.left) + "." + e.right)
                .collect(Collectors.joining(", "));
        m = cases.size() > 1
                ? "{ " + m + " }"
                : ": " + m;
        return m;
    }

    protected static String msgToString(AssrtMsg m) {
        return m.op + "(" +
                m.pay.stream().map(x -> x.var + ":" + x.data)
                        .collect(Collectors.joining(", ")) +
                "){" + m.ass + "}";
    }
}
