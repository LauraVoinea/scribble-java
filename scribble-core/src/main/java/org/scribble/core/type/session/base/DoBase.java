package org.scribble.core.type.session.base;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.NonRoleParamKind;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Arg;
import org.scribble.core.type.session.Do;
import org.scribble.core.type.session.Seq;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public abstract class DoBase<K extends ProtoKind, B extends Seq<K, B>>
        extends SVisitableBase<K, B> implements Do<K, B> {

    protected final ProtoName<K> proto;  // Currently disamb'd to fullname by GTypeTranslator (see GDoDel::translate)
    protected final List<Role> roles;  // Ordered role args -- Pre: size>2, unmodifiable
    protected final List<Arg<? extends NonRoleParamKind>> args;  // Pre: unmodifiable
    // NonRoleParamKind, not NonRoleArgKind, because latter includes AmbigKind due to parsing requirements

    public DoBase(CommonTree source, ProtoName<K> proto,
                  List<Role> roles, List<Arg<? extends NonRoleParamKind>> args) {
        super(source);
        this.proto = proto;
        this.roles = Collections.unmodifiableList(roles);
        this.args = Collections.unmodifiableList(args);
    }

    @Override
    public ProtoName<K> getProto() {
        return this.proto;
    }

    @Override
    public List<Role> getRoles() {
        return this.roles;
    }

    @Override
    public List<Arg<? extends NonRoleParamKind>> getArgs() {
        return this.args;
    }

    @Override
    public String toString() {
        return "do " + this.proto
                + "<"
                + this.args.stream().map(x -> x.toString())
                .collect(Collectors.joining(", "))
                + ">"
                + "(" + this.roles.stream().map(x -> x.toString())
                .collect(Collectors.joining(", "))
                + ");";
    }

    @Override
    public int hashCode() {
        int hash = 193;
        hash = 31 * hash + super.hashCode();
        hash = 31 * hash + this.proto.hashCode();
        hash = 31 * hash + this.roles.hashCode();
        hash = 31 * hash + this.args.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DoBase)) {
            return false;
        }
        DoBase<?, ?> them = (DoBase<?, ?>) o;
        return super.equals(o)  // Does canEquals
                && this.proto.equals(them.proto) && this.roles.equals(them.roles)
                && this.args.equals(them.args);
    }
}














	/*
	@Override
	public SType<K, B> visitWith(STypeVisitor<K, B> v) throws ScribException
	{
		return v.visitDo(this);
	}

	@Override
	public SType<K, B> visitWithNoEx(STypeVisitorNoEx<K, B> v)
	{
		return v.visitDo(this);
	}

	@Override
	public Set<Role> getRoles()
	{
		return this.roles.stream().collect(Collectors.toSet());
	}

	@Override
	public Set<MessageId<?>> getMessageIds()
	{
		Set<MessageId<?>> mids = new HashSet<>();
		for (Arg<? extends NonRoleParamKind> a : this.args)
		{
			if (a instanceof Message)
			{
				mids.add(((Message) a).getId());
			}
		}
		return mids;
	}

	@Override
	public Set<RecVar> getRecVars()
	{
		return Collections.emptySet();
	}

	@Override
	public SType<K, B> unfoldAllOnce(STypeUnfolder<K> u)
	{
		throw new RuntimeException("Unsupported for Do: " + this);
	}

	@Override
	public List<ProtocolName<K>> getProtoDependencies()
	{
		return Stream.of(this.proto).collect(Collectors.toList());
	}

	@Override
	public List<MemberName<?>> getNonProtoDependencies()
	{
		return this.args.stream()
				.filter(x -> (x instanceof MessageSig) || (x instanceof DataType))  // CHECKME: refactor?
				.map(x -> (MemberName<?>) x).collect(Collectors.toList());
	}

	@Override
	public Do<K, B, N> pruneRecs()
	{
		return this;
	}

	@Override
	public Do<K, B, N> substitute(Substitutions subs)
	{
		List<Role> roles = this.roles.stream().map(x -> subs.subsRole(x))
				.collect(Collectors.toList());
		List<Arg<? extends NonRoleParamKind>> args = new LinkedList<>();
		for (Arg<? extends NonRoleParamKind> a : this.args)
		{
			if (a instanceof MemberName<?> && subs.hasArg((MemberName<?>) a))
			{
				if (a instanceof DataType)
				{
					a = subs.subsArg((DataType) a);
				}
				else if (a instanceof MessageSigName)
				{
					a = subs.subsArg((MessageSigName) a);
				}
			}
			args.add(a);
		}
		return reconstruct(getSource(), this.proto, roles, args);
	}
	*/
