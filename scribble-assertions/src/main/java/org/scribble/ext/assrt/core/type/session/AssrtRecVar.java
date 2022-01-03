package org.scribble.ext.assrt.core.type.session;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;


public abstract class AssrtRecVar<K extends ProtoKind, B extends AssrtSType<K, B>>
		extends AssrtSTypeBase<K, B>
{
	public final RecVar recvar;  // N.B. AssrtRecVar vs. RecVar...
	public final List<AssrtAFormula> stateexprs;
	
	protected AssrtRecVar(CommonTree source, RecVar rv,
			List<AssrtAFormula> annotexprs)
	{
		super(source);
		this.recvar = rv;
		this.stateexprs = Collections.unmodifiableList(annotexprs);
	}
	
	@Override
	public <T> Stream<T> assrtCoreGather(
			Function<AssrtSType<K, B>, Stream<T>> f)
	{
		return f.apply(this);
	}

	@Override
	public Map<AssrtVar, DataName> getBoundSortEnv(Map<AssrtVar, DataName> ctxt)
	{
		return Collections.emptyMap();
	}

	@Override 
	public String toString()
	{
		return this.recvar.toString()
				+ "<" + this.stateexprs.stream().map(x -> x.toString())  // CHECKME: factor out with AssrtCoreDo?
						.collect(Collectors.joining(", "))
				+ ">";
	}
	
	@Override
	public boolean equals(Object o)
	{
		if (!(o instanceof AssrtRecVar))
		{
			return false;
		}
		AssrtRecVar<?, ?> them = (AssrtRecVar<?, ?>) o;
		return super.equals(o) // Checks canEquals -- implicitly checks kind
				&& this.recvar.equals(them.recvar)
				&& this.stateexprs.equals(them.stateexprs);
	}

	@Override
	public int hashCode()
	{
		int hash = 6733;
		hash = 31*hash + this.recvar.hashCode();
		hash = 31*hash + this.stateexprs.hashCode();
		return hash;
	}
}
