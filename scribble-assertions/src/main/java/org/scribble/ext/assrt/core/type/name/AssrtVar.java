package org.scribble.ext.assrt.core.type.name;

import java.util.Map;

import org.scribble.core.type.name.AbstractName;
import org.scribble.ext.assrt.core.type.kind.AssrtVarKind;

// TODO: integrate with AssrtVarFormula
// CHECKME: AssrtVar a different syntactic category than the SmtFormula vars --
// unify?
// FIXME: String formulae coming under AFormula
public class AssrtVar extends AbstractName<AssrtVarKind>
		implements AssrtPayElemType<AssrtVarKind>
{
	private static final long serialVersionUID = 1L;

	//public final String sort; // TODO: refactor -- // CHECKME: AssrtSort?

	public AssrtVar(String simplename)
	{
		super(AssrtVarKind.KIND, simplename);
		//this.sort = sort;
	}

	public String getSort(Map<AssrtVar, String> env)
	{
		if (!env.containsKey(this))
		{
			throw new RuntimeException("[assrt-core] Unknown var: " + this);
		}
		return env.get(this);
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtVar))
		{
			return false;
		}
		return super.equals(o);  // Checks canEquals
		//&& this.sort.equals(((AssrtIntVar) o).sort);
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtVar;
	}

	@Override
	public int hashCode()
	{
		int hash = 5519;
		hash = 31 * hash + super.hashCode();
		//hash = 31 * hash + this.sort.hashCode();
		return hash;
	}
	
	@Override
	public boolean isAnnotVarName()
	{
		return true;
	}
}
