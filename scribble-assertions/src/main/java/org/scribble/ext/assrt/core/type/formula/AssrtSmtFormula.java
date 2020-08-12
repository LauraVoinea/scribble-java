package org.scribble.ext.assrt.core.type.formula;

import java.util.Map;
import java.util.Set;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

// Formula is a "top-level" base class, cf. (Abstract)Name 
// TODO: rename AnnotFormula?
// CHECKME: Formulae still treated as String in some places, e.g., AssrtESend
public abstract class AssrtSmtFormula
{

	// TODO: deprecate
	public abstract AssrtSmtFormula disamb(Map<AssrtVar, DataName> env);  // TODO: throws ScribException -- e.g., WF errors (getInlined comes before current WF pass)

	// Currently no redundant quantifier elimination
	public abstract AssrtSmtFormula squash();  // Needs to be here (not AssrtBoolFormula) because whole tree needs to be copied -- otherwise this.formula is inconsistent

	public abstract AssrtSmtFormula subs(AssrtAVarFormula old,
			AssrtAVarFormula neu);

	public abstract String toSmt2Formula(Map<AssrtVar, DataName> env);  // Cf. toString -- but can be useful to separate, for debugging (and printing)
	
	public abstract Set<AssrtVar> getIntVars();  // Change return to AssrtIntVarFormula? less confusing -- cf. AssrtEState.statevars
	
	// N.B. "syntactic" comparison -- should use additonal routines to do further, e.g., normal forms
	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtSmtFormula))
		{
			return false;
		}
		return ((AssrtSmtFormula) o).canEqual(this);
	}

	protected abstract boolean canEqual(Object o);
	
	// In case subclasses do super
	@Override
	public int hashCode()
	{
		return 5869;
	}
}
