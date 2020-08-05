package org.scribble.ext.assrt.core.type.session.global;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.name.Substitutions;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtSType;
import org.scribble.ext.assrt.core.type.session.AssrtSyntaxException;
import org.scribble.ext.assrt.core.type.session.local.AssrtLType;
import org.scribble.ext.assrt.core.visit.global.AssrtGTypeInliner;


public interface AssrtGType extends AssrtSType<Global, AssrtGType>
{
	
	// CHECKME: refactor as visitors? -- base Core visitor pattern not currently ported

	// TODO: deprecate -- all vars now Assrt(Int)Var
	AssrtGType disamb(AssrtCore core, Map<AssrtVar, DataName> env);  // FIXME: throw ScribbleException, WF errors
	
	// Assrt overrides base STypeVisitor pattern -- explicitly declare each pass here ("Visitor" is just dumb data holder), instead of generic visitWith
	// CHECKME: some may need to be factored up to base
	AssrtGType substitute(AssrtCore core, Substitutions subs);
	AssrtGType checkDoArgs(AssrtCore core);
	AssrtGType inline(AssrtGTypeInliner v);
	AssrtGType pruneRecs(AssrtCore core);

	AssrtLType projectInlined(AssrtCore core, Role self, AssrtBFormula f,
			Map<Role, Set<AssrtVar>> known,
			Map<RecVar, LinkedHashMap<AssrtVar, Role>> located,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)  // N.B. phantom payvars have no init exprs (cf. statevars) -- FIXME: phantom should map sorts
			throws AssrtSyntaxException;  // N.B. checking "mergability"
	
	List<AssrtAnnotDataName> collectAnnotDataVarDecls(
			Map<AssrtVar, DataName> env);  // Currently only the vars are needed (not the data types)
}

