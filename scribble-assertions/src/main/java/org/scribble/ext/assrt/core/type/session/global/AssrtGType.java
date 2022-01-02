package org.scribble.ext.assrt.core.type.session.global;

import java.util.*;

import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.name.Substitutions;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.model.global.AssrtSModelFactory;
import org.scribble.ext.assrt.core.model.global.action.AssrtSSend;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.AssrtSType;
import org.scribble.ext.assrt.core.type.session.AssrtSyntaxException;
import org.scribble.ext.assrt.core.type.session.global.lts.AssrtGConfig;
import org.scribble.ext.assrt.core.type.session.global.lts.AssrtGEnv;
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

	// cf. top-level visitor accept -- cf. AssrtSTypeGatherer?
	List<AssrtAnnotDataName> collectAnnotDataVarDecls(
			Map<AssrtVar, DataName> env);  // Currently only the vars are needed (not the data types)


	// More like "subs"
	AssrtGType unfold(AssrtGTypeFactory gf, RecVar rv, AssrtGType body);

	// Gatherer visits whole type; no "gathered so far" state -- so cannot "prune", e.g., "quit" if already seen p,q
	// Returns _newly collected_ actions (i.e., return doesn't contain env by default)
	// Post: collected actions syntactically occur in all cases -- but no guarantees fire-able or safe (e.g., gammas may end up different)
	Map<Role, Set<AssrtSSend>> collectImmediateActions(
			AssrtSModelFactory mf, Map<Role, Set<AssrtSSend>> env);

	Optional<AssrtGConfig> step(AssrtGTypeFactory gf, AssrtGEnv gamma, AssrtSSend action);
}

