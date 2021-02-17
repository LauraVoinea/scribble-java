/*
 * Copyright 2008 The Scribble Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scribble.core.type.session.base;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.session.Recursion;
import org.scribble.core.type.session.Seq;

public abstract class RecursionBase<K extends ProtoKind, B extends Seq<K, B>>
        extends STypeBase<K, B> implements Recursion<K, B> {

    protected final RecVar recvar;
    protected final B body;

    // Due to inlining, source may be protocoldecl -> rec
    public RecursionBase(CommonTree source, RecVar recvar, B body) {
        super(source);
        this.recvar = recvar;
        this.body = body;
    }

    @Override
    public RecVar getRecVar() {
        return this.recvar;
    }

    @Override
    public B getBody() {
        return this.body;
    }

    @Override
    public String toString() {
        return "rec " + this.recvar + " {\n" + this.body + "\n}";
    }

    @Override
    public int hashCode() {
        int hash = 1487;
        hash = 31 * hash + super.hashCode();
        hash = 31 * hash + this.recvar.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof RecursionBase)) {
            return false;
        }
        RecursionBase<?, ?> them = (RecursionBase<?, ?>) o;
        return super.equals(o)  // Does canEquals
                && this.recvar.equals(them.recvar)
                && this.body.equals(them.body);
    }
}











	/*
	@Override
	public SType<K, B> visitWith(STypeVisitor<K, B> v) throws ScribException
	{
		return v.visitRecursion(this);
	}

	@Override
	public SType<K, B> visitWithNoEx(STypeVisitorNoEx<K, B> v)
	{
		return v.visitRecursion(this);
	}


	@Override
	public Set<Role> getRoles()
	{
		return this.body.getRoles();
	}

	@Override
	public Set<MessageId<?>> getMessageIds()
	{
		return this.body.getMessageIds();
	}

	@Override
	public Set<RecVar> getRecVars()
	{
		return this.body.getRecVars();
	}


	@Override
	public SType<K, B> unfoldAllOnce(STypeUnfolder<K> u)
	{
		if (!u.hasRec(this.recvar))  // N.B. doesn't work if recvars shadowed
		{
			u.pushRec(this.recvar, this.body);
			SType<K, B> unf = this.body.unfoldAllOnce(u);
			u.popRec(this.recvar);
					// Needed for, e.g., repeat do's in separate choice cases -- cf. stack.pop in GDo::getInlined, must pop sig there for Seqs
			return unf;
		}
		return this;
	}

	@Override
	public List<ProtocolName<K>> getProtoDependencies()
	{
		return this.body.getProtoDependencies();
	}

	@Override
	public List<MemberName<?>> getNonProtoDependencies()
	{
		return this.body.getNonProtoDependencies();
	}

	@Override
	public Recursion<K, B> substitute(Substitutions subs)
	{
		return reconstruct(getSource(), this.recvar, this.body.substitute(subs));
	}

	@Override
	public SType<K, B> pruneRecs()
	{
		// Assumes no shadowing (e.g., use after SType#getInlined recvar disamb)
		Set<RecVar> rvs = this.body.gather(new RecVarCollector<K, B>()::visit)
				.collect(Collectors.toSet());
		return rvs.contains(this.recvar)
				? this
				: this.body;  // i.e., return a Seq, to be "inlined" by Seq.pruneRecs -- N.B. must handle empty Seq case
	}
	*/
