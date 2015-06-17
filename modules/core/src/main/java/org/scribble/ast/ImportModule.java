package org.scribble.ast;

import org.scribble.ast.name.qualified.ModuleNameNode;
import org.scribble.ast.visit.ModelVisitor;
import org.scribble.del.ScribDel;
import org.scribble.sesstype.name.ModuleName;
import org.scribble.util.ScribbleException;

public class ImportModule extends ImportDecl
{
	public final ModuleNameNode modname;
	//public final SimpleProtocolNameNode alias;
	public ModuleNameNode alias;

	// FIXME: make a no alias constructor
	//public ImportModule(ModuleNameNode modname, SimpleProtocolNameNode alias)
	public ImportModule(ModuleNameNode modname, ModuleNameNode alias)
	{
		this.modname = modname;
		this.alias = alias;
	}

	@Override
	protected ImportModule copy()
	{
		return new ImportModule(this.modname, this.alias);
	}
	
	//protected ImportModule reconstruct(ModuleNameNode modname, SimpleProtocolNameNode alias)
	protected ImportModule reconstruct(ModuleNameNode modname, ModuleNameNode alias)
	{
		ScribDel del = del();
		ImportModule im = new ImportModule(modname, alias);
		im = (ImportModule) im.del(del);
		return im;
	}

	@Override
	public ImportModule visitChildren(ModelVisitor nv) throws ScribbleException
	{
		ModuleNameNode modname = (ModuleNameNode) visitChild(this.modname, nv);
		//SimpleProtocolNameNode alias = null;
		ModuleNameNode alias = null;
		if (isAliased())
		{
			//alias = (SimpleProtocolNameNode) visitChild(this.alias, nv);
			alias = visitChildWithClassCheck(this, this.alias, nv);
			//return new ImportModule(this.ct, modname, alias);
		}
		//return new ImportModule(this.ct, modname, null);
		return reconstruct(modname, alias);
	}
	
	@Override
	public boolean isAliased()
	{
		return this.alias != null;
	}
	
	/*@Override
	public ModuleName getVisibleName()
	{
		return isAliased() ? getAlias() : this.modname.toName();
	}*/
	
	//@Override
	public ModuleName getModuleNameAlias()
	{
		//return new ModuleName(this.alias.identifier);
		return this.alias.toName();
	}
	
	@Override
	public String toString()
	{
		String s = Constants.IMPORT_KW + " " + modname;
		if (isAliased())
		{
			s += " " + Constants.AS_KW + " " + this.alias;
		}
		return s + ";";
	}
	
	@Override
	public boolean isImportModule()
	{
		return true;
	}
}
