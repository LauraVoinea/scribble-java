 //$ java -cp scribble-parser/lib/antlr-3.5.2-complete.jar org.antlr.Tool -o scribble-assertions/target/generated-sources/antlr3 scribble-assertions/src/main/antlr3/org/scribble/parser/antlr/Assertions.g

// Windows:
//$ java -cp scribble-parser/lib/antlr-3.5.2-complete.jar org.antlr.Tool -o scribble-assertions/target/generated-sources/antlr3/org/scribble/parser/antlr scribble-assertions/src/main/antlr3/org/scribble/parser/antlr/Assertions.g
//$ mv scribble-assertions/target/generated-sources/antlr3/org/scribble/parser/antlr/Assertions.tokens scribble-assertions/target/generated-sources/antlr3/


grammar Assertions;  // TODO: rename AssrtExt(Id), or AssrtAnnotation


options
{
	language = Java;
	output = AST;
	ASTLabelType = CommonTree;
	//ASTLabelType = ScribNodeBase;
}

tokens
{
	/*
	 * Parser input constants (lexer output; keywords, Section 2.4)
	 */
	TRUE_KW = 'True';
	FALSE_KW = 'False';


	/*
	 * Parser output "node types" (corresponding to the various syntactic
	 * categories) i.e. the labels used to distinguish resulting AST nodes.
	 * The value of these token variables doesn't matter, only the token
	 * (i.e. variable) names themselves are used (for AST node root text
	 * field).
	 * 
	 * These token names are cased on by AssrtAntlrToFormulaParser.
	 */
	
	// TODO: rename EXT_... (or ANNOT_...)
	ROOT; 
	
	BOOLEXPR; 
	COMPEXPR; 
	ARITHEXPR; 
	NEGEXPR;

	VAR;  // TODO: rename Ambig
	INTVAL; 
	NEGINTVAL; 
	STRVAL; 

	TRUE;
	FALSE;
	
	ASSRT_HEADERANNOT;
	ASSRT_STATEVARDECL_LIST;
	ASSRT_STATEVARDECL;
	ASSRT_STATEVARARG_LIST;

	ASSRT_LOCATEDSTATEVARARGS_TEMP;
}


@parser::header
{
	package org.scribble.parser.antlr;
	
	import org.antlr.runtime.Token;
	import org.antlr.runtime.tree.CommonTree;

  import org.scribble.ast.ScribNodeBase;
  import org.scribble.ast.name.simple.AmbigNameNode;
  import org.scribble.ast.name.simple.RoleNode;

  import org.scribble.ext.assrt.ast.AssrtAExprNode;
  import org.scribble.ext.assrt.ast.AssrtBExprNode;
	import org.scribble.ext.assrt.ast.AssrtStateVarHeaderAnnot;
	import org.scribble.ext.assrt.ast.AssrtStateVarArgList;
	import org.scribble.ext.assrt.ast.AssrtStateVarDecl;
	import org.scribble.ext.assrt.ast.AssrtStateVarDeclList;
  import org.scribble.ext.assrt.ast.name.simple.AssrtIntVarNameNode;
	import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
	import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
	import org.scribble.ext.assrt.core.type.formula.AssrtSmtFormula;
  import org.scribble.ext.assrt.parser.assertions.AssertionsTreeAdaptor;
	import org.scribble.ext.assrt.parser.assertions.AssrtAntlrToFormulaParser;
}

@lexer::header
{
	package org.scribble.parser.antlr;
}

@parser::members
{
	@Override    
	public void displayRecognitionError(String[] tokenNames, 
			RecognitionException e)
	{
		super.displayRecognitionError(tokenNames, e);
  	System.exit(1);
	}
  
	// source is an EXTID.text, including the quotes
	public static AssrtBFormula parseAssertion(String source) 
			throws RecognitionException
	{
		source = source.substring(1, source.length()-1);  // Remove enclosing quotes -- cf. AssrtScribble.g EXTID
		AssertionsLexer lexer = new AssertionsLexer(new ANTLRStringStream(source));
		AssertionsParser parser = new AssertionsParser(
				new CommonTokenStream(lexer));
		AssrtSmtFormula<?> res = AssrtAntlrToFormulaParser
				.getInstance().parse((CommonTree) parser.bool_root().getTree());
		if (!(res instanceof AssrtBFormula))
		{
			System.out.println("Invalid assertion syntax: " + source);
			System.exit(1);
		}
		return (AssrtBFormula) res;
	}

	// source is an EXTID.text, including the quotes
	public static AssrtAFormula parseArithAnnotation(String source) 
			throws RecognitionException
	{
		source = source.substring(1, source.length()-1);  // Remove enclosing quotes -- cf. AssrtScribble.g EXTID
		AssertionsLexer lexer = new AssertionsLexer(new ANTLRStringStream(source));
		AssertionsParser parser = new AssertionsParser(new CommonTokenStream(lexer));
		//return (CommonTree) parser.arith_expr().getTree();
		AssrtAFormula res = (AssrtAFormula) AssrtAntlrToFormulaParser
				.getInstance().parse((CommonTree) parser.arith_root().getTree());
		return res;
	}
  
	// t is an EXTID token
	public static AssrtStateVarHeaderAnnot parseStateVarHeader(Token t) 
			throws RecognitionException
	{
		String source = t.getText();
		source = source.substring(1, source.length()-1);  // Remove enclosing quotes -- cf. AssrtScribble.g EXTID
		AssertionsLexer lexer = new AssertionsLexer(new ANTLRStringStream(source));
		AssertionsParser parser = new AssertionsParser(
				new CommonTokenStream(lexer));
		parser.setTreeAdaptor(new AssertionsTreeAdaptor());
		AssrtStateVarHeaderAnnot tmp = (AssrtStateVarHeaderAnnot) 
				parser.assrt_headerannot().getTree();
		
		int count = tmp.getChildCount();
		if (count > 1) {
			AssrtStateVarDeclList first = (AssrtStateVarDeclList) tmp.getChild(0);
			CommonTree last = (CommonTree) tmp.getChild(count - 1);
			boolean hasExpr = !(last instanceof AssrtStateVarDeclList);  // assertion still just a CommonTree (converted notAssrtBExprNode below)
			for (int i = 1; i < (hasExpr ? count - 1 : count); i++) {
				AssrtStateVarDeclList next = (AssrtStateVarDeclList) tmp.getChild(1);  // N.B. 1, not i
				next.getDeclChildren().forEach(c -> first.addChild(c));
				tmp.deleteChild(1);
			}
		}
		
		AssrtStateVarDeclList svars = (AssrtStateVarDeclList)
				tmp.getChild(AssrtStateVarHeaderAnnot.ASSRT_STATEVARDECLLIST_CHILD_INDEX);
		for (int i = 0; i < svars.getChildCount(); i++)
		{
			AssrtStateVarDecl sdecl = (AssrtStateVarDecl) svars.getChild(i);
			CommonTree arith_expr = (CommonTree) 
					sdecl.getChild(AssrtStateVarDecl.ASSRT_STATEVAREXPR_CHILD_INDEX);
			AssrtAExprNode arg = new AssrtAExprNode(t.getType(), t, (AssrtAFormula)
					AssrtAntlrToFormulaParser.getInstance().parse(arith_expr));
			sdecl.setChild(AssrtStateVarDecl.ASSRT_STATEVAREXPR_CHILD_INDEX, arg);
		}
		if (tmp.getChildCount() > 1)
		{
			CommonTree bool_expr = (CommonTree) 
					tmp.getChild(AssrtStateVarHeaderAnnot.ASSRT_ASSERTION_CHILD_INDEX);
			AssrtBExprNode ass = new AssrtBExprNode(t.getType(), t, (AssrtBFormula)
					AssrtAntlrToFormulaParser.getInstance().parse(bool_expr));
			tmp.setChild(AssrtStateVarHeaderAnnot.ASSRT_ASSERTION_CHILD_INDEX, ass);
		}
		return tmp;
	}

	// t is an EXTID token
	public static AssrtStateVarArgList parseStateVarArgList(Token t) 
			throws RecognitionException
	{
		String source = t.getText();
		source = source.substring(1, source.length()-1);  // Remove enclosing quotes -- cf. AssrtScribble.g EXTID
		AssertionsLexer lexer = new AssertionsLexer(new ANTLRStringStream(source));
		AssertionsParser parser = new AssertionsParser(new CommonTokenStream(lexer));
		parser.setTreeAdaptor(new AssertionsTreeAdaptor());
		
		CommonTree bar = (CommonTree) parser.assrt_statevarargs().getTree();
		if (bar.getText().equals("ASSRT_LOCATEDSTATEVARARGS_TEMP")) { // Located, and potentially multiple  // TODO: record rolename
			AssrtStateVarArgList first = (AssrtStateVarArgList) bar.getChild(0);
			for (int i = 1; i < bar.getChildCount(); i++) {
				//((CommonTree) bar.getChild(1)).getChildren().forEach(x -> first.addChild((AssrtAExprNode) x));  // N.B. 1, not i // getChildren broken by ScribNode override
				AssrtStateVarArgList next = (AssrtStateVarArgList) bar.getChild(1);
				for (int j = 0; j < next.getChildCount(); j++) {
					first.addChild(next.getChild(j));  // N.B. 1, not i
				}
				bar.deleteChild(1);
			}
			bar = first;
		}
		
		AssrtStateVarArgList tmp = (AssrtStateVarArgList) 
				//parser.assrt_statevarargs().getTree();
				bar;
		for (int i = 0; i < tmp.getChildCount(); i++)
		{
			CommonTree arith_expr = (CommonTree) tmp.getChild(i);
			AssrtAExprNode arg = new AssrtAExprNode(t.getType(), t, (AssrtAFormula)
					AssrtAntlrToFormulaParser.getInstance().parse(arith_expr));
			tmp.setChild(i, arg);
		}
		return tmp;
	}

	public static AssrtStateVarDeclList foo(CommonTree r,  CommonTree s) 
			throws RecognitionException
	{
		RoleNode rn = (RoleNode) r;
		AssrtStateVarDeclList svars = (AssrtStateVarDeclList) s;
		for (AssrtStateVarDecl svar : svars.getDeclChildren()) {
			svar.addChild(rn);  // AssrtStateVarDecl.ASSRT_ROLE_CHILD_INDEX
		}
		return svars;
	}

	public static CommonTree parseStringLit(Token t) 
	{
		String text = t.getText();
		t.setText(text.substring(1, text.length()-1)); 
		return new CommonTree(t);
	}
}


// Not referred to explicitly, deals with whitespace implicitly (don't delete this)
WHITESPACE:
	('\t' | ' ' | '\r' | '\n'| '\u000C')+
	{
		$channel = HIDDEN;
	}
;


/* Duplicated from AssrtScribble.g */

ID:
	LETTER (LETTER | DIGIT)*
;  

NUMBER: 
	(DIGIT)+
; 

STRING_LIT:
	'\'' (LETTER | DIGIT | WHITESPACE)* '\''
|
	'\"' (LETTER | DIGIT | WHITESPACE)* '\"' 
;

fragment LETTER:
	'a'..'z' | 'A'..'Z'
;

fragment DIGIT:
	'0'..'9'
;

ambigname: t=ID -> ID<AmbigNameNode>[$t] ;
rolename: t=ID -> ID<RoleNode>[$t] ;


/* Assrt */

assrt_varname: t=ID -> ID<AssrtIntVarNameNode>[$t] ;  // Currently, int or string

variable: 
	ID -> ^(VAR ID)
; 	  

intlit: 
	NUMBER -> ^(INTVAL NUMBER)	   
|
	'-' NUMBER -> ^(NEGINTVAL NUMBER)
; 
	
stringlit:
	t=STRING_LIT -> ^(STRVAL {AssertionsParser.parseStringLit($t)})
;


root:  // Seems mandatory?  For top-level Tree?
	bool_root -> bool_root
;

bool_root:  // EOF useful?
	bool_expr EOF -> bool_expr
;

arith_root:  // EOF useful?
	arith_expr EOF -> arith_expr
;


expr:
	bool_expr
;
	
bool_expr:
	bool_or_expr
;

bool_or_expr:
	bool_and_expr (op=('||') bool_and_expr)*
->
	^(BOOLEXPR bool_and_expr ($op bool_and_expr)*)  // ops a bit redundant, but currently using old, shared (and/or) AssrtAntlrBoolExpr parsing routine
;
// ANTLR seems to engender a pattern where expr "kinds" are nested under a single expr
// Cf. https://github.com/antlr/grammars-v3/blob/master/Java1.6/Java.g#L943
// ^Expr categories are all "nested", bottoming out at primary which recursively contains `parExpression`
// Precedence follows the nesting order, e.g., 1+2*3 -> 1+(2*3); o/w left-assoc (preserved by AssrtAntlr... routines)

bool_and_expr:
	comp_expr (op=('&&') comp_expr)*
->
	^(BOOLEXPR comp_expr ($op comp_expr)*)
;

comp_expr:  // "relational" expr
	arith_expr (op=('=' | '<' | '<=' | '>' | '>=') arith_expr)?
->
	^(COMPEXPR arith_expr $op? arith_expr?)
;
	
arith_expr:
	arith_add_expr
;

arith_add_expr:
	arith_mul_expr (arith_addsub_op arith_mul_expr)*
->
	^(ARITHEXPR arith_mul_expr (arith_addsub_op arith_mul_expr)*)  // Cannot distinguish the ops args?  Always the last one?
;

arith_addsub_op:
	'+' | '-'
;

arith_mul_expr:
	arith_unary_expr (op=('*') arith_unary_expr)*
->
	^(ARITHEXPR arith_unary_expr ($op arith_unary_expr)*)
;
	
arith_unary_expr:
	primary_expr
|
	'!' bool_expr -> ^(NEGEXPR bool_expr)  // Highly binding, so nest deeply
;
// '¬' doesn't seem to work

primary_expr:
	paren_expr
|
	literal
|
	variable
/*|
	unint_fun*/
;
	
paren_expr:
	'(' expr ')' -> expr
;

literal:
	TRUE_KW -> ^(TRUE)
|
	FALSE_KW -> ^(FALSE)
|
	intlit
|
	stringlit
;
	
// bool_expr parsed to AssrtBExprNode by parseStateVarHeader
assrt_headerannot:
	bool_expr
->
	^(ASSRT_HEADERANNOT ^(ASSRT_STATEVARDECL_LIST) bool_expr)
|
	assrt_statevardecls bool_expr?
->
	^(ASSRT_HEADERANNOT assrt_statevardecls bool_expr?)
|
	assrt_locatedstatevardecls_temp+ bool_expr?
->
	^(ASSRT_HEADERANNOT assrt_locatedstatevardecls_temp+ bool_expr?)
;

// An intermediary category, resolved by foo
assrt_locatedstatevardecls_temp:
	rolename assrt_statevardecls
->
	{foo($rolename.tree, $assrt_statevardecls.tree)}
;

assrt_statevardecls:
	'[' assrt_statevardecl (',' assrt_statevardecl)* ']'
->
	^(ASSRT_STATEVARDECL_LIST assrt_statevardecl+)
;

// arith_expr parsed to AssrtAExprNode by parseStateVarHeader
assrt_statevardecl:  // cf. payelem
	assrt_varname ':' ambigname '=' arith_expr
->
	^(ASSRT_STATEVARDECL assrt_varname ambigname arith_expr)  // N.B. rolename to be added by parseStateVarArgList
/*|
	assrt_varname ':' qualifieddataname '=' arith_expr  // TODO: qualifieddataname
->
	^(ASSRT_STATEVARDECL assrt_varname qualifieddataname arith_expr)*/
;

// An intermediary category, resolved by bar
assrt_statevarargs:
	assrt_nonlocatedstatevarargs
-> 
	assrt_nonlocatedstatevarargs
|
	(rolename assrt_nonlocatedstatevarargs)+
->
	^(ASSRT_LOCATEDSTATEVARARGS_TEMP assrt_nonlocatedstatevarargs+)
;

assrt_nonlocatedstatevarargs:
	'[' assrt_statevararg (',' assrt_statevararg)* ']'
->
	^(ASSRT_STATEVARARG_LIST assrt_statevararg+)
;

assrt_statevararg:
	arith_expr  // Parsed to AssrtAExprNode by parseStateVarArgList
;
	

	
	
	
	
	
	
	
	
	
	

	
	
	
	
	
	
	
	
/*
unint_fun:
	ID unint_fun_arg_list
->
	^(UNFUN ID unint_fun_arg_list)
; 
	
unint_fun_arg_list:
	'(' (arith_expr (',' arith_expr )*)? ')'
->
	^(UNFUNARGLIST arith_expr*)
;
*/
