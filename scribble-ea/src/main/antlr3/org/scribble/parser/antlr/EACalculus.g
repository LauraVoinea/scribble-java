//$ java -cp scribble-parser/lib/antlr-3.5.2-complete.jar org.antlr.Tool -o scribble-ea/target/generated-sources/antlr3 scribble-ea/src/main/antlr3/org/scribble/parser/antlr/EACalculus.g; mv scribble-ea/target/generated-sources/antlr3/scribble-ea/src/main/antlr3/org/scribble/parser/antlr/EACalculus*.java scribble-ea/target/generated-sources/antlr3/org/scribble/ext/ea/parser/antlr



grammar EACalculus;


options
{
	language = Java;
	output = AST;
}


tokens
{
    HANDLER_KW = 'handler';
    RETURN_KW = 'return';

  /* Scribble AST token types (corresponding to the Scribble BNF).
   * These token types are used by ScribTreeAdaptor to create the output nodes
   * using the org.scribble.ast classes.
   * (Trying to construct those classes directly from here doesn't seem to work
   * well for most cases.)
   * These tokens are ANTLR "imaginary tokens": they are derived by the ANTLR
   * "rewrite rules" on the actual source tokens.
   * The specific value of these tokens aren't important (the constants are
   * accessed via fields of ScribbleParser).
   * As a naming convention, we use a few "_" suffixes: _KW, _NAME, _LIT and
   * _LIST.
   */

   V_UNIT;
   V_HANDLERS;

   HANDLER;

   M_SEND;  // Separate from KW, otherwise node label will be the keyword itself (e.g., "return")
   M_RETURN;

}


@lexer::header
{
  package org.scribble.ext.ea.parser.antlr;
}


/*
@lexer::members
{
  @Override
  public void displayRecognitionError(String[] tokenNames,
  		RecognitionException e)
  {
    super.displayRecognitionError(tokenNames, e);
    System.exit(1);
  }
}
//*/


// Must come after tokens?
@parser::header
{
  package org.scribble.ext.ea.parser.antlr;
}

/*
  import org.scribble.ast.NonRoleArg;
  import org.scribble.ast.ScribNodeBase;
  import org.scribble.ast.UnaryPayElem;
  import org.scribble.ast.name.qualified.DataNameNode;
  import org.scribble.ast.name.simple.AmbigNameNode;
  import org.scribble.ast.name.simple.DataParamNode;
  import org.scribble.ast.name.simple.IdNode;
  import org.scribble.ast.name.simple.OpNode;
  import org.scribble.ast.name.simple.RecVarNode;
  import org.scribble.ast.name.simple.RoleNode;
  import org.scribble.ast.name.simple.SigParamNode;

  import org.scribble.ext.assrt.ast.AssrtAExprNode;
  import org.scribble.ext.assrt.ast.AssrtBExprNode;
  import org.scribble.ext.assrt.ast.name.simple.AssrtVarNameNode;
  import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
}


@parser::members
{
  // Abort tool run on parsing errors (and display user-friendly message) -- obsoletes CommonErrorNode check?
  @Override
  public void displayRecognitionError(String[] tokenNames,
  		RecognitionException e)
  {
    super.displayRecognitionError(tokenNames, e);
    System.exit(1);
  }

	// Currently unused -- TODO: check later in intermed translation, instead of parsing
  public static CommonTree checkId(CommonTree id)
  {
  	if (id.getText().contains("__"))
  	{
			System.err.println("Double underscores are reserved: " + id);
			System.exit(1);
  	}
  	return id;
  }
}
//*/


/****************************************************************************
 * Chapter 2 Lexical Structure (Lexer rules)
 ***************************************************************************/

/* *  // Double star here not accepted by ANTLR...
 * Section 2.1 White space (Section 2.1)
 */
// Not referred to explicitly, deals with whitespace implicitly (don't delete this)
WHITESPACE:
	('\t' | ' ' | '\r' | '\n'| '\u000C')+
	{
		$channel = HIDDEN;
	}
;


/**
 * Section 2.2 Comments
 */
COMMENT:
	'/*' .* '*/'
	{
		$channel=HIDDEN;
	}
;

LINE_COMMENT:
	'//' ~('\n'|'\r')* '\r'? '\n'
  {
		$channel=HIDDEN;
	}
;


/**
 * Section 2.3 Identifiers
 */
ID:
	(LETTER | DIGIT | UNDERSCORE)*
      /* Underscore currently can cause ambiguities in the API generation naming
       * scheme But maybe only consecutive underscores are the problem -- cannot
       * completely disallow underscores as needed for projection naming scheme
       * Or disallow underscores only for role/op/messagesig names
       */
;

fragment SYMBOL:
	'{' | '}' | '(' | ')' | '[' | ']' | ':' | '/' | '\\' | '.' | '\#'
|
	'&' | '?' | '!'  | UNDERSCORE
|
	'|' | '¬' | ',' | '=' | '<' | '>' | '+' | '-' | '*' // Assrt
;

fragment SYMBOL_SINGLE:
 SYMBOL | '\''
;

fragment SYMBOL_DOUBLE:
 SYMBOL | '\"'
 ;

// Comes after SYMBOL due to an ANTLR syntax highlighting issue involving quotes.
// CHECKME: parser doesn't work without locating the quotes here? (e.g., if inlined into parser rules)
EXTID:
	'\"' (LETTER | DIGIT | SYMBOL_SINGLE | WHITESPACE)* '\"'  // N.B. WHITESPACE, for assertions white space
|
	'\'' (LETTER | DIGIT | SYMBOL_DOUBLE | WHITESPACE)* '\''  // N.B. WHITESPACE, for assertions white space
;

fragment LETTER:
	'a'..'z' | 'A'..'Z'
;

fragment DIGIT:
	'0'..'9'
;

fragment UNDERSCORE:
	'_'
;


/****************************************************************************
 * Chapter 3 Syntax (Parser rules)
 ***************************************************************************/

/*
module:
  // Assrt
	t=MODULE_KW modulename ';' importmodule* nonprotodecl* //assert_fundecl*
	protodecl* EOF
->
	^(ASSRT_MODULE ^(MODULEDECL modulename) importmodule* nonprotodecl*
			//assrt_fundecl*
			protodecl*)
;
*/

start:
    nM EOF
;

role: ID;
op: ID;
var: ID;

// Parser rule non-terms must be lower case
nV:
    '(' ')'
->
    ^(V_UNIT)
|
    HANDLER_KW ID '{' handler+ '}'
->
    ^(V_HANDLERS ID handler+)
|
    ID
;

handler:
    op '(' var ')' '-' '>' nM
->
    ^(HANDLER op var nM)
;

nM:
    '(' nM ')'
->
    nM
|
    role '!' op '(' nV ')'
->
    ^(M_SEND ID ID nV)
|
    RETURN_KW nV
->
    ^(M_RETURN nV)  // Initial tree rewriting, no longer pure CST -- "node label tokens" needed or else "node value" is null (apart from children)
;


