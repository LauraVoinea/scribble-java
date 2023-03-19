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
    HANDLER_KW_A = 'Handler';
    LET_KW = 'let';
    IN_KW = 'in';
    RETURN_KW = 'return';
    SUSPEND_KW = 'suspend';
    MU_KW = 'mu';
    REC_KW = 'rec';

    END_KW = 'end';

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
   V_REC;
   V_VAR;

   HANDLER;  // H ... not a V or M

   A_HANDLER;
   A_UNIT;
   A_FUN;

   // Separate from KW, otherwise node label will be the keyword itself (e.g., "return")
   M_LET;
   M_RETURN;
   M_SEND;
   M_SUSPEND;
   M_APP;

   S_SELECT;
   S_BRANCH;
   S_REC;
   S_RECVAR;
   S_END;
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
	(LETTER | DIGIT | UNDERSCORE)+
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

/*// Comes after SYMBOL due to an ANTLR syntax highlighting issue involving quotes.
// CHECKME: parser doesn't work without locating the quotes here? (e.g., if inlined into parser rules)
EXTID:
	'\"' (LETTER | DIGIT | SYMBOL_SINGLE | WHITESPACE)* '\"'  // N.B. WHITESPACE, for assertions white space
|
	'\'' (LETTER | DIGIT | SYMBOL_DOUBLE | WHITESPACE)* '\''  // N.B. WHITESPACE, for assertions white space
;*/

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

// Simply "names", not actual categories
role: ID;
op: ID;
//var: ID;
//type: ID;
recvar: ID;
fname: ID;  // FIXME only used in rec (not in general expr -- parsed as var...)

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

/* ... */

start:
    nM EOF
;

/* V */

// Parser rule non-terms must be lower case
nV:
    '(' ')'
->
    ^(V_UNIT)
|
    '(' nV ')'
->
    nV
|
    HANDLER_KW role '{' handler (',' handler)* '}'
->
    ^(V_HANDLERS role handler+)
|
    REC_KW fname '{' session_type '}' '(' var ':' type ')' ':'  type '{' session_type'}' '.' nM
->
    ^(V_REC fname var type session_type session_type type nM)
|
    var
;

var:
    ID
->
    ^(V_VAR ID)
;

handler:
    '{' session_type '}' op '(' var ':' type ')' '|->' nM
->
    ^(HANDLER op var type session_type nM)
;

/* A */

type:
    HANDLER_KW_A '(' in_session_type ')'
->
    ^(A_HANDLER in_session_type)
|
    '1'
->
    ^(A_UNIT)
|
    '{' session_type '}' type '->' type  '{' session_type '}'
->
    ^(A_FUN type session_type session_type type)
;

/* M */

nM:
    '(' nM ')'
->
    nM
|
    LET_KW var ':' type '<=' nM IN_KW nM
->
    ^(M_LET var type nM nM)
|
    role '!' op '(' nV ')'
->
    ^(M_SEND role op nV)
|
    RETURN_KW nV
->
    ^(M_RETURN nV)  // Initial tree rewriting, no longer pure CST -- "node label tokens" needed or else "node value" is null (apart from children)
|
    SUSPEND_KW nV
->
    ^(M_SUSPEND nV)
|
    '[' nV nV ']'
->
    ^(M_APP nV nV)
;

/* S */

session_type:
    role '!' '{' session_type_case (',' session_type_case)* '}'
->
    ^(S_SELECT role session_type_case+)
|
    in_session_type
|
    MU_KW recvar '.' session_type
->
    ^(S_REC recvar session_type)
|
    END_KW
->
    ^(S_END)
|
    ID
->
    ^(S_RECVAR ID)
;

in_session_type:
    role '?' '{' session_type_case (',' session_type_case)* '}'
->
    ^(S_BRANCH role session_type_case+)
;

session_type_case:
   op '(' type ')' '.' session_type
;
