//$ java -cp scribble-parser/lib/antlr-3.5.2-complete.jar org.antlr.Tool -o scribble-ea/target/generated-sources/antlr3 scribble-ea/src/main/antlr3/org/scribble/parser/antlr/EACalculus.g; mv scribble-ea/target/generated-sources/antlr3/scribble-ea/src/main/antlr3/org/scribble/parser/antlr/EACalculus*.java scribble-ea/target/generated-sources/antlr3/org/scribble/ext/ea/parser/antlr



grammar EACalculus;


options
{
	language = Java;
	output = AST;
}


tokens
{
    HANDLER_KW_A = 'Handler';
    LET_KW = 'let';
    IN_KW = 'in';
    RETURN_KW = 'return';
    SUSPEND_KW = 'suspend';
    IF_KW = 'if';
    THEN_KW = 'then';
    ELSE_KW = 'else';
    SPAWN_KW = 'spawn';
    REGISTER_KW = 'register';  // NewAP "integrated" into EAAsyncSystem (cf. parallel comp, session restr.) -- ...cannot unbounded-ly generate fresh APs

    HANDLER_KW = 'handler';

    TRUE_KW = 'true';
    FALSE_KW = 'false';

    UNIT_KW = '1';  // N.B. Int(1) clash
    INT_KW = 'Int';
    BOOL_KW = 'Bool';
    //String_KW = 'String';
    AP_KW = 'AP';

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

   // Higher is higher prec
   M_PLUS;
   M_COMP;

   V_UNIT;
   V_HANDLERS;
   V_REC;
   V_VAR;
   V_INT;
   V_TRUE;
   V_FALSE;

   HANDLER;  // H ... not a V or M

   A_HANDLER;
   A_UNIT;
   A_FUN;
   A_INT;
   A_BOOL;
   A_AP;

   // Separate from KW, otherwise node label will be the keyword itself (e.g., "return")
   M_LET;
   M_RETURN;
   M_SEND;
   M_SUSPEND;
   M_APP;
   M_IF;
   M_REGISTER;
   M_SPAWN;

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
	(LETTER | UNDERSCORE)(LETTER | DIGIT0 | UNDERSCORE)*
      /* Underscore currently can cause ambiguities in the API generation naming
       * scheme But maybe only consecutive underscores are the problem -- cannot
       * completely disallow underscores as needed for projection naming scheme
       * Or disallow underscores only for role/op/messagesig names
       */
;

INT:
    DIGIT DIGIT0* | '0'
;

/*
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
 */

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
	'1'..'9'
;

fragment DIGIT0:
	'0' | DIGIT
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
/*    nVarith (options{greedy=true;}:  //https://stackoverflow.com/questions/7954142/antlr-decision-can-match-input-using-multiple-alternatives
    '<' nVarith)*
->
    ^(V_COMP nVarith+)
;

nVarith:
    nVprimary (options{greedy=true;}:
    '+' nVprimary)*
->
    ^(V_PLUS nVprimary+)
;

nVprimary:
*/
    '()'
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
    vInt
|
    TRUE_KW
->
    ^(V_TRUE)
|
    FALSE_KW
->
    ^(V_FALSE)
|
    var
;

vInt:
    UNIT_KW  // !!! clash
->
    ^(V_INT UNIT_KW)  // XXX FIXME
|
    INT
->
    ^(V_INT INT)
;

var:
    ID
->
    ^(V_VAR ID)
;

handler:
    '{' session_type '}' svar=var ':' stype=type ',' op '(' v=var ':' t=type ')' '|->' nM
->
    ^(HANDLER op $v $t session_type nM $svar $stype)
;

/* A */

type:
    HANDLER_KW_A '(' type ',' in_session_type ')'
->
    ^(A_HANDLER type in_session_type)
|
    UNIT_KW
->
    ^(A_UNIT)
|
    '{' session_type '}' type '->' type  '{' session_type '}'
->
    ^(A_FUN type session_type session_type type)
|
    INT_KW
->
    ^(A_INT)
|
    BOOL_KW
->
    ^(A_BOOL)
|
    AP_KW '(' role '->' session_type (',' role '->' session_type)+ ')'
->
   ^(A_AP role+ session_type+)
;

/* M */

nM:
    '(' nM ')'
->
    nM
|
    '<' nV nV
->
    ^(M_COMP nV nV)
|
    '+' nV nV
->
    ^(M_PLUS nV nV)
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
    SUSPEND_KW nV nV
->
    ^(M_SUSPEND nV nV)
|
    '[' nV nV ']'
->
    ^(M_APP nV nV)
|
    IF_KW nV THEN_KW nM ELSE_KW nM
->
    ^(M_IF nV nM nM)
|
    REGISTER_KW nV role nM
->
    ^(M_REGISTER nV role nM)
|
    SPAWN_KW nM
->
    ^(M_SPAWN nM)
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
