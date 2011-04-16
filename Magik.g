grammar Magik;

options {
	output = AST;
	k = 1;
}

tokens {
	ARGUMENTS;
	BLOCK;
	BODY;
	BOOT_CHEVRON;
	CATCH;
	CHEVRON;
	CLONE;
	CONTINUE;
	DYNAMIC_VARIABLE_DECLARATION;
	ELIF;
	ELSE;
	FALSE;
	FINALLY;
	FOR;
	GLOBAL_VARIABLE_DECLARATION;
	HANDLING;
	IF;
	IMPORT;
	INDEXED_METHOD_CALL;
	INDEXED_METHOD_CALL;
	INDEXED_METHOD_DECLARATION;
	LEAVE;
	LOCAL_VARIABLE_DECLARATION;
	LOCK;
	LOCKING;
	LOOP;
	LOOPBODY;
	LVALUE;
	MAYBE;
	METHOD_CALL;
	METHOD_DECLARATION;
	METHOD_MESSAGE_CALL;
	METHOD_MESSAGE_INDEXER;
	METHOD_MODIFIERS;
	OVER;
	PACKAGE_SPECIFICATION;
	PARALLEL_ASSIGNMENT;
	PARAMETER_MODIFIERS;
	PARAMETER;
	PARAMETERS;
	PRAGMA_VALUE;
	PRAGMA;
	PROCEDURE_CALL;
	PROCEDURE_DECLARATION;
	PROTECT;
	PROTECTION;
	RETURN;
	RVALUE;
	SELF;
	SIGN_MINUS;
	SIGN_PLUS;
	SIMPLE_VECTOR;
	SLOT_ACCESS;
	SUPER;
	THISTHREAD;
	THROW;
	TRANSMIT;
	TRUE;
	TRY;
	UNSET;
	VARIABLE_DECLARATION;
	WHEN;
	WITH;
}

magik
	:	(package_specification |
		 pragma |
		 method_declaration |
		 statement |
		 transmit |
		 (NEWLINE!)+)+
	;
	
transmit
	:	'$' -> TRANSMIT
	;

package_specification
	:	'_package' IDENTIFIER -> ^(PACKAGE_SPECIFICATION IDENTIFIER)
	;

pragma
	:	'_pragma' '(' pragma_param (',' pragma_param)* ')' -> ^(PRAGMA pragma_param+)
	;

pragma_param
	:	IDENTIFIER '=' pragma_value -> ^(PRAGMA_VALUE IDENTIFIER pragma_value)
	;

pragma_value
	:	IDENTIFIER |
		'{' IDENTIFIER (',' IDENTIFIER)* '}' -> IDENTIFIER+
	;

method_declaration
//	:	(method_modifiers '_method' IDENTIFIER '.' IDENTIFIER '(' parameter_list ')'  '<<' IDENTIFIER |
//		 method_modifiers '_method' IDENTIFIER '.' IDENTIFIER '(' parameter_list ')' '^<<' IDENTIFIER |

//		 method_modifiers '_method' IDENTIFIER '.' IDENTIFIER                         '<<' IDENTIFIER |
//		 method_modifiers '_method' IDENTIFIER '.' IDENTIFIER                        '^<<' IDENTIFIER |

//		 method_modifiers '_method' IDENTIFIER '[' parameter_list ']'                 '<<' IDENTIFIER |
//		 method_modifiers '_method' IDENTIFIER '[' parameter_list ']'                '^<<' IDENTIFIER |

//		 method_modifiers '_method' IDENTIFIER '.' IDENTIFIER '(' parameter_list ')' |
//		 method_modifiers '_method' IDENTIFIER '.' IDENTIFIER)
//	:	method_modifiers '_method' IDENTIFIER (('.' IDENTIFIER ('(' parameter_list? ')')?) | ('[' parameter_list ']')) (('<<'|'^<<') IDENTIFIER)? NEWLINE+
	:	method_modifiers '_method' IDENTIFIER (method_declaration_dot | method_declaration_lsquare) method_declaration_assignment? NEWLINE+
			(handling NEWLINE+)*
			(statement NEWLINE+)*
		'_endmethod' -> ^(METHOD_DECLARATION method_modifiers IDENTIFIER method_declaration_dot? method_declaration_lsquare? method_declaration_assignment? ^(BODY handling* statement*))
	;
	
method_declaration_dot
	:	'.' IDENTIFIER ('(' parameter_list? ')')? -> ^(METHOD_MESSAGE_CALL IDENTIFIER parameter_list?)
	;
	
method_declaration_lsquare
	:	'[' parameter_list ']' -> ^(METHOD_MESSAGE_INDEXER parameter_list)
	;
	
method_declaration_assignment
	:	'<<' IDENTIFIER -> ^(CHEVRON IDENTIFIER) |
		'^<<' IDENTIFIER -> ^(BOOT_CHEVRON IDENTIFIER)
	;

method_modifiers
	:	('_private' |
		 '_abstract' |
		 '_iter')* -> ^(METHOD_MODIFIERS '_private'* '_abstract'* '_iter'*)
	;

parameter_list
	:	parameter (',' NEWLINE* parameter)* -> parameter+
	;

parameter
	:	parameter_modifiers NEWLINE? IDENTIFIER -> ^(PARAMETER parameter_modifiers IDENTIFIER)
	;

parameter_modifiers
	:	('_optional' |
		 '_gather')* -> ^(PARAMETER_MODIFIERS '_optional'* '_gather'*)
	;

statement
	:	(parallel_assignment |
		 local_declaration_assignment |
		 dynamic_declaration_assignment |
		 global_declaration_assignment |
		 throw_statement |
		 import_statement |
		 continue_statement |
		 leave_statement |
		 loopbody_statement |
		 return_statement |
		 expression |
		 ';'!)
	;

parallel_assignment
	// XXX: TODO: make this part of an expression? '(' expression ')' would become '(' expression (',' expression)* ')'
	//  and this would become a normal assignment
	//  this might fix the ambiguity of the grammar
	:	lvalues_parenthesized '<<' NEWLINE* (rvalues_parenthesized | rvalues) -> ^(PARALLEL_ASSIGNMENT lvalues_parenthesized rvalues_parenthesized? rvalues?)
	;

local_declaration_assignment
//	:	'_local' '_constant'?  lvalue_tuple
//		'_local' '_constant'?  lvalue_tuple '<<' expression
//		'_local' '_constant'? '(' lvalue_tuple ')'
//		'_local' '_constant'? '(' lvalue_tuple ')' '<<' expression |
//		'_local' '_constant'? '(' lvalue_tuple ')' '<<' '(' rvalue_tuple ')' |
//		'_local' '_constsant'? (variable '<<' expression)+ // XXX: TODO
	:	'_local' '_constant'? (lvalues_parenthesized | lvalues) ('<<' NEWLINE* (rvalues_parenthesized | expression))? -> ^(LOCAL_VARIABLE_DECLARATION ^(VARIABLE_DECLARATION lvalues_parenthesized? lvalues?) ^(ARGUMENTS rvalues_parenthesized? expression?)) 
	;

dynamic_declaration_assignment
	:	'_dynamic' (lvalues_parenthesized | lvalues) ('<<' NEWLINE* (rvalues_parenthesized | expression))? -> ^(DYNAMIC_VARIABLE_DECLARATION ^(VARIABLE_DECLARATION lvalues_parenthesized? lvalues?) ^(ARGUMENTS rvalues_parenthesized? expression?)) 
	;

global_declaration_assignment
	:	'_global' '_constant'? (lvalues_parenthesized | lvalues) ('<<' NEWLINE* (rvalues_parenthesized | expression))? -> ^(GLOBAL_VARIABLE_DECLARATION ^(VARIABLE_DECLARATION lvalues_parenthesized? lvalues?) ^(ARGUMENTS rvalues_parenthesized? expression?)) 
	;

throw_statement
	:	'_throw' (SYMBOL|LABEL) with? -> ^(THROW SYMBOL? LABEL? with)// XXX: TODO: 1st expression should be SYMBOL?
	;

import_statement
	:	'_import' expression (',' expression)* -> ^(IMPORT expression+)
	;

continue_statement
	:	'_continue' LABEL? with -> ^(CONTINUE LABEL? with)
	;

leave_statement
	:	'_leave' LABEL? with ->  ^(LEAVE LABEL? with)
	;
	
loopbody_statement
	:	'_loopbody' rvalues_parenthesized -> ^(LOOPBODY rvalues_parenthesized)
	;

return_statement
	:	'_return' (rvalues_parenthesized | rvalues)? -> ^(RETURN rvalues_parenthesized? rvalues?) |
		'>>' NEWLINE* (rvalues_parenthesized | rvalues) -> ^(RETURN rvalues_parenthesized? rvalues?)
	;

expression
	:	assignment_expression
	;

assignment_expression
	:	logical_orif_expression (('<<' | '^<<' | '+<<' | '-<<' | '/<<' | '*<<')^ NEWLINE* logical_orif_expression)*
	;

logical_orif_expression
	:	logical_xor_expression (('_or' | '_orif')^ NEWLINE* logical_xor_expression)*
	;

logical_xor_expression
	:	logical_andif_expression ('_xor'^ NEWLINE* logical_andif_expression)*
	;

logical_andif_expression
	:	equality_expression (('_and' | '_andif')^ NEWLINE* equality_expression)*
	;

equality_expression
	:	relational_expression (('<>' | '=' |'_is' | '_isnt' | '~=')^ NEWLINE* relational_expression)*
	;

relational_expression
	:	additive_expression (('_cf' | '<' | '<=' | '>' | '>=')^ NEWLINE* additive_expression)*
	;

additive_expression
	:	multiplicative_expression (('+' | '-')^ NEWLINE* multiplicative_expression)*
	;

multiplicative_expression
	:	exponential_expression (('*' | '/' | '_div' | '_mod')^ NEWLINE* exponential_expression)*
	;

exponential_expression
	:	unary_expression ('**'^ NEWLINE? unary_expression)*
	;

unary_expression
	:	'~' unary_expression -> ^('~' unary_expression) |
		'_not' unary_expression -> ^('_not' unary_expression) |
		'-' unary_expression -> ^(SIGN_MINUS unary_expression) |
		'+' unary_expression -> ^(SIGN_PLUS unary_expression) |
		'_allresults' unary_expression -> ^('_allresults' unary_expression) |
		postfix_expression
	;

postfix_expression
	:	//literal { test: }({ if (isNewLine()) { break test; } } procedure_call? { System.out.println("procedure call"); })
		//literal (~NEWLINE) => (procedure_call? | (method_call | indexed_method_call)*) //(procedure_call? | method_call*)
//		literal (method_call | indexed_method_call)* //(procedure_call? | method_call*)
//		literal procedure_call?
		atom 
			(
				('(') => procedure_call?
			)
			(
				('.') => method_call |
				('[') => indexed_method_call
			)*
	;

procedure_call
	:	(~NEWLINE) => '(' NEWLINE* rvalues? NEWLINE* ')' -> ^(PROCEDURE_CALL ^(ARGUMENTS rvalues))
	;

method_call
//	:	'.' IDENTIFIER '(' expression? (',' expression)* ')'  '<<' expression |
//		'.' IDENTIFIER '(' expression? (',' expression)* ')' '^<<' expression |
//		'.' IDENTIFIER                                        '<<' expression |
//		'.' IDENTIFIER                                       '^<<' expression |
//		'.' IDENTIFIER '(' expression? (',' expression)* ')' |
//		'.' IDENTIFIER
	:	(~NEWLINE) => '.' NEWLINE* IDENTIFIER ('(' NEWLINE* rvalues? NEWLINE* ')')?  method_assignment? -> ^(METHOD_CALL IDENTIFIER ^(ARGUMENTS rvalues?) method_assignment?)
	;

method_assignment
	:	'<<' IDENTIFIER -> ^(CHEVRON IDENTIFIER) |
		'^<<' IDENTIFIER -> ^(BOOT_CHEVRON IDENTIFIER)
	;
	
indexed_method_call
//	:	'[' expression (',' expression)* ']'                  '<<' expression |
//		'[' expression (',' expression)* ']'                 '^<<' expression |
//		'[' expression (',' expression)* ']'
	:	(~NEWLINE) => '[' NEWLINE* rvalues NEWLINE* ']' method_assignment? -> ^(INDEXED_METHOD_CALL ^(ARGUMENTS rvalues) method_assignment?)
	;

atom
	:	'(' expression ')' -> expression |
		'_self'  -> SELF |
		'_clone' -> CLONE |
		'_super' -> SUPER | //('(' IDENTIFIER ')')? | // XXX _super(exemplar) is legal, but we solve this in a later stage
		'_unset' -> UNSET |
		'_true'  -> TRUE  |
		'_false' -> FALSE |
		'_maybe' -> MAYBE |
		'_thisthread' -> THISTHREAD |
		//'no_way' | // XXX: valid keyword, but we solve this in a later stage; for now take it as an IDENTIFIER
		simple_vector |
		procedure_declaration |
		block |
		protect_block |
		try_block |
		catch_block |
		lock_block |
		if_statement |
		for_loop |
		over_loop |
		loop |
		slot |
		INTEGER |
		FLOAT |
		CHARACTER_SEQUENCE |
		CHARACTER |
		SYMBOL |
//		LABEL |
		IDENTIFIER |
		PACKAGED_IDENTIFIER |
		LABELED_GLOBAL_VARIABLE
	;

procedure_declaration
//	:	'_iter'? '_proc'^ LABEL? '(' parameter? (',' parameter)* ')' NEWLINE*
	:	'_iter'? '_proc' LABEL? '(' parameter_list? ')' NEWLINE*
			(handling NEWLINE*)*
			(statement NEWLINE*)*
		'_endproc' -> ^(PROCEDURE_DECLARATION '_iter'? ^(PARAMETERS parameter_list?) ^(BODY handling* statement*))
	;
	
block
	:	'_block' LABEL? NEWLINE*
			(handling NEWLINE*)*
			(statement NEWLINE*)*
		'_endblock' -> ^(BLOCK LABEL? ^(BODY handling* statement*))
	;

handling
//	:	'_handling' IDENTIFIER (',' IDENTIFIER)* '_with' expression |
//		'_handling' IDENTIFIER (',' IDENTIFIER)* '_with' '_default' |
//		'_handling' '_default'
	:	'_handling' (IDENTIFIER (',' IDENTIFIER)* with_default) -> ^(HANDLING IDENTIFIER+ with_default)
	;

protect_block
	:	'_protect' locking? NEWLINE*
			(statement NEWLINE*)*
		protection
		'_endprotect' -> ^(PROTECT locking? ^(BODY statement*) protection)
	;

locking
	:	'_locking' expression -> ^(LOCKING expression)
	;

protection
	:	'_protection' NEWLINE*
			(statement NEWLINE*)* -> ^(PROTECTION ^(BODY statement*))
	;

try_block
	:	'_try' NEWLINE* with? NEWLINE*
			(statement NEWLINE+)*
		when+
		'_endtry' -> ^(TRY with ^(BODY statement*) when+)
	;
	
with
	:	'_with' NEWLINE* expression -> ^(WITH expression)
	;
	
with_default
	:	'_with' NEWLINE* (expression | '_default') -> ^(WITH expression? '_default')
	;
	
when
	:	'_when' IDENTIFIER (',' NEWLINE* IDENTIFIER)* NEWLINE*
			(statement NEWLINE+)* -> ^(WHEN IDENTIFIER+ ^(BODY statement*))
	;

catch_block
	:	'_catch' (LABEL | SYMBOL) NEWLINE*
			(handling NEWLINE+)*
			(statement NEWLINE+)*
		'_endcatch' -> ^(CATCH LABEL? SYMBOL? ^(BODY handling* statement*))
	;
	
lock_block
	:	'_lock' expression NEWLINE*
			(statement NEWLINE+)*
		'_endlock' -> ^(LOCK expression ^(BODY statement*))
	;

if_statement
	:	'_if' expression NEWLINE*
		'_then' NEWLINE*
			(statement NEWLINE*)*
		if_statement_elif*
		if_statement_else?
		'_endif' -> ^(IF expression ^(BODY statement*) if_statement_elif* if_statement_else*)
	;

if_statement_elif
	:	'_elif' expression NEWLINE*
		'_then' NEWLINE*
			(statement NEWLINE*)* -> ^(ELIF expression ^(BODY statement*))
	;

if_statement_else
	:	'_else' NEWLINE*
			(statement NEWLINE*)* -> ^(ELSE ^(BODY statement*))

	;

for_loop
	:	'_for' ((lvalues_parenthesized) | lvalues) over_loop -> ^(FOR lvalues_parenthesized? lvalues? over_loop)
	;

over_loop
	:	'_over' expression NEWLINE*
		loop -> ^(OVER expression loop)
	;

loop
	:	'_loop' LABEL? NEWLINE*
			(handling NEWLINE+)*
			(statement NEWLINE+)*
		finally_block?
		'_endloop' -> ^(LOOP LABEL? ^(BODY handling* statement*) finally_block?)
	;

finally_block
	:	'_finally' NEWLINE*
			(statement NEWLINE+)* -> ^(FINALLY ^(BODY statement*))
	;

simple_vector
	:	'{' NEWLINE* rvalues? NEWLINE* '}' -> ^(SIMPLE_VECTOR rvalues?)
	;
	
variable
	:	IDENTIFIER | PACKAGED_IDENTIFIER
	;

slot	:	'.' IDENTIFIER -> ^(SLOT_ACCESS IDENTIFIER)
	;

lvalues
	:	lvalue (',' NEWLINE* lvalue)* -> lvalue+
	;

lvalue
	:	'_gather'? (variable|slot) -> ^(LVALUE '_gather'? variable? slot?)
	;
	
lvalues_parenthesized
	:	'(' lvalues ')' -> lvalues
	;

rvalues
	:	rvalue (',' NEWLINE* rvalue)* -> rvalue+
	;
	
rvalue
	:	'_scatter'? expression -> ^(RVALUE '_scatter'? expression)
	;

rvalues_parenthesized
	:	'('! rvalues ')'!
	;

IDENTIFIER
	:	('a'..'z'|'A'..'Z'|'!') ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'!'|'?')*
	;

PACKAGED_IDENTIFIER
	:	IDENTIFIER ':' IDENTIFIER
	;

SYMBOL
	:	':' (
			('0'..'9'|'a'..'z'|'A'..'Z'|'_'|'!'|'?') |
			('|' ('0'..'9'|'a'..'z'|'A'..'Z'|' '|'('|')'|'{'|'}'|'['|']'|';'|':'|'*'|'/'|'_'|'-'|'\\'|'?'|'"'|'<'|'>'|'!')* '|')
		)+
	;

LABEL
	:	'@' ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'?'|'!'|'|')+
	;

LABELED_GLOBAL_VARIABLE
	:	LABEL ':' IDENTIFIER
	;
	
INTEGER
	:	('0'..'9')+ // XXX: TODO: Radix and exponent
	;

FLOAT  // XXX: TODO: Radix
	:	('0'..'9')+ '.' ('0'..'9')* EXPONENT? // XXX: TODO: "9.method_name" is _NOT_ a float, but a method invocation on an integer
	|	'.' ('0'..'9')+ EXPONENT?
	|	('0'..'9')+ EXPONENT
	;

ENCODING_COMMENT
	:	'#%' ~('\n'|'\r') { skip(); }
	;

METHOD_COMMENT
	:	'##' ~('\n'|'\r') { $channel = HIDDEN; }
	;

COMMENT
	:	'#' ~('\n'|'\r')* { skip(); }
	;

CHARACTER_SEQUENCE
	:	'"' ( ESC_SEQ | ~('"') )* '"'
	;

CHARACTER
//	:	'%' ('0'..'9'|'a'..'z'|'A'..'Z'|' '|'space'|'('|')'|'{'|'}'|'['|']'|';'|':'|'*'|'/'|'_'|'-'|'\\'|'?'|'"'|'<'|'>'|'|'|'.'|','|'$'|'='|'\t'|'^'|'\''|)
	:	'%' ('space' | 'tab' | 'newline' | ~('\n'|'\r'))
//		'%u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT 
	;
	
fragment
EXPONENT
	:	('e'|'E') ('+'|'-')? ('0'..'9')+
	;

fragment
HEX_DIGIT
	:	('0'..'9'|'a'..'f'|'A'..'F')
	;

fragment
ESC_SEQ
	:	'\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
	|	UNICODE_ESC
	|	OCTAL_ESC
	;
    
fragment
OCTAL_ESC
	:	'\\' ('0'..'3') ('0'..'7') ('0'..'7')
	|	'\\' ('0'..'7') ('0'..'7')
	|	'\\' ('0'..'7')
 	;

fragment
UNICODE_ESC
	:	'\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
	;

WS
	:	(' ' | '\t') { skip(); }
	;

NEWLINE
	:	('\r'? '\n') //{ $channel = HIDDEN; }
	;
