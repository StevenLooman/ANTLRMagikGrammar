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
	DECLARATION;
	ELIF;
	ELSE;
	FALSE;
	FINALLY;
	FOR;
	HANDLING;
	IF;
	IMPORT;
	INDEXED_METHOD_DECLARATION;
	INDEXED_METHOD_INVOCATION;
	LEAVE;
	LOCK;
	LOCKING;
	LOOP;
	LOOPBODY;
	LVALUE;
	MAYBE;
	METHOD_DECLARATION;
	METHOD_INVOCATION;
	METHOD_MESSAGE_CALL;
	METHOD_MESSAGE_INDEXER;
	METHOD_MODIFIERS;
	OVER;
	PACKAGE_SPECIFICATION;
	PARALLEL_ASSIGNMENT;
	PARAMETER_MODIFIERS;
	PARAMETER;
	PRAGMA_VALUE;
	PRAGMA;
	PRIMITIVE;
	PROCEDURE_CALL;
	PROCEDURE_DECLARATION;
	PROTECT;
	PROTECTION;
	RESULT;
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

@lexer::header {
	package com.realworld.sonar.magik.antlr;
}

@header {
	package com.realworld.sonar.magik.antlr;
}

@parser::members {
	private void enableNewline() {
		//CommonTokenStream stream = (CommonTokenStream)input;
		//stream.setTokenTypeChannel(NEWLINE, DEFAULT_TOKEN_CHANNEL);
	}
	
	private void disableNewline() {
		//CommonTokenStream stream = (CommonTokenStream)input;
		//stream.setTokenTypeChannel(NEWLINE, HIDDEN);
	}
}

magik
	:	(package_specification |
		 pragma |
		 method_declaration |
		 statement |
		 transmit |
		 NEWLINE!)+
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
			body
		'_endmethod' -> ^(METHOD_DECLARATION method_modifiers IDENTIFIER method_declaration_dot? method_declaration_lsquare? method_declaration_assignment? body)
	;
	
method_declaration_dot
	:	'.' IDENTIFIER ('(' parameter_list? NEWLINE* ')')? -> ^(METHOD_MESSAGE_CALL IDENTIFIER '('? parameter_list?)
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
	:	parameter (','? NEWLINE* parameter)* -> parameter+
	;

parameter
	:	parameter_modifiers NEWLINE* IDENTIFIER -> ^(PARAMETER parameter_modifiers IDENTIFIER) // (param_mods | newline)*
	;

parameter_modifiers
	:	('_optional' |
		 '_gather')* -> ^(PARAMETER_MODIFIERS '_optional'* '_gather'*)
	;
	
body
	:	(handling NEWLINE*)*
		(statement NEWLINE*)*
		-> ^(BODY handling* statement*)
	;

statement
	:	(parallel_assignment |
		 variable_declaration |
		 throw_statement |
		 continue_statement |
		 leave_statement |
		 loopbody_statement |
		 primitive_statement |
		 expression |
		 result_statement |
	 	 return_statement |
		 ';'!)
	;

parallel_assignment
	// XXX: TODO: make this part of an expression? '(' expression ')' would become '(' expression (',' expression)* ')'
	//  and this would become a normal assignment
	//  this might fix the ambiguity of the grammar
	:	lvalues_parenthesized '<<' NEWLINE* (rvalues_parenthesized | rvalues) -> ^(PARALLEL_ASSIGNMENT lvalues_parenthesized rvalues_parenthesized? rvalues?)
	;
	
variable_declaration
	// constant local constant parallel_assignemnt | sequential_assignment
	:	('_recursive' | '_constant' | '_local' | '_dynamic' | '_global' | '_import')+ (variable_declaration_sequential_assignment | variable_declaration_parallel_assignment)?
			-> ^(VARIABLE_DECLARATION '_recursive'? '_constant'? '_local'? '_dynamic'? '_global'? '_import'?
				variable_declaration_sequential_assignment? variable_declaration_parallel_assignment?)
	;

variable_declaration_parallel_assignment
	:	lvalues_parenthesized '<<' NEWLINE* (rvalues_parenthesized | rvalues) -> ^(DECLARATION lvalues_parenthesized ^(ARGUMENTS rvalues_parenthesized? rvalues?))
	;

variable_declaration_sequential_assignment
	:	lvalue ('<<' NEWLINE* rvalue)? NEWLINE* (',' NEWLINE* lvalue ('<<' NEWLINE* rvalue)?)* -> ^(DECLARATION lvalue+ ^(ARGUMENTS rvalue*))
	;

throw_statement
	:	'_throw' (SYMBOL|LABEL) with? -> ^(THROW SYMBOL? LABEL? with?)
	;

continue_statement
	:	'_continue' LABEL? with? -> ^(CONTINUE LABEL? with?)
	;

leave_statement
	:	'_leave' LABEL? with? ->  ^(LEAVE LABEL? with?)
	;
	
loopbody_statement
	:	'_loopbody' rvalues_parenthesized -> ^(LOOPBODY rvalues_parenthesized)
	;

primitive_statement
	:	'_primitive' NUMBER -> ^(PRIMITIVE NUMBER)
	;

return_statement
	:	'_return' (rvalues_parenthesized | rvalues)? -> ^(RETURN rvalues_parenthesized? rvalues?)
	;

result_statement
	:	'>>' NEWLINE* (rvalues_parenthesized | rvalues) -> ^(RESULT rvalues_parenthesized? rvalues?)
	;

expression
	:	assignment_expression
	;

assignment_expression
	:	logical_orif_expression (('<<' | '^<<' | '+<<' | '-<<' | '/<<' | '*<<'| '+^<<' | '-^<<' | '/^<<' | '*^<<')^ (NEWLINE!)* logical_orif_expression)*
	;

logical_orif_expression
	:	logical_xor_expression (('_or' | '_orif')^ (NEWLINE!)* logical_xor_expression)*
	;

logical_xor_expression
	:	logical_andif_expression ('_xor'^ (NEWLINE!)* logical_andif_expression)*
	;

logical_andif_expression
	:	equality_expression (('_and' | '_andif')^ (NEWLINE!)* equality_expression)*
	;

equality_expression
	:	relational_expression (('<>' | '=' |'_is' | '_isnt' | '~=')^ (NEWLINE!)* relational_expression)*
	;

relational_expression
	:	additive_expression (('_cf' | '<' | '<=' | '>' | '>=')^ (NEWLINE!)* additive_expression)*
	;

additive_expression
	:	multiplicative_expression (('+' | '-')^ (NEWLINE!)* multiplicative_expression)*
	;

multiplicative_expression
	:	exponential_expression (('*' | '/' | '_div' | '_mod')^ (NEWLINE!)* exponential_expression)*
	;

exponential_expression
	:	unary_expression ('**'^ (NEWLINE!)* unary_expression)*
	;

unary_expression
	:	'~' NEWLINE* unary_expression -> ^('~' unary_expression) |
		'_not' NEWLINE* unary_expression -> ^('_not' unary_expression) |
		'-' NEWLINE* unary_expression -> ^(SIGN_MINUS unary_expression) |
		'+' NEWLINE* unary_expression -> ^(SIGN_PLUS unary_expression) |
		'_allresults' NEWLINE* unary_expression -> ^('_allresults' unary_expression) |
		'_scatter' NEWLINE* unary_expression -> ^('_scatter' unary_expression) |
		postfix_expression
	;

postfix_expression
	:	atom { enableNewline(); }
			(
				('(') => procedure_call |
				('.') => method_invocation |
				('[') => indexed_method_invocation
			)* { disableNewline(); }
	;

procedure_call
	:	'(' NEWLINE* rvalues? NEWLINE* ')' -> ^(PROCEDURE_CALL ^(ARGUMENTS rvalues?))
	;

method_invocation // XXX: TODO: field access method invocation
//	:	'.' IDENTIFIER '(' expression? (',' expression)* ')'  '<<' expression |
//		'.' IDENTIFIER '(' expression? (',' expression)* ')' '^<<' expression |
//		'.' IDENTIFIER '(' expression? (',' expression)* ')'                  |
//		'.' IDENTIFIER                                        '<<' expression | // field method invocation
//		'.' IDENTIFIER                                       '^<<' expression | // field method invocation
//		'.' IDENTIFIER                                                          // field method invocation
	:	'.' NEWLINE* IDENTIFIER ('(' NEWLINE* rvalues? NEWLINE* ')')?  method_assignment? -> ^(METHOD_INVOCATION IDENTIFIER ^(ARGUMENTS rvalues?) method_assignment?)
	;

method_assignment
	:	'<<' NEWLINE* expression -> ^(CHEVRON expression) |
		'^<<' NEWLINE* expression -> ^(BOOT_CHEVRON expression)
	;
	
indexed_method_invocation
//	:	'[' expression (',' expression)* ']'                  '<<' expression |
//		'[' expression (',' expression)* ']'                 '^<<' expression |
//		'[' expression (',' expression)* ']'
	:	'[' NEWLINE* rvalues NEWLINE* ']' method_assignment? -> ^(INDEXED_METHOD_INVOCATION ^(ARGUMENTS rvalues) method_assignment?)
	;

atom
	:	'(' expression ')' -> expression |
		'_self'  -> SELF  |
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
		NUMBER |
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
			body
		'_endproc' -> ^(PROCEDURE_DECLARATION '_iter'? LABEL? parameter_list? body)
	;
	
block
	:	'_block' LABEL? NEWLINE*
			body
		'_endblock' -> ^(BLOCK LABEL? body)
	;

handling
//	:	'_handling' IDENTIFIER (',' IDENTIFIER)* '_with' expression |
//		'_handling' IDENTIFIER (',' IDENTIFIER)* '_with' '_default' |
//		'_handling' '_default'
	:	'_handling' (IDENTIFIER (',' IDENTIFIER)* NEWLINE* with_default) -> ^(HANDLING IDENTIFIER+ with_default)
	;

protect_block
	:	'_protect' locking? NEWLINE*
			body
		protection
		'_endprotect' -> ^(PROTECT locking? body protection)
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
			body
		when+
		'_endtry' -> ^(TRY with? body when+)
	;
	
with
	:	'_with' NEWLINE* '('? expression (',' NEWLINE* expression)* ')'? -> ^(WITH expression+)
	;
	
with_default
	:	'_with' NEWLINE* (expression | '_default') -> ^(WITH expression? '_default'?)
	;
	
when
	:	'_when' NEWLINE* IDENTIFIER (',' NEWLINE* IDENTIFIER)* NEWLINE*
			body -> ^(WHEN IDENTIFIER+ body)
	;

catch_block
	:	'_catch' (expression | LABEL) NEWLINE*
			body
		'_endcatch' -> ^(CATCH expression? LABEL? body)
	;
	
lock_block
	:	'_lock' expression NEWLINE*
			body
		'_endlock' -> ^(LOCK expression body)
	;

if_statement
	:	'_if' NEWLINE* expression NEWLINE*
		'_then' NEWLINE*
			body
		if_statement_elif*
		if_statement_else?
		'_endif' -> ^(IF expression body if_statement_elif* if_statement_else?)
	;

if_statement_elif
	:	'_elif' NEWLINE* expression NEWLINE*
		'_then' NEWLINE*
			body -> ^(ELIF expression body)
	;

if_statement_else
	:	'_else' NEWLINE*
			body -> ^(ELSE body)

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
			body
		finally_block?
		'_endloop' -> ^(LOOP LABEL? body finally_block?)
	;

finally_block
	:	'_finally' NEWLINE*
			body -> ^(FINALLY body)
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
	:	'(' rvalues ')' -> rvalues
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
			('|' (~'|')* '|')
		)+
	;

LABEL
	:	'@' ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'?'|'!'|'|')+
	;

LABELED_GLOBAL_VARIABLE
	:	LABEL ':' IDENTIFIER
	;
	
NUMBER
	:	(('0'..'9')+ '.' ('0'..'9')+) => FLOAT |
		(('0'..'9')+ '.' ~('0'..'9')) => INTEGER
	;
	
fragment
INTEGER // XXX: TODO: Radix
	:	('0'..'9')+ EXPONENT?
	;
	
fragment
FLOAT  // XXX: TODO: Radix
	:	('0'..'9')+ '.' ('0'..'9')+ EXPONENT?
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
	:	'"' (~'"')* '"'
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

WS
	:	(' ' | '\t') { skip(); }
	;

NEWLINE
	:	('\r'? '\n')
	;
