grammar Magik;

options {
	output = AST;
	k = 1;
}

magik
	:	(package_specification |
		 pragma |
		 method_declaration |
		 statement |
		 transmit |
		 NEWLINE+)+
	;
	
transmit
	:	'$'
	;

package_specification
	:	'_package' IDENTIFIER
	;

pragma
	:	'_pragma' '(' pragma_param (',' pragma_param)* ')'
	;

pragma_param
	:	IDENTIFIER '='^ pragma_value
	;

pragma_value
	:	IDENTIFIER |
		'{' IDENTIFIER (',' IDENTIFIER)* '}'
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
	:	method_modifiers '_method' IDENTIFIER (('.' IDENTIFIER ('(' parameter_list? ')')?) | ('[' parameter_list ']')) (('<<'|'^<<') IDENTIFIER)? NEWLINE+
				(handling NEWLINE+)*
			(statement NEWLINE+)*
		'_endmethod'
	;

method_modifiers
	:	('_private' |
		 '_abstract' |
		 '_iter')*
	;

parameter_list
	:	parameter (',' NEWLINE* parameter)*
	;

parameter
	:	parameter_modifiers NEWLINE? IDENTIFIER
	;

parameter_modifiers
	:	('_optional' |
		 '_gather')*
	;

statement
	:	(parallel_assignment |
		 local_declaration_assignment |
		 dynamic_declaration_assignment |
		 global_declaration_assignment |
		 throw_statement |
		 import_statement |
		 control_statement |
		 loopbody_statement |
		 return_statement |
		 expression |
		 ';')
	;

parallel_assignment
	:	lvalue_tuple_parenthesized '<<'^ NEWLINE* (rvalue_tuple_parenthesized | rvalue_tuple)
	;

local_declaration_assignment
//	:	'_local' '_constant'?  lvalue_tuple
//		'_local' '_constant'?  lvalue_tuple '<<' expression
//		'_local' '_constant'? '(' lvalue_tuple ')'
//		'_local' '_constant'? '(' lvalue_tuple ')' '<<' expression |
//		'_local' '_constant'? '(' lvalue_tuple ')' '<<' '(' rvalue_tuple ')' |
//		'_local' '_constsant'? (variable '<<' expression)+ // XXX: TODO
	:	'_local' '_constant'? (lvalue_tuple_parenthesized | lvalue_tuple) ('<<' NEWLINE* (rvalue_tuple_parenthesized | expression))?
	;

dynamic_declaration_assignment
	:	'_dynamic' (lvalue_tuple_parenthesized | lvalue_tuple) ('<<' NEWLINE* (rvalue_tuple_parenthesized | expression))?
	;

global_declaration_assignment
	:	'_global' '_constant'? (lvalue_tuple_parenthesized | lvalue_tuple) ('<<' NEWLINE* (rvalue_tuple_parenthesized | expression))?
	;

throw_statement
	:	'_throw'^ (SYMBOL|LABEL) ('_with' expression)? // XXX: TODO: 1st expression should be SYMBOL?
	;

import_statement
	:	'_import'^ expression (',' expression)*
	;

control_statement
	:	('_continue' | '_leave')^ LABEL? ('_with' expression)*
	;
	
loopbody_statement
	:	'_loopbody' rvalue_tuple_parenthesized
	;

return_statement
	:	'_return' (rvalue_tuple_parenthesized | rvalue_tuple)? |
		'>>' NEWLINE* (rvalue_tuple_parenthesized | rvalue_tuple)
	;

expression
	:	assignment_expression
	;

assignment_expression
	:	logical_orif_expression (('<<' | '^<<'|'+<<'|'-<<'|'/<<'|'*<<')^ NEWLINE* logical_orif_expression)*
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
	:	'~'^ unary_expression |
		'_not'^ unary_expression |
		'-'^ unary_expression |
		'+'^ unary_expression |
		'_allresults' unary_expression |
		postfix_expression
	;

postfix_expression
	:	//literal { test: }({ if (isNewLine()) { break test; } } procedure_call? { System.out.println("procedure call"); })
		//literal (~NEWLINE) => (procedure_call? | (method_call | indexed_method_call)*) //(procedure_call? | method_call*)
//		literal (method_call | indexed_method_call)* //(procedure_call? | method_call*)
//		literal procedure_call?
		literal 
			(
				('(') => procedure_call?
			)
			(
				('.') => method_call |
				('[') => indexed_method_call
			)*
	;

procedure_call
	:	(~NEWLINE) => '(' NEWLINE* rvalue_tuple? NEWLINE* ')'
	;

method_call
//	:	'.' IDENTIFIER '(' expression? (',' expression)* ')'  '<<' expression |
//		'.' IDENTIFIER '(' expression? (',' expression)* ')' '^<<' expression |
//		'.' IDENTIFIER                                        '<<' expression |
//		'.' IDENTIFIER                                       '^<<' expression |
//		'.' IDENTIFIER '(' expression? (',' expression)* ')' |
//		'.' IDENTIFIER
	:	(~NEWLINE) => '.' NEWLINE* IDENTIFIER ('(' NEWLINE* rvalue_tuple? NEWLINE* ')')?  (('<<'|'^<<') NEWLINE* expression)?
	;

indexed_method_call
//	:	'[' expression (',' expression)* ']'                  '<<' expression |
//		'[' expression (',' expression)* ']'                 '^<<' expression |
//		'[' expression (',' expression)* ']'
	:	(~NEWLINE) => '[' NEWLINE* rvalue_tuple NEWLINE* ']' (('<<'|'^<<') NEWLINE* expression)?
	;

literal
	:	'(' expression ')' |
		'_self'  |
		'_clone' |
		'_super' | //('(' IDENTIFIER ')')? | // XXX _super(exemplar) is legal, but we solve this in a later stage
		'_unset' |
		'_true'  |
		'_false' |
		'_maybe' |
		'_thisthread' |
		//'no_way' | // XXX: valid keyword, but we solve this in a later stage
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
//		GLBOBAL_VARIABLE
	;

procedure_declaration
//	:	'_iter'? '_proc'^ LABEL? '(' parameter? (',' parameter)* ')' NEWLINE*
	:	'_iter'? '_proc'^ LABEL? '(' parameter_list? ')' NEWLINE*
			(handling NEWLINE*)*
			(statement NEWLINE*)*
		'_endproc'
	;
	
block
	:	'_block' LABEL? NEWLINE*
			(handling NEWLINE*)*
			(statement NEWLINE*)*
		'_endblock'
	;

handling
//	:	'_handling' IDENTIFIER (',' IDENTIFIER)* '_with' expression |
//		'_handling' IDENTIFIER (',' IDENTIFIER)* '_with' '_default' |
//		'_handling' '_default'
	:	'_handling' (IDENTIFIER (',' IDENTIFIER)*)? (('_with' NEWLINE* (expression | '_default')) | '_default')
	;

protect_block
	:	'_protect'^ ('_locking' expression)? NEWLINE*
			(statement NEWLINE*)*
		'_protection' NEWLINE*
			(statement NEWLINE*)*
		'_endprotect'
	;

try_block
	:	'_try' NEWLINE* ('_with' expression)? NEWLINE*
			(statement NEWLINE+)*
		('_when' IDENTIFIER (',' NEWLINE* IDENTIFIER)* NEWLINE*
			(statement NEWLINE+)*)+
		'_endtry'
	;

catch_block
	:	'_catch'^ (LABEL | SYMBOL) NEWLINE*
			(handling NEWLINE+)*
			(statement NEWLINE+)*
		'_endcatch'
	;
	
lock_block
	:	'_lock' expression NEWLINE*
			(statement NEWLINE+)*
		'_endlock'
	;

if_statement
	:	'_if'^ expression NEWLINE*
		'_then' NEWLINE*
			(statement NEWLINE*)*
		if_statement_elif*
		if_statement_else?
		'_endif'
	;

if_statement_elif
	:	'_elif' expression NEWLINE*
		'_then' NEWLINE*
			(statement NEWLINE*)*
	;

if_statement_else
	:	'_else' NEWLINE*
			(statement NEWLINE*)*

	;

for_loop
	:	'_for' ((lvalue_tuple_parenthesized) | lvalue_tuple) over_loop
	;

over_loop
	:	'_over' expression NEWLINE*
		loop
	;

loop
	:	'_loop' LABEL? NEWLINE*
			(handling NEWLINE+)*
			(statement NEWLINE+)*
		finally_block?
		'_endloop'
	;

finally_block
	:	'_finally' NEWLINE*
			(statement NEWLINE+)*
	;

simple_vector
	:	'{' NEWLINE* rvalue_tuple? NEWLINE* '}'
	;
	
variable
	:	IDENTIFIER | PACKAGED_IDENTIFIER
	;

slot	:	'.' IDENTIFIER
	;

lvalue_tuple
	:	'_gather'? (variable|slot) (',' NEWLINE* '_gather'? (variable|slot))*
	;
	
lvalue_tuple_parenthesized
	:	'(' lvalue_tuple ')'
	;

rvalue_tuple
	:	'_scatter'? expression (',' NEWLINE* '_scatter'? expression)*
	;

rvalue_tuple_parenthesized
	:	'(' rvalue_tuple ')'
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
