tree grammar Magik_tree;

options {
	tokenVocab = Magik;
	ASTLabelType = CommonTree;
}

@header {
//	package com.realworld.sonar.magik.antlr;
}

@members {
	public void p(String str) {
		System.out.println(str);
	}
}

magik
	:	(package_specification |
		 pragma |
		 method_declaration |
		 statement |
		 TRANSMIT)+
	;
	
package_specification
	:	^(PACKAGE_SPECIFICATION name=IDENTIFIER) { p("package_specification: " + $name.text); }
	;
	
pragma	:	^(PRAGMA pragma_param+)
	;
	
pragma_param
	:	^(PRAGMA_VALUE IDENTIFIER IDENTIFIER+)
	;

method_declaration
	:	^(METHOD_DECLARATION method_modifiers exemplar=IDENTIFIER { p("method_declaration: exemplar: " +  $exemplar.text); } (method_declaration_dot | method_declaration_lsquare) method_declaration_assignment? body)
	;
	
method_declaration_dot
	:	^(METHOD_MESSAGE_CALL name=IDENTIFIER { p("method_declaration: method: " +  $name.text); } parameter*)
	;

method_declaration_lsquare
	:	^(METHOD_MESSAGE_INDEXER parameter*)
	;

method_declaration_assignment
	:	^(CHEVRON p=IDENTIFIER) { p("method_declaration: assignment: chevron: " + $p.text); } |
		^(BOOT_CHEVRON p=IDENTIFIER) { p("method_declaration: assignment: boot_chevron: " + $p.text); }
	;

parameter
	:	^(PARAMETER parameter_modifiers name=IDENTIFIER)
	;

parameter_modifiers
	:	^(PARAMETER_MODIFIERS '_optional'* '_gather'*)
	;

method_modifiers
	:	^(METHOD_MODIFIERS '_private'* '_abstract'* '_iter'*)
	;
	
body
	:	^(BODY { p("body"); } handling* statement*)
	;

statement
	:	parallel_assignment |
		local_declaration_assignment |
		dynamic_declaration_assignment |
		global_declaration_assignment |
		throw_statement |
		import_statement |
		continue_statement |
		leave_statement |
		loopbody_statement |
		return_statement |
		expression
//		procedure_call |
//		method_call |
//		indexed_method_call
	;
	
procedure_call
	:	^(PROCEDURE_CALL argument*) { p("procedure call"); }
	;
	
argument
	:	^(ARGUMENTS rvalue*)
	;
	
method_call
	:	^(METHOD_CALL method=IDENTIFIER { p("method call: method: " + $method.text); } ^(ARGUMENTS rvalue*) assignment?)
	;

assignment
	:	^(CHEVRON IDENTIFIER) |
		^(BOOT_CHEVRON IDENTIFIER)
	;
		
indexed_method_call
	:	^(INDEXED_METHOD_CALL ^(ARGUMENTS rvalue+) assignment?) { p("indexed method call"); }
	;

parallel_assignment
	:	^(PARALLEL_ASSIGNMENT lvalue* rvalue*)
	;

local_declaration_assignment
	:	^(LOCAL_VARIABLE_DECLARATION ^(VARIABLE_DECLARATION lvalue*) ^(ARGUMENTS rvalue* expression?)) 
	;

dynamic_declaration_assignment
	:	^(DYNAMIC_VARIABLE_DECLARATION ^(VARIABLE_DECLARATION lvalue*) ^(ARGUMENTS rvalue* expression?)) 
	;

global_declaration_assignment
	:	^(GLOBAL_VARIABLE_DECLARATION ^(VARIABLE_DECLARATION lvalue*) ^(ARGUMENTS rvalue* expression?)) 
	;

lvalue
	:	^(LVALUE '_gather'? IDENTIFIER? slot?)
	;

rvalue
	:	^(RVALUE { p("rvalue"); } '_scatter'? expression)
	;

return_statement
	:	^(RETURN { p("return"); } rvalue*)
	;
	
for_loop
	:	^(FOR lvalue* over_loop)
	;

over_loop
	:	^(OVER expression loop)
	;

loop
	:	^(LOOP LABEL? body)
	;
	
expression
	:	(binary_expression | unary_expression | atom) procedure_call? (method_call | indexed_method_call)*
	;

binary_expression
	:	^(binary_operator l=expression r=expression?)
	;

binary_operator
	:	(o='<<'   | o='^<<'    | o='+<<'     | o='-<<'      | o='/<<' | o='*<<' |
		 o='_or'  | o='_orif'  |
		 o='_xor' |
		 o='_and' | o='_andif' |
		 o='<>'   | o='='      | o='_is'     | o='_isnt'    | o='~=' |
		 o='_cf'  | o='<'      | o='<='      | o='>'        | o='>=' |
		 o='+'    | o='-'      |
		 o='*'    | o='/'      | o='_div'    | o='_mod'     |
		 o='**')
		 	{ p("expression: binary_op: " + $o.text); }
	;

unary_expression
	:	^(unary_operator r=expression)
	;

unary_operator
	:	(o='~'    | o='_not'   | o=SIGN_PLUS | o=SIGN_MINUS | o='_allresults')
		 	{ p("expression: unary_op: " + $o.text); }
	;

atom
	:	SELF  { p("self"); }  |
		CLONE { p("clone"); } |
		SUPER { p("super"); } |
		UNSET { p("unset"); } |
		TRUE  { p("true"); }  |
		FALSE { p("false"); } |
		MAYBE { p("maybe"); } |
		THISTHREAD { p("thisthread"); } |
		simple_vector |
		slot |
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
		e=IDENTIFIER { p("expression: identifier: " + $e.text); } |
		e=INTEGER { p("expression: integer: " + $e.text); } |
		e=FLOAT { p("expression: float: " + $e.text); } |
		e=CHARACTER_SEQUENCE { p("expression: character_sequence: " + $e.text); } |
		e=CHARACTER { p("expression: character: " + $e.text); } |
		e=SYMBOL { p("expression: symbol: " + $e.text); } |
		e=PACKAGED_IDENTIFIER { p("expression: packaged_identifier: " + $e.text); } |
		e=LABELED_GLOBAL_VARIABLE { p("expression: labeled_global_variable: " + $e.text); }
	;

slot
	:	^(SLOT_ACCESS name=IDENTIFIER) { p("expression: slot: " + $name.text); }
	;

simple_vector
	:	^(SIMPLE_VECTOR { p("expression: simple_vector"); } rvalue*)
	;

procedure_declaration
	:	^(PROCEDURE_DECLARATION '_iter'? ^(PARAMETERS parameter*) body)
	;

handling
	:	^(HANDLING IDENTIFIER+)
	;
	
block
	:	^(BLOCK LABEL? body)
	;

protect_block
	:	^(PROTECT (^(LOCKING expression))? body ^(PROTECTION body))
	;

try_block
	:	^(TRY with body (^(WHEN IDENTIFIER+ body))+)
	;

catch_block
	:	^(CATCH LABEL? SYMBOL? body)
	;
	
lock_block
	:	^(LOCK expression body)
	;

throw_statement
	:	^(THROW SYMBOL? LABEL? with)
	;

import_statement
	:	^(IMPORT expression+)
	;

continue_statement
	:	^(CONTINUE LABEL? with)
	;

leave_statement
	:	^(LEAVE LABEL? with)
	;

loopbody_statement
	:	^(LOOPBODY rvalue*)
	;

if_statement
	:	^(IF expression body if_statement_elif* if_statement_else*)
	;
	
if_statement_elif
	:	^(ELIF expression body)
	;

if_statement_else
	:	^(ELSE body)
	;

with
	:	^(WITH (expression | '_default'))
	;