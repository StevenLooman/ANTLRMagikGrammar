tree grammar MagikExemplarFinder;

options {
	tokenVocab = Magik;
	ASTLabelType = CommonTree;
}

@members {
	public void p(String str) {
		//System.out.println(str);
	}
	
	
	private List<String> exemplars = new ArrayList<String>();
	private boolean recording = false;
	
	public void onIdentifier(String identifier) {
		if (identifier.equals("def_slotted_exemplar"))
			recording = true;
	}
	
	public void onSymbol(String symbol) {
		if (recording) {
			exemplars.add(symbol.substring(1)); // strip off the ':'
			recording = false;
		}
	}
	
	public List<String> getExemplars() {
		return exemplars;
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
	:	^(PACKAGE_SPECIFICATION IDENTIFIER)
	;
	
pragma	:	^(PRAGMA pragma_param+)
	;
	
pragma_param
	:	^(PRAGMA_VALUE IDENTIFIER IDENTIFIER+)
	;

method_declaration
	:	^(METHOD_DECLARATION method_modifiers IDENTIFIER
			(method_declaration_dot | method_declaration_indexer)
			method_declaration_assignment?
			body)
	;
	
method_declaration_dot
	:	^(METHOD_MESSAGE_CALL IDENTIFIER
			'('? parameter*)
	;

method_declaration_indexer
	:	^(METHOD_MESSAGE_INDEXER parameter*)
	;

method_declaration_assignment
	:	^(CHEVRON IDENTIFIER) |
		^(BOOT_CHEVRON IDENTIFIER)
	;

parameter
	:	^(PARAMETER parameter_modifiers IDENTIFIER)
	;

parameter_modifiers
	:	^(PARAMETER_MODIFIERS '_optional'* '_gather'*)
	;

method_modifiers
	:	^(METHOD_MODIFIERS '_private'* '_abstract'* '_iter'*)
	;
	
body
	:	^(BODY handling* statement*)
	;

statement
	:	(parallel_assignment |
/*
		 local_declaration_assignment |
		 constant_declaration_assignment |
		 dynamic_declaration_assignment |
		 global_declaration_assignment |
*/
		 variable_declaration |
		 throw_statement |
		 import_statement |
		 continue_statement |
		 leave_statement |
		 loopbody_statement |
		 expression |
		 return_statement |
		 result_statement)
	;

procedure_call
	:	^(PROCEDURE_CALL arguments*)
	;
	
arguments
	:	^(ARGUMENTS rvalue*)
	;
	
method_call
	:	^(METHOD_CALL IDENTIFIER ^(ARGUMENTS rvalue*) assignment?)
	;

assignment
	:	^(CHEVRON expression) |
		^(BOOT_CHEVRON expression)
	;
		
indexed_method_call
	:	^(INDEXED_METHOD_CALL ^(ARGUMENTS rvalue+) assignment?)
	;

parallel_assignment
	:	^(PARALLEL_ASSIGNMENT lvalue* rvalue*)
	;

variable_declaration
	:	^(VARIABLE_DECLARATION '_local'? '_dynamic'? '_global'? '_constant'? ^(DECLARATION lvalue+ arguments))
	;

throw_statement
	:	^(THROW SYMBOL? LABEL? with?)
	;

import_statement
	:	^(IMPORT IDENTIFIER+)
	;

continue_statement
	:	^(CONTINUE LABEL? with?)
	;

leave_statement
	:	^(LEAVE LABEL? with?)
	;

loopbody_statement
	:	^(LOOPBODY rvalue*)
	;

lvalue
	:	^(LVALUE '_gather'? IDENTIFIER? slot?)
	;

rvalue
	:	^(RVALUE '_scatter'? expression)
	;

return_statement
	:	^(RETURN rvalue*)
	;
	
result_statement
	:	^(RESULT rvalue*)
	;
	
for_loop
	:	^(FOR lvalue* over_loop)
	;

over_loop
	:	^(OVER expression loop)
	;

loop
	:	^(LOOP LABEL? body finally_block?)
	;

finally_block
	:	^(FINALLY body)
	;

expression
	:	(binary_expression | unary_expression | atom) (procedure_call | method_call | indexed_method_call)*
	;

binary_expression
	:	^(binary_operator expression expression)
	;

binary_operator
	:	('<<'   | '^<<'    | '+<<'     | '-<<'      | '/<<' | '*<<' | '+^<<'     | '-^<<'      | '/^<<' | '*^<<' |
		 '_or'  | '_orif'  |
		 '_xor' |
		 '_and' | '_andif' |
		 '<>'   | '='      | '_is'     | '_isnt'    | '~=' |
		 '_cf'  | '<'      | '<='      | '>'        | '>=' |
		 '+'    | '-'      |
		 '*'    | '/'      | '_div'    | '_mod'     |
		 '**')
	;

unary_expression
	:	^(unary_operator expression)
	;

unary_operator
	:	('~'    | '_not'   | SIGN_PLUS | SIGN_MINUS | '_allresults' | '_scatter')
	;

atom
	:	SELF |
		CLONE |
		SUPER |
		UNSET |
		TRUE |
		FALSE |
		MAYBE |
		THISTHREAD |
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
		i=IDENTIFIER { onIdentifier($i.text); } |
//		INTEGER |
//		FLOAT |
		NUMBER |
		CHARACTER_SEQUENCE |
		CHARACTER |
		s=SYMBOL { onSymbol($s.text); }|
		PACKAGED_IDENTIFIER |
		LABELED_GLOBAL_VARIABLE
	;

slot
	:	^(SLOT_ACCESS IDENTIFIER)
	;

simple_vector
	:	^(SIMPLE_VECTOR rvalue*)
	;

procedure_declaration
	:	^(PROCEDURE_DECLARATION '_iter'? LABEL? parameter* body)
	;

handling
	:	^(HANDLING IDENTIFIER+ with)
	;
	
block
	:	^(BLOCK LABEL? body)
	;

protect_block
	:	^(PROTECT (^(LOCKING expression))? body ^(PROTECTION body))
	;

try_block
	:	^(TRY with? body when+)
	;

when
	:	^(WHEN IDENTIFIER+ body)
	;

catch_block
	:	^(CATCH (expression | LABEL) body)
	;
	
lock_block
	:	^(LOCK expression body)
	;
if_statement
	:	^(IF expression body if_statement_elif* if_statement_else?)
	;
	
if_statement_elif
	:	^(ELIF expression body)
	;

if_statement_else
	:	^(ELSE body)
	;

with
	:	^(WITH (expression+ | '_default'))
	;
