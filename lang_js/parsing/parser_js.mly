/* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 */
%{
(* 
 * src: ocamlyaccified from Marcel Laverdet 'fbjs2' via emacs macros, itself
 * extracted from the official ECMAscript specification at:
 *  http://www.ecma-international.org/publications/standards/ecma-262.htm
 * 
 * see also http://en.wikipedia.org/wiki/ECMAScript_syntax
 *)

open Common

open Ast_js

let e x = (x, Ast_js.noType())
let bop op a b c = e(B(a, (op, b), c))
let uop op a b = e(U((op,a), b))
%}

/*(*************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(* the comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_js.info> TCommentSpace TCommentNewline   TComment

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with a value *)*/
%token<string * Ast_js.info> T_NUMBER
%token<string * Ast_js.info> T_IDENTIFIER 
%token<string * Ast_js.info> T_STRING
%token<string * Ast_js.info> T_REGEX

/*(* keywords tokens *)*/
%token <Ast_js.info> 
 T_FUNCTION T_IF T_IN T_INSTANCEOF T_RETURN T_SWITCH T_THIS T_THROW T_TRY
 T_VAR T_WHILE T_WITH T_CONST T_NULL T_FALSE T_TRUE
 T_BREAK T_CASE T_CATCH T_CONTINUE T_DEFAULT T_DO T_FINALLY T_FOR

%token <Ast_js.info> T_ELSE

%token <Ast_js.info> T_NEW 

/*(* syntax *)*/
%token <Ast_js.info>  
 T_LCURLY T_RCURLY
 T_LPAREN T_RPAREN
 T_LBRACKET T_RBRACKET
 T_SEMICOLON 
 T_COMMA
 T_PERIOD

/*(* operators *)*/
%token <Ast_js.info>  
 T_RSHIFT3_ASSIGN T_RSHIFT_ASSIGN T_LSHIFT_ASSIGN 
 T_BIT_XOR_ASSIGN T_BIT_OR_ASSIGN T_BIT_AND_ASSIGN T_MOD_ASSIGN T_DIV_ASSIGN
 T_MULT_ASSIGN T_MINUS_ASSIGN T_PLUS_ASSIGN T_ASSIGN

%token <Ast_js.info>
 T_PLING T_COLON
 T_OR
 T_AND
 T_BIT_OR
 T_BIT_XOR
 T_BIT_AND
 T_EQUAL T_NOT_EQUAL T_STRICT_EQUAL T_STRICT_NOT_EQUAL
 T_LESS_THAN_EQUAL T_GREATER_THAN_EQUAL T_LESS_THAN T_GREATER_THAN 
 T_IN T_INSTANCEOF
 T_LSHIFT T_RSHIFT T_RSHIFT3
 T_PLUS T_MINUS
 T_DIV T_MULT T_MOD
 T_NOT T_BIT_NOT T_INCR T_DECR T_DELETE T_TYPEOF T_VOID

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/

%token <Ast_js.info> T_VIRTUAL_SEMICOLON

/*(* classic *)*/
%token <Ast_js.info> TUnknown
%token <Ast_js.info> EOF

/*(*-----------------------------------------*)*/
/*(* priorities *)*/
/*(*-----------------------------------------*)*/

/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc SHIFTHERE

/*(* Special if / else associativity*)*/
%nonassoc p_IF
%nonassoc T_ELSE

%nonassoc p_POSTFIX

%right 
 T_RSHIFT3_ASSIGN T_RSHIFT_ASSIGN T_LSHIFT_ASSIGN 
 T_BIT_XOR_ASSIGN T_BIT_OR_ASSIGN T_BIT_AND_ASSIGN T_MOD_ASSIGN T_DIV_ASSIGN
 T_MULT_ASSIGN T_MINUS_ASSIGN T_PLUS_ASSIGN T_ASSIGN

%left T_OR
%left T_AND
%left T_BIT_OR
%left T_BIT_XOR
%left T_BIT_AND
%left T_EQUAL T_NOT_EQUAL T_STRICT_EQUAL T_STRICT_NOT_EQUAL
%left 
 T_LESS_THAN_EQUAL T_GREATER_THAN_EQUAL T_LESS_THAN T_GREATER_THAN 
 T_IN T_INSTANCEOF
%left T_LSHIFT T_RSHIFT T_RSHIFT3
%left T_PLUS T_MINUS
%left T_DIV T_MULT T_MOD
%right T_NOT T_BIT_NOT T_INCR T_DECR T_DELETE T_TYPEOF T_VOID

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <Ast_js.toplevel list> main

%%

/*(*************************************************************************)*/
/*(* Toplevel *)*/
/*(*************************************************************************)*/

main: program EOF { $1 ++ [FinalDef $2] }

program: statement_list { $1 }

source_element:
 | statement            { St $1 }
 | function_declaration { FunDecl $1 }

/*(*************************************************************************)*/
/*(* statement *)*/
/*(*************************************************************************)*/

statement:
 | block                { $1 }
 | variable_statement   { $1 }
 | empty_statement      { $1 }
 | expression_statement { $1 }
 | if_statement         { $1 }
 | iteration_statement  { $1 }
 | continue_statement   { $1 }
 | break_statement      { $1 }
 | return_statement     { $1 }
 | with_statement       { $1 }
 | labelled_statement   { $1 }
 | switch_statement     { $1 }
 | throw_statement      { $1 }
 | try_statement        { $1 }


block:
 | T_LCURLY statement_list T_RCURLY { Block ($1, $2, $3) }
 | T_LCURLY T_RCURLY                { Block ($1, [], $2) }


variable_statement:
 | T_VAR variable_declaration_list semicolon  { Variable ($1, $2, $3) }
 /*(* pad: not in original grammar *)*/
 | T_CONST variable_declaration_list semicolon { Const ($1, $2, $3) }

variable_declaration:
 | identifier initializeur { $1, Some $2 }
 | identifier { $1, None }

initializeur:
 | T_ASSIGN assignment_expression { $1, $2 }


empty_statement:
 | semicolon { Nop $1 }

expression_statement:
 | expression_no_statement semicolon { Expr ($1, $2) }


if_statement:
 | T_IF T_LPAREN expression T_RPAREN statement T_ELSE statement 
     { If ($1, ($2, $3, $4), $5, Some ($6, $7)) }
 | T_IF T_LPAREN expression T_RPAREN statement %prec p_IF 
     { If ($1, ($2, $3, $4), $5, None) }


iteration_statement:
 | T_DO statement T_WHILE T_LPAREN expression T_RPAREN semicolon 
     { Do ($1, $2, $3, ($4, $5, $6), $7) }
 | T_WHILE T_LPAREN expression T_RPAREN statement 
     { While ($1, ($2, $3, $4), $5) }
 | T_FOR T_LPAREN 
     expression_no_in_opt T_SEMICOLON 
     expression_opt T_SEMICOLON 
     expression_opt 
     T_RPAREN statement 
     { For ($1, $2, $3 +>Common.fmap (fun x -> LHS x), $4, $5, $6, $7, $8, $9) }
 | T_FOR T_LPAREN 
     T_VAR variable_declaration_list_no_in T_SEMICOLON 
     expression_opt T_SEMICOLON 
     expression_opt 
     T_RPAREN statement 
     { For ($1, $2, Some (Vars ($3, $4)), $5, $6, $7, $8, $9, $10) }
 | T_FOR T_LPAREN left_hand_side_expression T_IN expression T_RPAREN statement
     { ForIn ($1, $2, LHS $3, $4, $5, $6, $7) }
 | T_FOR 
     T_LPAREN T_VAR variable_declaration_list_no_in T_IN expression T_RPAREN 
     statement 
     { ForIn ($1, $2, Vars ($3, $4), $5, $6, $7, $8) }

variable_declaration_no_in:
 | identifier initializer_no_in { $1, Some $2 }
 | identifier { $1, None }

initializer_no_in:
 | T_ASSIGN assignment_expression_no_in { $1, $2 }


continue_statement:
 | T_CONTINUE identifier semicolon { Continue ($1, Some $2, $3) }
 | T_CONTINUE semicolon            { Continue ($1, None, $2) }


break_statement:
 | T_BREAK identifier semicolon { Break ($1, Some $2, $3) }
 | T_BREAK semicolon            { Break ($1, None, $2) }


return_statement:
 | T_RETURN expression semicolon { Return ($1, Some $2, $3) }
 | T_RETURN semicolon            { Return ($1, None, $2) }


with_statement:
 | T_WITH T_LPAREN expression T_RPAREN statement { With ($1, ($2, $3, $4), $5) }


switch_statement:
 | T_SWITCH T_LPAREN expression T_RPAREN case_block { Switch ($1, ($2, $3, $4), $5) }



labelled_statement:
 | identifier T_COLON statement { Labeled ($1, $2, $3) }


throw_statement:
 | T_THROW expression semicolon { Throw ($1, $2, $3) }


try_statement:
 | T_TRY block catch         { Try ($1, $2, Some $3, None)  }
 | T_TRY block       finally { Try ($1, $2, None, Some $3) }
 | T_TRY block catch finally { Try ($1, $2, Some $3, Some $4) }


catch:
 | T_CATCH T_LPAREN identifier T_RPAREN block { $1, ($2, $3, $4), $5 }


finally:
 | T_FINALLY block { $1, $2 }

/*(*----------------------------*)*/
/*(* auxillary statements *)*/
/*(*----------------------------*)*/

case_block:
 | T_LCURLY case_clauses_opt T_RCURLY 
     { ($1, $2, $3) }
 | T_LCURLY case_clauses_opt default_clause case_clauses_opt T_RCURLY 
     { ($1, $2 ++ [$3] ++ $4, $5) }


case_clause:
 | T_CASE expression T_COLON statement_list { Case ($1, $2, $3, $4) }
 | T_CASE expression T_COLON { Case ($1, $2, $3, []) }


default_clause:
 | T_DEFAULT T_COLON { Default ($1, $2, [])}
 | T_DEFAULT T_COLON statement_list { Default ($1, $2, $3) }

/*(*************************************************************************)*/
/*(* function declaration *)*/
/*(*************************************************************************)*/

function_declaration:
 | T_FUNCTION identifier T_LPAREN formal_parameter_list T_RPAREN 
     T_LCURLY function_body T_RCURLY 
     { $1, Some $2, ($3, $4, $5), ($6, $7, $8) }
 | T_FUNCTION identifier T_LPAREN T_RPAREN 
     T_LCURLY function_body T_RCURLY 
     { $1, Some $2, ($3, [], $4), ($5, $6, $7) }


function_expression:
 | T_FUNCTION identifier T_LPAREN formal_parameter_list T_RPAREN 
     T_LCURLY function_body T_RCURLY 
     { e(Function ($1, Some $2, ($3, $4, $5), ($6, $7, $8))) }
 | T_FUNCTION identifier T_LPAREN T_RPAREN 
     T_LCURLY function_body T_RCURLY 
     { e(Function ($1, Some $2, ($3, [], $4), ($5, $6, $7))) }
 | T_FUNCTION T_LPAREN formal_parameter_list T_RPAREN 
     T_LCURLY function_body T_RCURLY 
     { e(Function ($1, None, ($2, $3, $4), ($5, $6, $7))) }
 | T_FUNCTION T_LPAREN T_RPAREN 
     T_LCURLY function_body T_RCURLY 
     { e(Function ($1, None, ($2, [], $3), ($4, $5, $6))) }

formal_parameter_list:
 | identifier                                { [Left $1] }
 | formal_parameter_list T_COMMA identifier  { $1 ++ [Right $2; Left $3] }

function_body:
 | /*(* empty *)*/ { [] }
 | statement_list  { $1 }

/*(*************************************************************************)*/
/*(* expression *)*/
/*(*************************************************************************)*/

expression:
 | assignment_expression { $1 }
 | expression T_COMMA assignment_expression { e(Seq ($1, $2, $3)) }

assignment_expression:
 | conditional_expression { $1 }
 | left_hand_side_expression assignment_operator assignment_expression 
     { e(Assign ($1, $2, $3)) }

assignment_operator:
 | T_ASSIGN         { A_eq , $1 }
 | T_MULT_ASSIGN    { A_mul, $1 }
 | T_DIV_ASSIGN     { A_div, $1 }
 | T_MOD_ASSIGN     { A_mod, $1 }
 | T_PLUS_ASSIGN    { A_add, $1 }
 | T_MINUS_ASSIGN   { A_sub, $1 }
 | T_LSHIFT_ASSIGN  { A_lsl, $1 }
 | T_RSHIFT_ASSIGN  { A_lsr, $1 }
 | T_RSHIFT3_ASSIGN { A_asr, $1 }
 | T_BIT_AND_ASSIGN { A_and, $1 }
 | T_BIT_XOR_ASSIGN { A_xor, $1 }
 | T_BIT_OR_ASSIGN  { A_or , $1 }

left_hand_side_expression:
 | new_expression  { $1 }
 | call_expression { $1 }

conditional_expression:
 | post_in_expression { $1 }
 | post_in_expression 
     T_PLING assignment_expression 
     T_COLON assignment_expression 
     { e(Conditional ($1, $2, $3, $4, $5)) }

post_in_expression:
 | pre_in_expression { $1 }
 | post_in_expression T_LESS_THAN post_in_expression          { bop B_lt $1 $2 $3 }
 | post_in_expression T_GREATER_THAN post_in_expression       { bop B_gt $1 $2 $3 }
 | post_in_expression T_LESS_THAN_EQUAL post_in_expression    { bop B_le $1 $2 $3 }
 | post_in_expression T_GREATER_THAN_EQUAL post_in_expression { bop B_ge $1 $2 $3 }
 | post_in_expression T_INSTANCEOF post_in_expression         { bop B_instanceof $1 $2 $3 }
 | post_in_expression T_IN post_in_expression                 { bop B_in $1 $2 $3 }
 | post_in_expression T_EQUAL post_in_expression              { bop B_equal $1 $2 $3 }
 | post_in_expression T_NOT_EQUAL post_in_expression          { bop B_notequal $1 $2 $3 }
 | post_in_expression T_STRICT_EQUAL post_in_expression       { bop B_physequal $1 $2 $3 }
 | post_in_expression T_STRICT_NOT_EQUAL post_in_expression   { bop B_physnotequal $1 $2 $3 }
 | post_in_expression T_BIT_AND post_in_expression            { bop B_bitand $1 $2 $3 }
 | post_in_expression T_BIT_XOR post_in_expression            { bop B_bitxor $1 $2 $3 }
 | post_in_expression T_BIT_OR post_in_expression             { bop B_bitor $1 $2 $3 }
 | post_in_expression T_AND post_in_expression                { bop B_and $1 $2 $3 }
 | post_in_expression T_OR post_in_expression                 { bop B_or $1 $2 $3 }           

pre_in_expression:
 | left_hand_side_expression                     { $1 }
 | pre_in_expression T_INCR %prec p_POSTFIX      { uop U_post_increment $2 $1 }
 | pre_in_expression T_DECR %prec p_POSTFIX      { uop U_post_decrement $2 $1 }
 | T_DELETE pre_in_expression                    { uop U_delete $1 $2 }
 | T_VOID pre_in_expression                      { uop U_void $1 $2 }
 | T_TYPEOF pre_in_expression                    { uop U_typeof $1 $2 }
 | T_INCR pre_in_expression                      { uop U_pre_increment $1 $2 }
 | T_DECR pre_in_expression                      { uop U_pre_decrement $1 $2 }
 | T_PLUS pre_in_expression                      { uop U_plus $1 $2 }
 | T_MINUS pre_in_expression                     { uop U_minus $1 $2}
 | T_BIT_NOT pre_in_expression                   { uop U_bitnot $1 $2 }
 | T_NOT pre_in_expression                       { uop U_not $1 $2 }

 | pre_in_expression T_MULT pre_in_expression    { bop B_mul $1 $2 $3 }
 | pre_in_expression T_DIV pre_in_expression     { bop B_div $1 $2 $3 }
 | pre_in_expression T_MOD pre_in_expression     { bop B_mod $1 $2 $3 }
 | pre_in_expression T_PLUS pre_in_expression    { bop B_add $1 $2 $3 }
 | pre_in_expression T_MINUS pre_in_expression   { bop B_sub $1 $2 $3 }
 | pre_in_expression T_LSHIFT pre_in_expression  { bop B_lsl $1 $2 $3 }
 | pre_in_expression T_RSHIFT pre_in_expression  { bop B_lsr $1 $2 $3 }
 | pre_in_expression T_RSHIFT3 pre_in_expression { bop B_asr $1 $2 $3 }         

call_expression:
 | member_expression arguments                      { e(Apply ($1, $2)) }
 | call_expression arguments                        { e(Apply ($1, $2)) }
 | call_expression T_LBRACKET expression T_RBRACKET { e(Bracket($1, ($2, $3, $4))) }
 | call_expression T_PERIOD identifier              { e(Period ($1, $2, $3)) }

new_expression:
 | member_expression    { $1 }
 | T_NEW new_expression { uop U_new $1 $2 }

member_expression:
 | primary_expression                                 { $1 }
 | member_expression T_LBRACKET expression T_RBRACKET { e(Bracket($1, ($2, $3, $4))) }
 | member_expression T_PERIOD identifier              { e(Period ($1, $2, $3)) }
 | T_NEW member_expression arguments                  
     { e(Apply(uop U_new $1 $2, $3)) }

primary_expression:
 | primary_expression_no_statement { $1 }
 | object_literal                  { e(Object $1) }
 | function_expression             { $1 }

primary_expression_no_statement:
 | T_THIS          { e((This $1)) }
 | identifier      { e((V $1)) }

 | null_literal    { e(L(Null $1)) }
 | boolean_literal { e(L(Bool $1)) }
 | numeric_literal { e(L(Num $1)) }
 | string_literal  { e(L(String $1)) }
 /*(* marcel: this isn't an expansion of literal in ECMA-262... mistake? *)*/
 | regex_literal                { e(L(Regexp $1)) }
 | array_literal                { e($1) }
 | T_LPAREN expression T_RPAREN { e(Paren ($1, $2, $3)) }

/*(*----------------------------*)*/
/*(* no in *)*/
/*(*----------------------------*)*/
expression_no_in:
 | assignment_expression_no_in { $1 }
 | expression_no_in T_COMMA assignment_expression_no_in { e(Seq ($1, $2, $3)) }

assignment_expression_no_in:
 | conditional_expression_no_in { $1 }
 | left_hand_side_expression assignment_operator assignment_expression_no_in 
     { e(Assign ($1, $2, $3)) }

conditional_expression_no_in:
 | post_in_expression_no_in { $1 }
 | post_in_expression_no_in 
     T_PLING assignment_expression_no_in 
     T_COLON assignment_expression_no_in 
     { e(Conditional ($1, $2, $3, $4, $5)) }

post_in_expression_no_in:
 | pre_in_expression { $1 }
 | post_in_expression_no_in T_LESS_THAN post_in_expression          { bop B_lt $1 $2 $3 }           
 | post_in_expression_no_in T_GREATER_THAN post_in_expression       { bop B_gt $1 $2 $3 }           
 | post_in_expression_no_in T_LESS_THAN_EQUAL post_in_expression    { bop B_le $1 $2 $3 }           
 | post_in_expression_no_in T_GREATER_THAN_EQUAL post_in_expression { bop B_ge $1 $2 $3 }           
 | post_in_expression_no_in T_INSTANCEOF post_in_expression         { bop B_instanceof $1 $2 $3 }   
 /*(* no T_IN case *)*/
 | post_in_expression_no_in T_EQUAL post_in_expression              { bop B_equal $1 $2 $3 }        
 | post_in_expression_no_in T_NOT_EQUAL post_in_expression          { bop B_notequal $1 $2 $3 }     
 | post_in_expression_no_in T_STRICT_EQUAL post_in_expression       { bop B_physequal $1 $2 $3 }    
 | post_in_expression_no_in T_STRICT_NOT_EQUAL post_in_expression   { bop B_physnotequal $1 $2 $3 } 
 | post_in_expression_no_in T_BIT_AND post_in_expression            { bop B_bitand $1 $2 $3 }       
 | post_in_expression_no_in T_BIT_XOR post_in_expression            { bop B_bitxor $1 $2 $3 }       
 | post_in_expression_no_in T_BIT_OR post_in_expression             { bop B_bitor $1 $2 $3 }        
 | post_in_expression_no_in T_AND post_in_expression                { bop B_and $1 $2 $3 }          
 | post_in_expression_no_in T_OR post_in_expression                 { bop B_or $1 $2 $3 }           
                                                                    

/*(*----------------------------*)*/
/*(* (no statement)*)*/
/*(*----------------------------*)*/
expression_no_statement:
 | assignment_expression_no_statement { $1 }
 | expression_no_statement T_COMMA assignment_expression { e(Seq ($1, $2, $3)) }

assignment_expression_no_statement:
 | conditional_expression_no_statement { $1 }
 | left_hand_side_expression_no_statement assignment_operator assignment_expression 
     { e(Assign ($1, $2, $3)) }

conditional_expression_no_statement:
 | post_in_expression_no_statement { $1 }
 | post_in_expression_no_statement 
     T_PLING assignment_expression 
     T_COLON assignment_expression 
     { e(Conditional ($1, $2, $3, $4, $5)) }



post_in_expression_no_statement:
 | pre_in_expression_no_statement { $1 }
 | post_in_expression_no_statement T_LESS_THAN post_in_expression          { bop B_lt $1 $2 $3 }           
 | post_in_expression_no_statement T_GREATER_THAN post_in_expression       { bop B_gt $1 $2 $3 }           
 | post_in_expression_no_statement T_LESS_THAN_EQUAL post_in_expression    { bop B_le $1 $2 $3 }           
 | post_in_expression_no_statement T_GREATER_THAN_EQUAL post_in_expression { bop B_ge $1 $2 $3 }           
 | post_in_expression_no_statement T_INSTANCEOF post_in_expression         { bop B_instanceof $1 $2 $3 }   
 | post_in_expression_no_statement T_IN post_in_expression                 { bop B_in $1 $2 $3 }           
 | post_in_expression_no_statement T_EQUAL post_in_expression              { bop B_equal $1 $2 $3 }        
 | post_in_expression_no_statement T_NOT_EQUAL post_in_expression          { bop B_notequal $1 $2 $3 }     
 | post_in_expression_no_statement T_STRICT_EQUAL post_in_expression       { bop B_physequal $1 $2 $3 }    
 | post_in_expression_no_statement T_STRICT_NOT_EQUAL post_in_expression   { bop B_physnotequal $1 $2 $3 } 
 | post_in_expression_no_statement T_BIT_AND post_in_expression            { bop B_bitand $1 $2 $3 }       
 | post_in_expression_no_statement T_BIT_XOR post_in_expression            { bop B_bitxor $1 $2 $3 }       
 | post_in_expression_no_statement T_BIT_OR post_in_expression             { bop B_bitor $1 $2 $3 }        
 | post_in_expression_no_statement T_AND post_in_expression                { bop B_and $1 $2 $3 }          
 | post_in_expression_no_statement T_OR post_in_expression                 { bop B_or $1 $2 $3 }           


pre_in_expression_no_statement:
 | left_hand_side_expression_no_statement                     { $1 }
 | pre_in_expression_no_statement T_INCR                      { uop U_post_increment $2 $1 } 
 | pre_in_expression_no_statement T_DECR                      { uop U_post_decrement $2 $1 } 
 | T_DELETE pre_in_expression                                 { uop U_delete $1 $2 }         
 | T_VOID pre_in_expression                                   { uop U_void $1 $2 }           
 | T_TYPEOF pre_in_expression                                 { uop U_typeof $1 $2 }         
 | T_INCR pre_in_expression                                   { uop U_pre_increment $1 $2 }  
 | T_DECR pre_in_expression                                   { uop U_pre_decrement $1 $2 }  
 | T_PLUS pre_in_expression                                   { uop U_plus $1 $2 }           
 | T_MINUS pre_in_expression                                  { uop U_minus $1 $2}           
 | T_BIT_NOT pre_in_expression                                { uop U_bitnot $1 $2 }         
 | T_NOT pre_in_expression                                    { uop U_not $1 $2 }            
                                                                                             
 | pre_in_expression_no_statement T_MULT pre_in_expression    { bop B_mul $1 $2 $3 }         
 | pre_in_expression_no_statement T_DIV pre_in_expression     { bop B_div $1 $2 $3 }         
 | pre_in_expression_no_statement T_MOD pre_in_expression     { bop B_mod $1 $2 $3 }         
 | pre_in_expression_no_statement T_PLUS pre_in_expression    { bop B_add $1 $2 $3 }         
 | pre_in_expression_no_statement T_MINUS pre_in_expression   { bop B_sub $1 $2 $3 }         
 | pre_in_expression_no_statement T_LSHIFT pre_in_expression  { bop B_lsl $1 $2 $3 }         
 | pre_in_expression_no_statement T_RSHIFT pre_in_expression  { bop B_lsr $1 $2 $3 }         
 | pre_in_expression_no_statement T_RSHIFT3 pre_in_expression { bop B_asr $1 $2 $3 }         

left_hand_side_expression_no_statement:
 | new_expression_no_statement { $1 }
 | call_expression_no_statement { $1 }

new_expression_no_statement:
 | member_expression_no_statement { $1 }
 | T_NEW new_expression { uop U_new $1 $2 }

call_expression_no_statement:
 | member_expression_no_statement arguments                      { e(Apply ($1, $2)) }
 | call_expression_no_statement arguments                        { e(Apply ($1, $2)) }
 | call_expression_no_statement T_LBRACKET expression T_RBRACKET { e(Bracket($1, ($2, $3, $4))) }
 | call_expression_no_statement T_PERIOD identifier              { e(Period ($1, $2, $3)) }

member_expression_no_statement:
 | primary_expression_no_statement                                 { $1 }
 | member_expression_no_statement T_LBRACKET expression T_RBRACKET { e(Bracket($1, ($2, $3, $4))) }
 | member_expression_no_statement T_PERIOD identifier              { e(Period ($1, $2, $3)) }
 | T_NEW member_expression arguments                               { e(Apply(uop U_new $1 $2, $3)) }

/*(*----------------------------*)*/
/*(* scalar *)*/
/*(*----------------------------*)*/
null_literal:
 | T_NULL { $1 }

boolean_literal:
 | T_TRUE  { true, $1 }
 | T_FALSE { false, $1 }

numeric_literal:
 | T_NUMBER { $1 }

regex_literal:
 | T_REGEX { $1 }

string_literal:
 | T_STRING { $1 }

/*(*----------------------------*)*/
/*(* array *)*/
/*(*----------------------------*)*/

array_literal:
 | T_LBRACKET elison T_RBRACKET              { Array($1, $2, $3) }
 | T_LBRACKET        T_RBRACKET              { Array($1, [], $2) }
 | T_LBRACKET element_list T_RBRACKET        { Array($1, $2, $3) }
 | T_LBRACKET element_list elison T_RBRACKET { Array($1, $2 ++ $3, $4) }


element_list:
 | elison   assignment_expression { $1 ++ [Left $2] }
 |          assignment_expression { [Left $1] }
 | element_list   elison   assignment_expression { $1 ++ $2 ++ [Left $3] }



object_literal:
 | T_LCURLY T_RCURLY 
     { ($1, [], $2) }
 | T_LCURLY property_name_and_value_list T_VIRTUAL_SEMICOLON T_RCURLY 
     { ($1, $2, $4) }




property_name_and_value_list:
 | property_name T_COLON assignment_expression 
     { [Left ($1, $2, $3)] }
 | property_name_and_value_list T_COMMA 
     property_name T_COLON assignment_expression
     { $1 ++ [Right $2; Left ($3, $4, $5)] }

/*(*----------------------------*)*/
/*(* variable *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(* function call *)*/
/*(*----------------------------*)*/

arguments:
 | T_LPAREN               T_RPAREN { ($1, [], $2) }
 | T_LPAREN argument_list T_RPAREN { ($1, $2, $3) }

argument_list:
 | assignment_expression 
     { [Left $1] }
 | argument_list T_COMMA assignment_expression 
     { $1 ++ [Right $2; Left $3] }

/*(*----------------------------*)*/
/*(* auxillary bis *)*/
/*(*----------------------------*)*/

/*(*************************************************************************)*/
/*(* Entities, names *)*/
/*(*************************************************************************)*/
identifier:
 | T_IDENTIFIER { $1 }

property_name:
 | identifier      { PN_String $1 }
 | string_literal  { PN_String $1 }
 | numeric_literal { PN_Num $1 }

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

semicolon:
 | T_SEMICOLON         { Some $1 }
 | T_VIRTUAL_SEMICOLON { None }

elison:
 | T_COMMA { [Right $1] }
 | elison T_COMMA { $1 ++ [Right $2] }



statement_list:
 | source_element { [$1] }
 | statement_list source_element { $1 ++ [$2] }

case_clauses:
 | case_clause { [$1] }
 | case_clauses case_clause { $1 ++ [$2] }



variable_declaration_list:
 | variable_declaration 
     { [Left $1]  }
 | variable_declaration_list T_COMMA variable_declaration 
     { $1 ++ [Right $2; Left $3] }

variable_declaration_list_no_in:
 | variable_declaration_no_in 
     { [Left $1] }
 | variable_declaration_list_no_in T_COMMA variable_declaration_no_in 
     { $1 ++ [Right $2; Left $3] }



expression_opt:
 | /*(* empty *)*/ { None }
 | expression      { Some $1 }

expression_no_in_opt:
 | /*(* empty *)*/  { None }
 | expression_no_in { Some $1 }

case_clauses_opt:
 | /*(* empty *)*/ { [] }
 | case_clauses    { $1 }
