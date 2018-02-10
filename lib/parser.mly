%{
open Core
open Syntax
%}

%token <int> INT
%token <Syntax.symbol> SYMBOL
%token <string> STRING
%token TRUE
%token FALSE
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token PLUS
%token STRING_PLUS
%token LIST_PLUS
%token MINUS
%token MUL
%token DIV
%token SEMICOLON
%token GE
%token LE
%token NEQ
%token GT
%token LT
%token EQ
%token ASSIGN
%token FUNCTION
%token IF
%token ELSE
%token WHILE
%token FOR
%token IN
%token RETURN
%token BREAK
%token CONTINUE
%token COMMA
%token ARROW
%token COLON
%token EOF
%left GE LE NEQ EQ GT LT STRING_PLUS LIST_PLUS
%left PLUS MINUS
%left MUL DIV
%start <Syntax.statements> prog
%%

prog:
    | v = statements_ {v}
;
statements_:
    | v = statements; EOF
      {v}

statements:
    | v = statement; vl = statements; 
      {v::vl}
    | LPAREN; v = statements; RPAREN
      {v}
    | {[]}

;
statement:
    | s = SYMBOL; ASSIGN; v = value; SEMICOLON
      {Assignment {v=(s, v);symbol_pos={start_pos=$startpos;end_pos=$endpos(s)}}}
    | IF; LPAREN; v = value; RPAREN; LBRACE; s1 = statements; RBRACE; ELSE; LBRACE; s2 = statements; RBRACE
      {If (v, s1, Some s2)}
    | IF; LPAREN; v = value; RPAREN; LBRACE; s = statements; RBRACE
      {If (v, s, None)}
    | FOR; LPAREN; s = SYMBOL; IN; v = value; RPAREN; LBRACE; s1 = statements; RBRACE
      {For {v=(s, v, s1);symbol_pos={start_pos=$startpos(s);end_pos=$endpos(s)}}}
    | WHILE; LPAREN; v = value; RPAREN; LBRACE; s = statements; RBRACE
      {While (v, s)}
    | FUNCTION; n = SYMBOL; LPAREN; s = symbol_list; RPAREN;COLON; t = fun_type; LBRACE; s1 = statements; RBRACE
      {Function (n, s, s1, t)}
    | RETURN; v = value; SEMICOLON
      {Return v}
    | BREAK; SEMICOLON
      {Break {pos={start_pos=$startpos;end_pos=$endpos}}}
    | CONTINUE; SEMICOLON
      {Continue {pos={start_pos=$startpos;end_pos=$endpos}}}
    | v = value; SEMICOLON
      {Value v}
      
;
fun_type:
    | s = SYMBOL; ARROW; sl = fun_type
      {s :: sl}
    | s = SYMBOL; s1=SYMBOL
      {[s^" "^s1]}
    | s = SYMBOL
      {[s]}
symbol_list:
    | s = SYMBOL; COMMA; sl = symbol_list
      {s :: sl}
    | s = SYMBOL
      {[s]}
    | {[]}
;      
value_list:
    |  v = value; COMMA; vl = value_list
      {v :: vl}
    | v = value
      {[v]}
    |  {[]}

;
basic_value:
    | v = INT
      {Num v}
    | v = STRING
      {String v}
    | FALSE
      {Bool false}
    | TRUE
      {Bool true}
    | LBRACKET; vl = value_list; RBRACKET
      {List vl}
    | v = SYMBOL
      {Symbol v}
;      
op_value:
    | v1 = value; PLUS; v2 = value
      {Op (v1, Plus, v2)}
    | v1 = value; STRING_PLUS; v2 = value
      {Op (v1, String_plus, v2)}
    | v1 = value; LIST_PLUS; v2 = value
      {Op (v1, List_plus, v2)}
    | v1 = value; MINUS; v2 = value
      {Op (v1, Minus, v2)}
    | MINUS; v = value
      {Op (Basic_value {v=(Num 0);pos={start_pos=$startpos;end_pos=$endpos}}, Minus, v)}
    | v1 = value; MUL; v2 = value
      {Op (v1, Mul, v2)}
    | v1 = value; DIV; v2 = value
      {Op (v1, Div, v2)}
    | v1 = value; GT; v2 = value
      {Op (v1, Gt, v2)}
    | v1 = value; LT; v2 = value
      {Op (v1, Lt, v2)}
    | v1 = value; GE; v2 = value
      {Op (v1, Ge, v2)}
    | v1 = value; LE; v2 = value
      {Op (v1, Le, v2)}
    | v1 = value; NEQ; v2 = value
      {Op (v1, Neq, v2)}
    | v1 = value; EQ; v2 = value
      {Op (v1, Eq, v2)}
;      
value:
    | s = SYMBOL; LPAREN; vl = value_list; RPAREN
      {Fun_call {v=(s, vl);pos={start_pos=$startpos;end_pos=$endpos}}}
    | v = op_value
      {Op_value {v;pos={start_pos=$startpos;end_pos=$endpos}}}
    | v = basic_value
      {Basic_value {v;pos={start_pos=$startpos;end_pos=$endpos}}}
    | LPAREN; v = value; RPAREN
      {v}
;      