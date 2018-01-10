{
open Lexing
open Syntax
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = 0;
               pos_lnum = pos.pos_lnum + 1
    }
}


let int = ['1'-'9'] ['0'-'9']* | '0'

(* part 2 *)
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

(* part 3 *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let symbol = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*



rule read =
     parse
     | white	{read lexbuf}
     | newline	{next_line lexbuf; read lexbuf}
     | "true"	{TRUE}
     | "false"	{FALSE}
     | int	{INT (int_of_string (Lexing.lexeme lexbuf))}
     | '['	{LBRACKET}
     | ']'	{RBRACKET}
     | '('	{LPAREN}
     | ')'	{RPAREN}
     | '{'      {LBRACE}
     | '}'	{RBRACE}
     | ','	{COMMA}
     | '+'	{PLUS}
     | "++"     {STRING_PLUS}
     | "@@"     {LIST_PLUS}
     | '-'	{MINUS}
     | '*'	{MUL}
     | '/'	{DIV}
     | ';'	{SEMICOLON}
     | '"'      {read_string (Buffer.create 15) lexbuf}
     | ">="	{GE}
     | "<="	{LE}
     | "<>"	{NEQ}
     | "!="	{NEQ}
     | ">"	{GT}
     | "<"	{LT}
     | "=="	{EQ}
     | '=' 	{ASSIGN}
     | "fun"    {FUNCTION}
     | "if"    	{IF}
     | "else" 	{ELSE}
     | "while"	{WHILE}
     | "for"	{FOR}
     | "in"	{IN}
     | "return" {RETURN}
     | symbol   {SYMBOL (Lexing.lexeme lexbuf)}
     | eof	{lexbuf.lex_eof_reached<-true; EOF}
     
and read_string buf =
    parse
    | '"'		{STRING (Buffer.contents buf)}
    | '\\' '\\'		{Buffer.add_char buf '\\';read_string buf lexbuf}
    | '\\' 'n'		{Buffer.add_char buf '\n';read_string buf lexbuf}
    | '\\' 't'		{Buffer.add_char buf '\t';read_string buf lexbuf}
    | '\\' 'r'		{Buffer.add_char buf '\r';read_string buf lexbuf}
    | [^ '"' '\\']+	{Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf} 
    | _			{raise (SyntaxError ("illegal string:" ^ Lexing.lexeme lexbuf))}
    | eof		{raise (SyntaxError "string is not terminated")}