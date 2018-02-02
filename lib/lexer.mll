{
open Lexing
open Syntax
open Position
exception SyntaxError of string
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
     | white	{next_token lexbuf; read lexbuf}
     | newline	{next_line lexbuf; read lexbuf}
     | "true"	{next_token lexbuf;TRUE}
     | "false"	{next_token lexbuf;FALSE}
     | int	{next_token lexbuf;INT (int_of_string (Lexing.lexeme lexbuf))}
     | '['	{next_token lexbuf;LBRACKET}
     | ']'	{next_token lexbuf;RBRACKET}
     | '('	{next_token lexbuf;LPAREN}
     | ')'	{next_token lexbuf;RPAREN}
     | '{'      {next_token lexbuf;LBRACE}
     | '}'	{next_token lexbuf;RBRACE}
     | ','	{next_token lexbuf;COMMA}
     | '+'	{next_token lexbuf;PLUS}
     | "++"     {next_token lexbuf;STRING_PLUS}
     | "@@"     {next_token lexbuf;LIST_PLUS}
     | '-'	{next_token lexbuf;MINUS}
     | '*'	{next_token lexbuf;MUL}
     | '/'	{next_token lexbuf;DIV}
     | ';'	{next_token lexbuf;SEMICOLON}
     | '"'      {next_token lexbuf;read_string (Buffer.create 15) lexbuf }
     | "//"	{next_token lexbuf;read_comment lexbuf}
     | ">="	{next_token lexbuf;GE}
     | "<="	{next_token lexbuf;LE}
     | "<>"	{next_token lexbuf;NEQ}
     | "!="	{next_token lexbuf;NEQ}
     | ">"	{next_token lexbuf;GT}
     | "<"	{next_token lexbuf;LT}
     | "=="	{next_token lexbuf;EQ}
     | '=' 	{next_token lexbuf;ASSIGN}
     | "->"     {next_token lexbuf;ARROW}
     | ':' 	{next_token lexbuf;COLON}
     | "fun"    {next_token lexbuf;FUNCTION}
     | "if"    	{next_token lexbuf;IF}
     | "else" 	{next_token lexbuf;ELSE}
     | "while"	{next_token lexbuf;WHILE}
     | "for"	{next_token lexbuf;FOR}
     | "in"	{next_token lexbuf;IN}
     | "return" {next_token lexbuf;RETURN}
     | symbol   {next_token lexbuf;SYMBOL (Lexing.lexeme lexbuf)}
     | eof	{lexbuf.lex_eof_reached<-true; EOF}
     
and read_string buf =
    parse
    | '"'		{next_token lexbuf;STRING (Buffer.contents buf)}
    | '\\' '\\'		{next_token lexbuf;Buffer.add_string buf "\\\\";read_string buf lexbuf}
    | '\\' 'n'		{next_token lexbuf;Buffer.add_string buf "\\n";read_string buf lexbuf}
    | '\\' 't'		{next_token lexbuf;Buffer.add_string buf "\\t";read_string buf lexbuf}
    | '\\' 'r'		{next_token lexbuf;Buffer.add_string buf "\\r";read_string buf lexbuf}
    | '\\' '"'          {next_token lexbuf;Buffer.add_string buf "\\\"";read_string buf lexbuf} 
    | [^ '"' '\\']+	{next_token lexbuf;Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf} 
    | _			{raise (SyntaxError ("illegal string:" ^ Lexing.lexeme lexbuf))}
    | eof		{raise (SyntaxError "string is not terminated")}


and read_comment =
    parse
    | '\n'	{next_line lexbuf;read lexbuf}
    | eof	{lexbuf.lex_eof_reached<-true;EOF}
    | _ 	{next_token lexbuf;read_comment lexbuf}
