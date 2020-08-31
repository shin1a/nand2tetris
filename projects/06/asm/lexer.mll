{ 
    open Support 
    open Parser (* open parser.ml *) 

    let text = Lexing.lexeme

    
    let str_buf     = ref (Bytes.create 1024)
    let str_end     = ref 0 
    let add_char c  = Bytes.set !str_buf !str_end c; incr str_end  
    let get_str ()  = Bytes.sub !str_buf 0 !str_end 

}

let digit = ['0'-'9']
let space = [' ' '\t'] 
let init  = ['a'-'z' 'A'-'Z' '_' ':' '.' '$']
let var   = init (init | digit )* 
rule token = parse
    | space                 { token lexbuf  }
    | '@'                   { AT } 
    | '='                   { EQ } 
    | '|'                   { OR } 
    | '&'                   { AND } 
    | '!'                   { BANG } 
    | ':'                   { COLON } 
    | ';'                   { SEMI } 
    | '('                   { LPAREN } 
    | ')'                   { RPAREN } 
    | 'A'                   { A   } 
    | 'M'                   { M   } 
    | 'D'                   { D   }
    | "MD"                  { MD  } 
    | "AM"                  { AM  } 
    | "AD"                  { AD  } 
    | "AMD"                 { AMD }
    | "JGT"                 { JGT } 
    | "JGE"                 { JGE } 
    | "JEQ"                 { JEQ } 
    | "JLT"                 { JLT } 
    | "JNE"                 { JNE } 
    | "JLE"                 { JLE } 
    | "JMP"                 { JMP } 
    | '+'                   { PLUS } 
    | '-'                   { MINUS } 
    | '*'                   { MUL } 
    | '\n' (space* ['\n'])* { NEWLINE } 
    | "//"                  { comment lexbuf } 
    | var                   { VAR (text lexbuf)         } 
    | digit+   as num       { NUM (int_of_string num)   } 
    | eof                   { EOF                       } 
    | _                     { token lexbuf }

and comment = parse 
    | [^ '\n']              { comment lexbuf } 
    | '\n'                  { token lexbuf } 

