%{
    open Syntax
    open Support
    
    exception ALUError


    let symboltbl:tbl = Hashtbl.create 1024 
    let reserved = [
    ("SP", 0);
    ("LCL", 1);
    ("ARG", 2);
    ("THIS", 3);("THAT", 4); 
    ("R0", 0);("R1", 1);("R2", 2);("R3", 3);("R4", 4);
    ("R5", 5);("R6", 6);("R7", 7);("R8", 8);("R9", 9);
    ("R10",10);("R11",11);("R12",12);("R13",13);("R14",14);("R15",15);
    ("SCREEN", 16384);
    ("KBD", 24576)
    ]
    let _ = List.iter (fun (s,i)-> Hashtbl.add symboltbl s i) reserved 

    let line = ref 0
    let incr_line () = incr line 
    let decr_line () = decr line 


%}

/* %token <arg> TOKEN */ 

%token NEWLINE 
%token EOF 
%token LPAREN RPAREN 
%token EQ 
%token PLUS MINUS MUL AND OR 
%token <int> NUM 
%token <string> VAR
%token A D M MD AM AD AMD 
%token AT 
%token ONE ZERO 
%token BANG
%token COLON SEMI 
%token JGT JEQ JGE JLT JNE JLE JMP 


%left  PLUS

%start input 
%type <Syntax.commands * Support.tbl> input 

%%
input: 
    | file                      { $1,symboltbl } 
file: 
    | EOF                       { []                 } 
    | line file                 { List.append $1 $2  } 

line : 
    | NEWLINE                   { []    }
    | command NEWLINE           { [$1]  }

command: 
    | comp jump                 { incr_line(); C_COMMAND([],$1,$2) } 
    | dest comp                 { incr_line(); C_COMMAND($1,$2,NULL) } 
    | dest comp jump            { incr_line(); C_COMMAND($1, $2, $3) }
    | AT symbol                 { incr_line(); A_COMMAND($2) }
    | LPAREN VAR RPAREN         { add symboltbl $2 !line; L_COMMAND($2) }

symbol: 
    | VAR                       { VAR($1)  } 
    | NUM                       { ADDR($1) } 

jump: 
    | SEMI jjj                  { $2  } 
jjj: 
    | JGT                       { JGT } 
    | JEQ                       { JEQ } 
    | JGE                       { JGE } 
    | JLT                       { JLT } 
    | JNE                       { JNE } 
    | JLE                       { JLE } 
    | JMP                       { JMP } 

dest: 
    | regs EQ                   { $1 } 
regs: 
    | reg                       { [$1]      } 
    | MD                        { [M;D]     } 
    | AM                        { [A;M]     } 
    | AD                        { [A;D]     } 
    | AMD                       { [A;M;D]   } 

/* CPU instruction  */
comp: 
    |       regOI               { REG($1)                   } 
    | MINUS regOI               { UMINUS($2)                } 
    | BANG  reg                 { NOT($2)                   } 
    | reg PLUS  regOI           { PLUS($1, $3)              } 
    | reg MINUS regOI           { MINUS($1,$3)              } 
    | reg MUL   regOI           { MUL($1,$3)                } 
    | reg AND   regOI           { AND($1,$3)                } 
    | reg OR    regOI           { OR($1,$3)                 } 

reg: 
    | A                         { A                         }                    
    | M                         { M                         }                      
    | D                         { D                         }                     

regOI: 
    | reg                       { $1                        } 
    | NUM                       { if $1=0 then ZERO else if $1=1 then ONE else raise ALUError } 
