


exception ParseError

type address = 
    | VAR of string 
    | ADDR of int 

type reg = 
    | A | D | M 
    | ONE | ZERO 

type dest = reg list 

type jump = 
    | NULL
    | JGT
    | JEQ
    | JGE
    | JLT
    | JNE
    | JLE
    | JMP 

type comp  = 
    | PLUS      of reg * reg 
    | MINUS     of reg * reg 
    | MUL       of reg * reg 
    | AND       of reg * reg 
    | OR        of reg * reg 
    | NOT       of reg 
    | UMINUS    of reg 
    | REG       of reg 

type command = 
    | A_COMMAND of address
    | C_COMMAND of dest * comp * jump  
    | L_COMMAND of string  

type commands = command list 

let pr = print_string 
let pi = print_int 
let pn = print_newline
let pr_addr = function 
    | VAR(s) -> pr s 
    | ADDR(i) -> pi i 
let pr_cmd = function 
    | A_COMMAND(a)      -> pr"@"; pr_addr a ;pn()
    | C_COMMAND(d,c,j)  -> ()
    | _                 -> () 

type bit = O | I 
type code = bit list 

exception ParseErrorComp
exception ParseErrorDest 
exception ParseErrorJump 

let asm_comp = function 
    | REG(ZERO)         -> "0101010" 
    | REG(ONE)          -> "0111111" 
    | UMINUS(ONE)       -> "0111010" 
    | REG(D)            -> "0001100" 
    | REG(A)            -> "0110000" 
    | NOT(D)            -> "0001101"
    | NOT(A)            -> "0110001"
    | UMINUS(D)         -> "0001111"
    | UMINUS(A)         -> "0110011"
    | PLUS(D,ONE)       -> "0011111" 
    | PLUS(A,ONE)       -> "0110111"
    | MINUS(D,ONE)      -> "0001110"
    | MINUS(A,ONE)      -> "0110010"
    | PLUS(D,A)         -> "0000010"
    | MINUS(D,A)        -> "0010011"
    | MINUS(A,D)        -> "0000111"
    | AND(D,A)          -> "0000000"
    | OR(D,A)           -> "0010101"
    | REG(M)            -> "1110000"
    | NOT(M)            -> "1110001"
    | UMINUS(M)         -> "1110011"
    | PLUS(M,ONE)       -> "1110111" 
    | MINUS(M,ONE)      -> "1110010" 
    | PLUS(D,M)         -> "1000010"
    | MINUS(D,M)        -> "1010011"
    | MINUS(M,D)        -> "1000111"
    | AND(D,M)          -> "1000000" 
    | OR(D,M)           -> "1010101" 
    | _                 -> raise ParseErrorComp

let asm_dest = function 
    | []                -> "000" 
    | M::[]             -> "001" 
    | D::[]             -> "010" 
    | M::D::[]          -> "011" 
    | A::[]             -> "100"
    | A::M::[]          -> "101" 
    | A::D::[]          -> "110" 
    | A::M::D::[]       -> "111" 
    | _                 -> raise ParseErrorDest 

let asm_jump = function 
    | NULL              -> "000" 
    | JGT               -> "001" 
    | JEQ               -> "010" 
    | JGE               -> "011" 
    | JLT               -> "100" 
    | JNE               -> "101" 
    | JLE               -> "110" 
    | JMP               -> "111" 

(* A_Command *) 

exception ParseErrorACommand
exception OutOfMemory 

let a_cmd = ref (Bytes.create 16)

let rec exp n m = if m<= 0 then 1 else n * exp n(m-1) 

let rec binary_of_int i = function 
    | n when n>15       -> raise OutOfMemory
    | 1                 -> if i=1 then "1" else "0"  
    | n                 -> let pivot = exp 2 (n-1) in 
                            if i < pivot 
                            then "0" ^ binary_of_int i (n-1) 
                            else "1" ^ binary_of_int (i-pivot) (n-1) 

let get_addr tbl alloc str = 
    try Hashtbl.find tbl str 
    with Not_found -> let i = !alloc in Hashtbl.add tbl str i; incr alloc; i ;;

let asm_cmd tbl alloc = function 
    | A_COMMAND(ADDR(i))    -> ( "0" ^ binary_of_int i 15 ) ^ "\n" 
    | A_COMMAND(VAR(str))   -> let i = get_addr tbl alloc str  in 
                               ( "0" ^ binary_of_int i 15 ) ^ "\n"
    | C_COMMAND(d,c,j)      -> ( "111" ^ asm_comp c ^ asm_dest d ^ asm_jump j ) ^ "\n"  
    | L_COMMAND(_)          -> "" 



