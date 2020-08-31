let pi = print_int 
let pn = print_newline 


type info           = L of int | UNKNOWN 
type 'a withinfo    = {i: info ; v: 'a} 

let createInfo l    = L(l) 


exception LabelNameDuplicated 
type tbl = (string,int) Hashtbl.t
let add tbl s i = 
        try Hashtbl.find tbl s ; raise LabelNameDuplicated 
        with | LabelNameDuplicated -> raise LabelNameDuplicated 
             | e -> Hashtbl.add tbl s i 
