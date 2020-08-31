
open Arg
open Syntax
open Printf


(* PARSE *) 
let parse in_channel = 
    let lexbuf  = Lexing.from_channel in_channel in 
    let cmds    = Parser.input Lexer.token lexbuf 
    in Parsing.clear_parser(); close_in in_channel; cmds 


(* INPUT FILE *) 
exception SingleFileMustBeSpecified
let file        = ref (None : string option ) 
let push_file   = fun str -> match !file with 
    | Some (_)      -> raise SingleFileMustBeSpecified
    | None          -> file := Some str 

let out_file    = ref "a.out" 
let args        = [
    ("-o", String (fun s-> out_file := s), "Output file name") ] 
let parseArgs() = Arg.parse args push_file "" 

let get_file    = fun () -> match !file with 
    | Some s        -> s 
    | None          -> raise SingleFileMustBeSpecified  ;;


(* MAIN: FILE -> LEX -> PARSE -> ASSEMBLE *) 
let _ = parseArgs () in 
let file = get_file () in 
let ch = open_in file in 
let cmds,tbl = parse ch  in 
let alloc = ref 16 in 
let out_channel = open_out !out_file in 
let out s       = fprintf out_channel "%s" s in 
List.iter (fun c -> out (asm_cmd tbl alloc c)) cmds ; 
close_out out_channel ;;


