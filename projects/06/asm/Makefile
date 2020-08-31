
all: ml  
	ocamlc -o Asm support.cmo lexer.cmo parser.cmo syntax.cmo asm.cmo 

parser:	
	ocamlc -c support.ml 
	ocamlc -c syntax.ml
	ocamlyacc parser.mly 
	ocamlc -c parser.mli
	
lexer: parser  
	ocamllex lexer.mll

ml: lexer 
	ocamlc -c lexer.ml parser.ml syntax.ml asm.ml 


clean: 
	rm -rf *.mli *.cmo *.cmi Asm lexer.ml parser.ml
