load Xor.hdl,
output-file Xor.out,
output-list a b out a a out a ;
set a 0, set b 0, eval,output;
set a 0, set b 1, eval,output;
set a 1, set b 0, eval,output;
set a 1, set b 1, eval,output;

