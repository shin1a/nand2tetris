(INIT)
	// initially set the screen pointer to point to @SCREEN
	@SCREEN
	D=A
	@screen_ptr
	M=D

	// create a pointer, @SCREEN_END, to point to the end
	// of the screen's memory map
	@8192
	D=D+A
	@SCREEN_END
	M=D

	// set the correct color depending on keyboard input
	@KBD
	D=M
	@WHITE
	D;JEQ

	@color
	M=-1 // -1 sets all the bits in the word to 1

	@DRAW
	0;JMP

(WHITE)
	@color
	M=0

(DRAW)
	// If the pointer is pointing to the last word in
	// the screens memory map, go back to init
	@screen_ptr
	D=M
	@SCREEN_END
	D=D-M
	@INIT
	D;JEQ

	// change the memory pointed at by the screen pointer
	// to the correct color
	@color
	D=M
	@screen_ptr
	A=M
	M=D

	// move the pointer to the next word in the screen's
	// memory map.
	@screen_ptr
	M=M+1

	@DRAW
	0;JMP
