// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

// Put your code here.

(LOOP)
@KBD // A = KBD
D=M // D = *KBD
@CLEAR  // A = LOOP
D;JEQ  // if D == 0 (no keypress), jump to CLEAR
@FILL
0;JMP  // else jump to FILL

(CLEAR)
@R3
M=0 // set color to 0000 0000 0000 0000
@COLOR
0;JMP // color the screen

(FILL)
@21845
D=A
@R3
M=D // set color to 0101 0101 0101 0101
@COLOR
0;JMP // color the screen

(COLOR)
@SCREEN
D=A  // D = SCREEN
@R0
M=D // *R0 = SCREEN
@256
D=A // D = 256
@R1
M=D  // *R1 = 256

(ROWLOOP)
@R1
D=M  // D = *R1
@LOOP
D;JEQ // if we have no rows left to color, go to loop
@32
D=A // D = 32
@R2 // init loop variable for chunkloop
M=D // *R2 = 32; number of 16-bit numbers we need to put in to get a row
@R1
M=M-1 // decrement row counter

(CHUNKLOOP)
@R2
D=M // D = *R2 = remaining chunks in row
@ROWLOOP
D;JEQ  // done with this row! go to next one!
@R3
D=M // D = *R3 = color of fill
@R0  // A = R0 (memory address of pixels we want to poke)
A=M  // A = *A
M=D  // *A (which is **R0) = D toggle fill.
@R0
M=M+1 // advance screen pointer
@R2
M=M-1  // decrement num chunks left
@CHUNKLOOP
0;JMP  // keep filling in chunks

@D
@LOOP
0;JMP