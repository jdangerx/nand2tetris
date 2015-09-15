// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.

// While R0 >= 0:
// R0 -= 1
// R2 += R1

@R2
M=0

(ADDLOOP)
@R0  // A = R0
D=M  // D = *R0
@END
D;JEQ // jump to end if D = 0
@R1  // A = R1
D=M  // D = *R1
@R2  // A = R2
M=D+M  // *R2 = *R2 + *R1
@R0  // A = R0
M=M-1 // *R0 -= 1
@ADDLOOP
0;JMP

(END)
@END
0;JMP