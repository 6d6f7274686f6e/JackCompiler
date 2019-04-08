# Jack Compiler

## Description
Jack Compiler written in Haskell for the [nand2tetris](https://www.nand2tetris.org/) course.
Compiles from the Jack programming language to a virtual stack machine, and from the VM to an assembly language. 

See the nand2tetris course for more details.

## Usage
I called this program `jc` ("Jack Compiler"). First argument is the target language (`VM` or `ASM`), next two arguments
are the source folder and the destination.
```
jc (VM | ASM) source dest
```
