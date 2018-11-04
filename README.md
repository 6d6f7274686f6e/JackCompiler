# Jack Compiler

## Description
Jack Compiler written in Haskell for the [nand2tetris](https://www.nand2tetris.org/) course.
Compiles from the Jack programming language to a virtual stack machine, and from the VM to an assembly language. 

See the nand2tetris course for more details.

## Usage
For now, a proper compiling interface has yet to be implemented. Compilation is done directly in GHCi after loading 
`Main.hs`, with the `compileFolder :: Source -> Destination -> IO ()` and
`compileFolder2VMFolder :: Source -> Destination -> IO ()` functions.
