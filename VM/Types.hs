module VM.Types where

data Segment = LCL
             | THIS
             | THAT
             | ARG
             | STATIC
             | TEMP
             | POINTER
             | CONSTANT
             deriving (Eq, Show)

type Value    = Int
type Label    = String
type Name     = String
type Program  = [Command]

data ArithLogicOp = SUB
                  | ADD
                  | NEG
                  | NOT
                  | AND
                  | OR
                  deriving (Eq)

data CompOp = EQOp
            | GTOp
            | LTOp
            deriving (Eq)

data BranchOp = GOTO
              | LABEL
              | IFGOTO
              deriving (Eq, Show)

data Command = PUSH     Segment Value
             | POP      Segment Value
             | ARLOG    ArithLogicOp
             | COMP     CompOp
             | BRNCH    BranchOp Label
             | CALL     Name Int
             | FUNCTION Name Int
             | RETURN
             deriving (Eq)

-- Write instances

segWrite :: Segment -> String
segWrite LCL      = "local"
segWrite THIS     = "this"
segWrite THAT     = "that"
segWrite STATIC   = "static"
segWrite TEMP     = "temp"
segWrite ARG      = "argument"
segWrite POINTER  = "pointer"
segWrite CONSTANT = "constant"

arithWrite :: ArithLogicOp -> String
arithWrite SUB = "sub"
arithWrite NEG = "neg"
arithWrite ADD = "add"
arithWrite NOT = "not"
arithWrite AND = "and"
arithWrite OR  = "or"

compWrite :: CompOp -> String
compWrite EQOp = "eq"
compWrite GTOp = "gt"
compWrite LTOp = "lt"

commandWrite :: Command -> String
commandWrite (PUSH  seg val)   = "push " ++ segWrite seg ++ ' ':(show val)
commandWrite (POP   seg val)   = "pop " ++ segWrite seg ++ ' ':(show val)
commandWrite (ARLOG arilog)    = arithWrite arilog
commandWrite (COMP  compop)    = compWrite compop
commandWrite (BRNCH GOTO    l) = "goto " ++ l
commandWrite (BRNCH LABEL   l) = "label " ++ l
commandWrite (BRNCH IFGOTO  l) = "if-goto " ++ l
commandWrite (CALL  name    n) = "call " ++ name ++ ' ':(show n)
commandWrite (FUNCTION name n) = "function " ++ name ++ ' ':(show n)
commandWrite RETURN            = "return"

programWrite :: Program -> String
programWrite = unlines . map commandWrite
