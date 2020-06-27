module VM.Types where

data Segment = LCL
             | THIS
             | THAT
             | ARG
             | STATIC
             | TEMP
             | POINTER
             | CONSTANT
             deriving (Eq)

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
              deriving (Eq)

data Command = PUSH     Segment Value
             | POP      Segment Value
             | ARLOG    ArithLogicOp
             | COMP     CompOp
             | BRNCH    BranchOp Label
             | CALL     Name Int
             | FUNCTION Name Int
             | RETURN
             deriving (Eq)

-- Show instances

instance Show Segment where
  show LCL      = "local"
  show THIS     = "this"
  show THAT     = "that"
  show STATIC   = "static"
  show TEMP     = "temp"
  show ARG      = "argument"
  show POINTER  = "pointer"
  show CONSTANT = "constant"

instance Show ArithLogicOp where
  show SUB = "sub"
  show NEG = "neg"
  show ADD = "add"
  show NOT = "not"
  show AND = "and"
  show OR  = "or"

instance Show CompOp where
  show EQOp = "eq"
  show GTOp = "gt"
  show LTOp = "lt"

instance Show Command where
  show (PUSH  seg val)   = "push " ++ show seg ++ ' ':show val
  show (POP   seg val)   = "pop "  ++ show seg ++ ' ':show val
  show (ARLOG arilog)    = show arilog
  show (COMP  compop)    = show compop
  show (BRNCH GOTO    l) = "goto " ++ l
  show (BRNCH LABEL   l) = "label " ++ l
  show (BRNCH IFGOTO  l) = "if-goto " ++ l
  show (CALL  name    n) = "call " ++ name ++ ' ':show n
  show (FUNCTION name n) = "function " ++ name ++ ' ':show n
  show RETURN            = "return"

programWrite :: Program -> String
programWrite = unlines . map show
