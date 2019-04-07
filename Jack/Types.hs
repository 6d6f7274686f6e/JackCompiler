module Jack.Types where

type ParameterList   = [(Type, VarName)]
type Identifier      = String
type ClassName       = Identifier
type SubroutineName  = Identifier
type VarName         = Identifier
type Statements      = [Statement]
type ExpressionList  = [Expression]

data Class = Class ClassName [ClassVarDec] [SubroutineDec]
           deriving (Eq)

-- Variable Declaration Statement
data ClassVarDec = ClassVarDec ClassVarDec_ Type [VarName]
                 deriving (Eq)
data ClassVarDec_ = Static | Field
                  deriving (Eq)

data Type = Int_ | Char_ | Boolean | Void | Type ClassName
          deriving (Eq)

-- Subroutine Declaration Statement
data SubroutineDec = SubroutineDec SubroutineDec_
                                   Type
                                   SubroutineName
                                   ParameterList
                                   [VarDec]
                                   Statements
                   deriving (Eq)
data SubroutineDec_ = Constructor | Function | Method
                    deriving (Eq)

data VarDec = VarDec Type [VarName]
            deriving (Eq)

data Statement = LetStatement VarName (Maybe Expression) Expression
               | DoStatement SubroutineCall
               | WhileStatement Expression Statements
               | ReturnStatement (Maybe Expression)
               | IfStatement Expression Statements (Maybe Statements)
               deriving (Eq)

data SubroutineCall = SubroutineCall (Maybe Identifier)
                                     SubroutineName
                                     ExpressionList
                    deriving (Eq)

data Op = Plus
        | Minus
        | Mult
        | Div
        | And
        | Or
        | Lt
        | Gt 
        | Eq_
        deriving (Eq)
data UnaryOp = UnaryMinus | Tilde
             deriving (Eq)

data KeywordConstant_ = True_ | False_ | Null | This
                      deriving (Eq)

data Term = IntegerConstant Int
          | StringConstant  String
          | KeywordConstant KeywordConstant_
          | VarTerm VarName
          | VarArrayTerm VarName Expression
          | SubroutineTerm SubroutineCall
          | ExpressionTerm Expression
          | UnaryOpTerm UnaryOp Term
          deriving (Eq)

data Expression = Expression Term [(Op, Term)]
                deriving (Eq)

-- Show instances (theses are completely optional)

-- utility function
addIndent :: String -> String
addIndent = unlines . map ("  "++) . lines

instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"
  show And   = "&"
  show Or    = "|"
  show Lt    = "<"
  show Gt    = ">"
  show Eq_   = "="

instance Show UnaryOp where
  show UnaryMinus = "-"
  show Tilde      = "~"

instance Show KeywordConstant_ where
  show True_  = "true"
  show False_ = "false"
  show Null   = "null"
  show This   = "this"

instance Show Term where
  show (IntegerConstant x)     = show x
  show (StringConstant s)      = show s
  show (KeywordConstant k)     = show k
  show (VarTerm name)          = name
  show (VarArrayTerm name exp) = name ++ '[':(show exp) ++ "]"
  show (SubroutineTerm subcal) = show subcal
  show (ExpressionTerm expr)   = '(':(show expr) ++ ")"
  show (UnaryOpTerm op term)   = show op ++ show term

instance Show Expression where
  show (Expression term rest) = show term ++ (concat $ map (\(o, t) -> ' ':(show o) ++ ' ':(show t)) rest)

instance Show SubroutineCall where
  show (SubroutineCall ide name explist) = shwid ++ name ++ '(':shwexps ++ ")" 
    where shwexps = case explist of
                      [] -> ""
                      otherwise -> init $ init $ concat $ map ((++", ") . show) explist
          shwid   = case ide of
                      Nothing -> ""
                      Just s  -> s ++ "."

instance Show Statement where
  show (LetStatement name mexp exp) = "let " ++ name ++ shwmexp ++ " = " ++ show exp ++ ";\n"
    where shwmexp = case mexp of 
                      Nothing  -> ""
                      Just xpr -> '[':(show xpr) ++ "]"
  show (DoStatement subcall) = "do " ++ show subcall ++ ";\n"
  show (ReturnStatement mexp) = "return " ++ shwmexp ++ ";\n"
    where shwmexp = case mexp of
                      Nothing  -> ""
                      Just xpr -> show xpr
  show (WhileStatement cond sts) = "while(" ++ show cond ++ ")\n{" ++ shwsts ++ "}\n"
    where shwsts = addIndent $ concat $ map show sts
  show (IfStatement cond sts msts) = "if(" ++ show cond ++ ")\n{" ++ shwsts ++ "}\n" ++ shwmsts
    where shwsts = addIndent $ concat $ map show sts
          shwmsts = case msts of
                      Nothing   -> ""
                      Just ests -> "else\n{" ++ (addIndent $ concat $ map show ests) ++ "}\n"

instance Show VarDec where
  show (VarDec typ names) = "var " ++ show typ ++ ' ':shwnames ++ ";\n"
    where shwnames = case names of
                       [] -> "" 
                       otherwise -> (init $ init $ concat $ map (++", ") names)

instance Show SubroutineDec_ where
  show Constructor = "constructor"
  show Function    = "function"
  show Method      = "method"

instance Show SubroutineDec where
  show (SubroutineDec sd_ typ name params vars sts) = show sd_ ++ " " ++ show typ ++ ' ':name 
                                                         ++ '(':shwparams ++ ")\n{\n" ++ shwvars ++ "\n"
                                                         ++ shwsts ++ "}\n"
    where shwparams = case params of
                        [] -> ""
                        otherwise -> init $ init $ concat $ map (\(t,n) -> show t ++ ' ':n ++ ", ") params
          shwsts    = addIndent $ concat $ map show sts
          shwvars   = addIndent $ concat $ map show vars

instance Show Type where
  show Int_     = "int"
  show Char_    = "char"
  show Boolean  = "boolean"
  show Void     = "void"
  show (Type s) = s

instance Show Class where
  show (Class name vars subs) = "class " ++ name ++ "\n{\n" ++ shwvars ++ "\n" ++ shwsubs ++ "}\n"
    where shwvars = addIndent $ concat $ map show vars
          shwsubs = addIndent $ concat $ map show subs

instance Show ClassVarDec_ where
  show Static = "static"
  show Field  = "field"

instance Show ClassVarDec where
  show (ClassVarDec cvd_ typ vars) = show cvd_ ++ " " ++ show typ ++ " " ++ shwvars ++ ";\n"
    where shwvars = init $ init $ concat $ map (++ ", ") vars
