module Jack2VM where

import qualified Jack.Types as Jack
import qualified VM.Types as VM

import Control.Monad.State
import Data.Char
import qualified Data.Map as M

type Counter     = Integer
type SymbolValue = (Jack.Type, VM.Segment, VM.Value)
type SymbolTable = M.Map Jack.Identifier SymbolValue

class2VM :: Jack.Class -> VM.Program
class2VM = flip (evalState . class2VM_st) 0

emptySymbolTable :: Jack.ClassName -> SymbolTable
emptySymbolTable name = M.fromList [ ("this",            (Jack.Type name, VM.ARG, 0))
                                   , ("0stringConstant", (Jack.Type "String", VM.TEMP, 1))
                                   ]
-- 0stringConstant is used to store Jack.StringConstant terms.
-- Since the identifier starts with a number, the parser will reject any 
-- Jack program trying to use it as a proper variable.

addClassVars :: SymbolTable -> [Jack.ClassVarDec] -> SymbolTable
addClassVars symtab decs = addStatics 0 decs
  where addStatics _ [] = addFields 0 decs
        addStatics n ((Jack.ClassVarDec Jack.Field  _ _  ):rest) = addStatics n rest
        addStatics n ((Jack.ClassVarDec Jack.Static t nms):rest) = addSts n t nms $ addStatics (n + (length nms)) rest
        addSts _ _ []     symtab = symtab
        addSts n t (s:ss) symtab = M.insert s (t, VM.STATIC, n) $ addSts (n+1) t ss symtab
        addFields _ [] = symtab
        addFields n ((Jack.ClassVarDec Jack.Static _ _  ):rest) = addFields n rest
        addFields n ((Jack.ClassVarDec Jack.Field  t nms):rest) = addFds n t nms $ addFields (n + (length nms)) rest
        addFds _ _ []     symtab = symtab
        addFds n t (f:fs) symtab = M.insert f (t, VM.THIS, n) $ addFds (n+1) t fs symtab

addVars :: SymbolTable -> [Jack.VarDec] -> SymbolTable
addVars symtab = loop 0
  where loop _ []                             = symtab
        loop n ((Jack.VarDec typ names):rest) = addVars_ n typ names $ loop (n + (length names)) rest
        addVars_ _ _ []     symtab = symtab
        addVars_ n t (s:ss) symtab = M.insert s (t, VM.LCL, n) $ addVars_ (n+1) t ss symtab

addParamsFunc :: SymbolTable -> Jack.ParameterList -> SymbolTable
addParamsFunc symtab list = addParams_ list 0 symtab
addParams :: SymbolTable -> Jack.ParameterList -> SymbolTable
addParams symtab list = addParams_ list 1 symtab
addParams_ :: Jack.ParameterList -> Int -> SymbolTable -> SymbolTable
addParams_ []                 _ symtab = symtab
addParams_ ((typ, name):rest) n symtab = M.insert name (typ, VM.ARG, n) $ addParams_ rest (n+1) symtab

expression2VM_st :: Jack.Expression -> SymbolTable -> Jack.ClassName -> State Counter VM.Program
expression2VM_st (Jack.Expression t1 ts) symtab className = do
  t1_ <- term2VM_st t1 symtab className 
  ts_ <- concat <$> (forM ts $ \(op, t2) -> ((++ op2VM op) <$>) $ term2VM_st t2 symtab className )
  return $ t1_ ++ ts_

term2VM_st :: Jack.Term -> SymbolTable -> Jack.ClassName -> State Counter VM.Program
term2VM_st (Jack.StringConstant s)  symtab className = (++) <$> statements2VM_st sts symtab className <*> tempstr
  where sts = [Jack.LetStatement "0stringConstant"
                                 Nothing
                                 $ Jack.Expression (Jack.SubroutineTerm $ Jack.SubroutineCall
                                                      (Just "String") 
                                                      "new" 
                                                      [Jack.Expression (Jack.IntegerConstant $ length s) []])
                                                   []]
              ++ map ((\c -> Jack.DoStatement (Jack.SubroutineCall
                                                (Just "0stringConstant") 
                                                "appendChar"
                                                [Jack.Expression (Jack.IntegerConstant c) []]))
                    . getCharCode) s
        getCharCode = ord
        tempstr = term2VM_st (Jack.VarTerm "0stringConstant") symtab className
term2VM_st (Jack.ExpressionTerm e)  symtab className = expression2VM_st e symtab className
term2VM_st (Jack.SubroutineTerm sc) symtab className = subCall2VM_st sc symtab className
term2VM_st (Jack.VarArrayTerm v  i) symtab className = arrayVar2VM_push_st v i symtab className
term2VM_st (Jack.UnaryOpTerm op t)  symtab className = (++ unaryOp2VM op) <$> term2VM_st t symtab className
term2VM_st (Jack.IntegerConstant n) _      className = return $ [VM.PUSH VM.CONSTANT n]
term2VM_st (Jack.VarTerm var)       symtab className = return $ var2VM_push var symtab
term2VM_st (Jack.KeywordConstant k) _      className = return $ keywordConstant2VM k

unaryOp2VM :: Jack.UnaryOp -> VM.Program
unaryOp2VM Jack.UnaryMinus = [VM.ARLOG VM.NEG]
unaryOp2VM Jack.Tilde      = [VM.ARLOG VM.NOT]

op2VM :: Jack.Op -> VM.Program
op2VM Jack.Plus  = [VM.ARLOG VM.ADD]
op2VM Jack.Minus = [VM.ARLOG VM.SUB]
op2VM Jack.Or    = [VM.ARLOG VM.OR]
op2VM Jack.And   = [VM.ARLOG VM.AND]
op2VM Jack.Eq_   = [VM.COMP VM.EQOp]
op2VM Jack.Gt    = [VM.COMP VM.GTOp]
op2VM Jack.Lt    = [VM.COMP VM.LTOp]
op2VM Jack.Mult  = [VM.CALL "Math.multiply" 2]
op2VM Jack.Div   = [VM.CALL "Math.divide" 2]

var2VM_push :: Jack.VarName -> SymbolTable -> VM.Program
var2VM_push var symtab = case var2VM var symtab of
                           Just (_, seg, n) -> [VM.PUSH seg n]
                           Nothing          -> error $ "Compilation : Variable " ++ var ++ " does not exist in this scope."

var2VM_pop :: Jack.VarName -> SymbolTable -> VM.Program
var2VM_pop var symtab = case var2VM var symtab of
                          Nothing          -> error $ "Compilationa : Variable " ++ var ++ " does not exist in this scope."
                          Just (_, seg, n) -> [VM.POP seg n]

var2VM :: Jack.VarName -> SymbolTable -> Maybe SymbolValue
var2VM = M.lookup

arrayVar2VM_push_st :: Jack.VarName -> Jack.Expression -> SymbolTable -> Jack.ClassName -> State Counter VM.Program
arrayVar2VM_push_st var exp symtab className = (++ [ VM.PUSH VM.THAT 0 ]) <$> arrayVar2VM_st var exp symtab className 

arrayVar2VM_pop_st :: Jack.VarName -> Jack.Expression -> SymbolTable -> Jack.ClassName -> State Counter VM.Program
arrayVar2VM_pop_st var exp symtab className = (++ [VM.POP VM.THAT 0]) <$> arrayVar2VM_st var exp symtab className

arrayVar2VM_st :: Jack.VarName -> Jack.Expression -> SymbolTable -> Jack.ClassName -> State Counter VM.Program
arrayVar2VM_st var exp symtab className = (++ [ VM.PUSH seg n, VM.ARLOG VM.ADD, VM.POP VM.POINTER 1 ])
                                        <$> expression2VM_st exp symtab className
  where (seg, n) = case var2VM var symtab of
                     Nothing -> error $ "Compilation : Array "
                                      ++ var 
                                      ++ " does not exist in this scope (Class : "
                                      ++ className
                                      ++ ")."
                     Just (_, seg, n) -> (seg, n)

keywordConstant2VM :: Jack.KeywordConstant_ -> VM.Program
keywordConstant2VM (Jack.True_)  = [VM.PUSH VM.CONSTANT 0, VM.ARLOG VM.NOT]
keywordConstant2VM (Jack.False_) = [VM.PUSH VM.CONSTANT 0]
keywordConstant2VM (Jack.Null)   = [VM.PUSH VM.CONSTANT 0]
keywordConstant2VM (Jack.This)   = [VM.PUSH VM.POINTER 0]

subCall2VM_st :: Jack.SubroutineCall -> SymbolTable -> Jack.ClassName -> State Counter VM.Program
subCall2VM_st (Jack.SubroutineCall mid name explist) symtab className = do
  pushList <- ((VM.PUSH VM.POINTER 0):) . concat <$> mapM (\x -> expression2VM_st x symtab className) explist
  let nargs    = length explist + 1
  case mid of
    Nothing -> return $ pushList ++ [VM.CALL (className ++ '.':name) nargs ]
    Just id -> case var2VM id symtab of
                 Nothing                      -> return $ tail pushList ++ [ VM.CALL (id ++ '.':name) (nargs-1) ]
                 Just (Jack.Type typ, seg, n) -> return $ var2VM_push id symtab 
                                                   ++ tail pushList
                                                   ++ [ VM.CALL (typ ++ '.':name) nargs ]
                 otherwise                    -> error $ "Compilation : Trying to call a method on a non-object type.\n"
                                                       ++ " In SubroutineCall : " 
                                                       ++ show (Jack.SubroutineCall mid name explist)

statement2VM_st :: Jack.Statement -> SymbolTable -> Jack.ClassName -> State Counter VM.Program
statement2VM_st (Jack.LetStatement var marr expr) symtab className =
  (++) <$> expression2VM_st expr symtab className <*> case marr of
                                                   Nothing -> return $ var2VM_pop var symtab
                                                   Just id -> arrayVar2VM_pop_st var id symtab className
statement2VM_st (Jack.IfStatement cond sts1 msts2) symtab className = do
  c <- get
  modify (+1)
  beginexp <- expression2VM_st cond symtab className
  let begin = beginexp
            ++ [ VM.ARLOG VM.NOT
              , VM.BRNCH VM.IFGOTO $ "IF." ++ show c ++ ".FALSE"
            ] 
      middle = [ VM.BRNCH VM.GOTO $ "IF." ++ show c ++ ".END"
               , VM.BRNCH VM.LABEL $ "IF." ++ show c ++ ".FALSE"
               ]
      end = [VM.BRNCH VM.LABEL $ "IF." ++ show c ++ ".END"]
  sts1_ <- statements2VM_st sts1 symtab className
  sts2_ <- case msts2 of 
             Nothing   -> return []
             Just sts2 -> statements2VM_st sts2 symtab className
  return $ begin ++ sts1_ ++ middle ++ sts2_  ++ end
statement2VM_st (Jack.WhileStatement cond sts) symtab className = do
  c <- get
  modify (+1)
  beginexp <- expression2VM_st cond symtab className
  let begin = [VM.BRNCH VM.LABEL $ "WHILE." ++ show c ++ ".BEGIN"]
            ++ beginexp
            ++ [ VM.ARLOG VM.NOT
              , VM.BRNCH VM.IFGOTO $ "WHILE." ++ show c ++ ".END"
              ]
      end = [ VM.BRNCH VM.GOTO $ "WHILE." ++ show c ++ ".BEGIN"
            , VM.BRNCH VM.LABEL $ "WHILE." ++ show c ++ ".END"
            ]
  sts_ <- statements2VM_st sts symtab className
  return $ begin ++ sts_ ++ end
statement2VM_st (Jack.DoStatement subcall)  symtab className =
  (++ [VM.POP VM.TEMP 0 ]) <$> subCall2VM_st subcall symtab className 
statement2VM_st (Jack.ReturnStatement mexp) symtab className =
  case mexp of
    Nothing -> return $ [VM.PUSH VM.CONSTANT 0, VM.RETURN]
    Just xp -> (++ [VM.RETURN]) <$> expression2VM_st xp symtab className

statements2VM_st :: Jack.Statements -> SymbolTable -> Jack.ClassName -> State Counter VM.Program
statements2VM_st sts symtab className = concat <$> (forM sts $ \statement -> statement2VM_st statement symtab className)

subDec2VM_st :: Jack.SubroutineDec -> Int -> SymbolTable -> Jack.ClassName -> State Counter VM.Program
subDec2VM_st (Jack.SubroutineDec Jack.Constructor typ name params decs sts) nfields symtab className = do
  sts_ <- statements2VM_st sts (M.insert "this" (typ, VM.POINTER, 0)
          $ addVars (addParamsFunc symtab params) decs) className
  return $ [ VM.FUNCTION (className ++ '.':name) (countVars decs)
      , VM.PUSH VM.CONSTANT nfields
      , VM.CALL "Memory.alloc" 1
      , VM.POP VM.POINTER 0
      ]
    ++ sts_
subDec2VM_st (Jack.SubroutineDec Jack.Function typ name params decs sts) nfields symtab className = do
  sts_ <- statements2VM_st sts (addVars (addParamsFunc symtab params) decs) className
  return $ [ VM.FUNCTION (className ++ '.':name) (countVars decs) ] ++ sts_
subDec2VM_st (Jack.SubroutineDec Jack.Method   typ name params decs sts) nfields symtab className = do
  sts_ <- statements2VM_st sts (addVars (addParams symtab params) decs) className
  return $ [ VM.FUNCTION (className ++ '.':name) (countVars decs) 
      , VM.PUSH VM.ARG 0
      , VM.POP VM.POINTER 0
      ]
      ++ sts_

class2VM_st :: Jack.Class -> State Counter VM.Program
class2VM_st (Jack.Class name classdecs subdecs) =
  let nfields = countFields classdecs
      symtab = addClassVars (emptySymbolTable name) classdecs
  in concat <$> (forM subdecs $ \dec -> subDec2VM_st dec nfields symtab name)

countFields :: [Jack.ClassVarDec] -> Int
countFields [] = 0
countFields ((Jack.ClassVarDec Jack.Field _ f):rest) = (length f) + countFields rest
countFields ((Jack.ClassVarDec _          _ _):rest) = countFields rest

countVars :: [Jack.VarDec] -> Int
countVars [] = 0
countVars ((Jack.VarDec _ v):rest) = length v + countVars rest
