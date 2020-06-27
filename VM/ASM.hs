module VM.ASM where

import Control.Monad.State
import System.Environment
import System.FilePath
import Data.Char

import VM.Types

type Counter       = Int
type Filename      = String

vmPop2ASM :: String -> Value -> [String]
vmPop2ASM seg val = [ '@':show val
                    , "D=A"
                    , '@':seg
                    , "M=M+D"
                    , "@SP"
                    , "M=M-1"
                    , "A=M"
                    , "D=M"
                    , '@':seg
                    , "A=M"
                    , "M=D"
                    , '@':show val
                    , "D=A"
                    , '@':seg
                    , "M=M-D"
                    ]

vmPush2ASM :: String -> Value -> [String]
vmPush2ASM seg val = [ '@':show val
                     , "D=A"
                     , '@':seg
                     , "M=M+D"
                     , "A=M"
                     , "D=M"
                     , "@SP"
                     , "A=M"
                     , "M=D"
                     , "@SP"
                     , "M=M+1"
                     , '@':show val
                     , "D=A"
                     , '@':seg
                     , "M=M-D"
                     ]

vmPushPointer2ASM :: String -> [String]
vmPushPointer2ASM seg = [ '@':seg
                        , "D=M"
                        , "@SP"
                        , "A=M"
                        , "M=D"
                        , "@SP"
                        , "M=M+1"
                        ]

vmPopPointer2ASM :: String -> [String]
vmPopPointer2ASM seg = [ "@SP"
                       , "M=M-1"
                       , "A=M"
                       , "D=M"
                       , '@':seg
                       , "M=D"
                       ]

-- a counter is used here to avoid duplicate label names
vmCommand2ASM_ST :: Filename -> Command -> State Counter [String]
vmCommand2ASM_ST filename (PUSH seg val) = case seg of
                                             STATIC    -> return [ '@':(filename ++ '.':show val)
                                                            , "D=M"
                                                            , "@SP"
                                                            , "A=M"
                                                            , "M=D"
                                                            , "@SP"
                                                            , "M=M+1"
                                                            ]
                                             TEMP      -> return [ '@':show (val + 5)
                                                            , "D=M"
                                                            , "@SP"
                                                            , "A=M"
                                                            , "M=D"
                                                            , "@SP"
                                                            , "M=M+1"
                                                            ]
                                             POINTER   -> case val of
                                                            0 -> return $ vmPushPointer2ASM "THIS"
                                                            1 -> return $ vmPushPointer2ASM "THAT"
                                                            _ -> error $ "Illegal operation : push pointer "
                                                                               ++ show val
                                             CONSTANT  -> return [ '@':show val
                                                            , "D=A"
                                                            , "@SP"
                                                            , "A=M"
                                                            , "M=D"
                                                            , "@SP"
                                                            , "M=M+1"
                                                            ]
                                             _         -> return $ vmPush2ASM (show seg) val
vmCommand2ASM_ST filename (POP  seg val) = case seg of
                                             STATIC    -> return [ "@SP"
                                                            , "M=M-1"
                                                            , "A=M"
                                                            , "D=M"
                                                            , '@':(filename ++ '.':show val)
                                                            , "M=D"
                                                            ]
                                             TEMP      -> return [ "@SP"
                                                            , "M=M-1"
                                                            , "A=M"
                                                            , "D=M"
                                                            , '@':show (5 + val)
                                                            , "M=D"
                                                            ]
                                             POINTER   -> case val of
                                                            0 -> return $ vmPopPointer2ASM "THIS"
                                                            1 -> return $ vmPopPointer2ASM "THAT"
                                                            _ -> error $ "Illegal operation : pop pointer " 
                                                                               ++ show val
                                             CONSTANT  -> error $ "Illegal operation : pop constant " ++ show val
                                             _         -> return $ vmPop2ASM  (show seg) val
vmCommand2ASM_ST _        (ARLOG op)     = case op of
                                             NEG -> return [ "@SP"
                                                      , "A=M-1"
                                                      , "M=-M"
                                                      ]
                                             NOT -> return [ "@SP"
                                                      , "A=M-1"
                                                      , "M=!M"
                                                      ]
                                             ADD -> return $ vmCommandArlog2ASM '+'
                                             SUB -> return $ vmCommandArlog2ASM '-'
                                             AND -> return $ vmCommandArlog2ASM '&'
                                             OR  -> return $ vmCommandArlog2ASM '|'
                                         where vmCommandArlog2ASM op = [ "@SP"
                                                                       , "A=M-1"
                                                                       , "D=M"
                                                                       , "@SP"
                                                                       , "M=M-1"
                                                                       , "A=M-1"
                                                                       , "M=M" ++ op:"D"
                                                                       ]
-- It's up to the user to be careful not to set the same label several times
vmCommand2ASM_ST filename (BRNCH brchop label) = case brchop of
                                                   GOTO   -> return [ '@':filename ++ ".CUSTOM.LABEL." ++ label
                                                               , "0;JMP"
                                                               ]
                                                   LABEL  -> return [ '(':filename ++ ".CUSTOM.LABEL." ++ label ++ ")" ]
                                                   IFGOTO -> return [ "@SP"
                                                               , "M=M-1"
                                                               , "A=M"
                                                               , "D=M"
                                                               , '@':filename ++ ".CUSTOM.LABEL." ++ label
                                                               , "D;JNE"
                                                               ]
-- vmCommand2ASM_ST depends on and modifies counter ONLY if it is a COMP or a CALL
vmCommand2ASM_ST filename (COMP compop) = do c <- get
                                             modify (+1)
                                             case compop of
                                               EQOp -> return [ "@SP"
                                                         , "A=M-1"
                                                         , "D=M"
                                                         , "@SP"
                                                         , "M=M-1"
                                                         , "A=M-1"
                                                         , "D=M-D"
                                                         , '@':filename ++ ".EQ.false." ++ show c
                                                         , "D;JNE"
                                                         , '@':filename ++ ".EQ.true." ++ show c
                                                         , "0;JMP"
                                                         , '(':filename ++ ".EQ.false." ++ show c ++ ")"
                                                         , "D=-1"
                                                         , '(':filename ++ ".EQ.true." ++ show c ++ ")"
                                                         , "@SP"
                                                         , "A=M-1"
                                                         , "M=!D"
                                                         ]
                                               LTOp -> return $ vmCommandComp2ASM "LT" c
                                               GTOp -> return $ vmCommandComp2ASM "GT" c
                                             where vmCommandComp2ASM op c = [ "@SP"
                                                                            , "A=M-1"
                                                                            , "D=M"
                                                                            , "@SP"
                                                                            , "M=M-1"
                                                                            , "A=M-1"
                                                                            , "D=M-D"
                                                                            , '@':filename++'.':op++".true."++show c
                                                                            , "D;J" ++ op
                                                                            , "D=0"
                                                                            , '@':filename++'.':op++".end."++show c
                                                                            , "0;JMP"
                                                                            , '(':filename++'.':op++".true."++show c++")"
                                                                            , "D=-1"
                                                                            , '(':filename++'.':op++".end."++show c++")"
                                                                            , "@SP"
                                                                            , "A=M-1"
                                                                            , "M=D"
                                                                            ]
vmCommand2ASM_ST filename (CALL     name nArgs)  = do c <- get
                                                      modify (+1)
                                                      return [ '@':filename ++ '.':name ++ ".return." ++ show c
                                                        , "D=A"
                                                        , "@SP"
                                                        , "M=M+1"
                                                        , "A=M-1"
                                                        , "M=D"
                                                        , "@LCL"
                                                        , "D=M"
                                                        , "@SP"
                                                        , "M=M+1"
                                                        , "A=M-1"
                                                        , "M=D"
                                                        , "@ARG"
                                                        , "D=M"
                                                        , "@SP"
                                                        , "M=M+1"
                                                        , "A=M-1"
                                                        , "M=D"
                                                        , "@THIS"
                                                        , "D=M"
                                                        , "@SP"
                                                        , "M=M+1"
                                                        , "A=M-1"
                                                        , "M=D"
                                                        , "@THAT"
                                                        , "D=M"
                                                        , "@SP"
                                                        , "M=M+1"
                                                        , "A=M-1"
                                                        , "M=D"
                                                        , "@SP"
                                                        , "D=M"
                                                        , "@ARG"
                                                        , "M=D"
                                                        , '@':show (nArgs + 5)
                                                        , "D=A"
                                                        , "@ARG"
                                                        , "M=M-D"
                                                        , "@SP"
                                                        , "D=M"
                                                        , "@LCL"
                                                        , "M=D"
                                                        , '@':name
                                                        , "0;JMP"
                                                        , '(':filename ++ '.':name ++ ".return." ++ show c ++ ")"
                                                        ]
vmCommand2ASM_ST filename RETURN                 = return [ "@LCL"
                                                     , "D=M"
                                                     , "@R15"
                                                     , "M=D"
                                                     , "@5"
                                                     , "D=A"
                                                     , "@R15"
                                                     , "A=M-D"
                                                     , "D=M"
                                                     , "@R15"
                                                     , "M=D"
                                                     , "@SP"
                                                     , "A=M-1"
                                                     , "D=M"
                                                     , "@ARG"
                                                     , "A=M"
                                                     , "M=D"
                                                     , "@ARG"
                                                     , "D=M+1"
                                                     , "@SP"
                                                     , "M=D"
                                                     , "@LCL"
                                                     , "M=M-1"
                                                     , "A=M"
                                                     , "D=M"
                                                     , "@THAT"
                                                     , "M=D"
                                                     , "@LCL"
                                                     , "M=M-1"
                                                     , "A=M"
                                                     , "D=M"
                                                     , "@THIS"
                                                     , "M=D"
                                                     , "@LCL"
                                                     , "M=M-1"
                                                     , "A=M"
                                                     , "D=M"
                                                     , "@ARG"
                                                     , "M=D"
                                                     , "@LCL"
                                                     , "M=M-1"
                                                     , "A=M"
                                                     , "D=M"
                                                     , "@LCL"
                                                     , "M=D"
                                                     , "@R15"
                                                     , "A=M"
                                                     , "0;JMP"
                                                     ]
vmCommand2ASM_ST filename (FUNCTION name nLocal) = return $ ('(':name ++ ")"):concat (replicate nLocal push0)
  where push0 = [ "@SP"
                , "M=M+1"
                , "A=M-1"
                , "M=0"
                ]

vmCommands2ASM :: Filename -> [Command] -> [String]
vmCommands2ASM filename commands = concat $ (evalState $ forM commands $ vmCommand2ASM_ST filename) 0

parseVMLine :: String -> Command
parseVMLine s = let ws = words s
                in case length ws of
                     1 -> case map toUpper (head ws) of
                            "ADD"     -> ARLOG ADD
                            "AND"     -> ARLOG AND
                            "SUB"     -> ARLOG SUB
                            "NEG"     -> ARLOG NEG
                            "NOT"     -> ARLOG NOT
                            "OR"      -> ARLOG OR
                            "EQ"      -> COMP EQOp
                            "GT"      -> COMP GTOp
                            "LT"      -> COMP LTOp
                            "RETURN"  -> RETURN
                            _         -> error $ "Error : unrecognised command : " ++ head ws
                     2 -> let [command, label] = ws
                          in case map toUpper command of
                               "GOTO"     -> BRNCH GOTO   label
                               "LABEL"    -> BRNCH LABEL  label
                               "IF-GOTO"  -> BRNCH IFGOTO label
                               "CALL"     -> CALL         label 0
                               "FUNCTION" -> FUNCTION     label 0
                               _          -> error $ "Error : unrecognised command : " ++ command
                     3 -> let [command, arg1, arg2] = ws 
                           in if map toUpper command == "PUSH" || map toUpper command == "POP"
                              then let parsedCom = case map toUpper command of
                                                     "PUSH" -> PUSH
                                                     "POP"  -> POP
                                                     _      -> error $ "Error : unrecognised command : " ++ command
                                       parsedSeg = case map toUpper arg1 of
                                                     "THIS"     -> THIS
                                                     "THAT"     -> THAT
                                                     "LOCAL"    -> LCL
                                                     "POINTER"  -> POINTER
                                                     "ARGUMENT" -> ARG
                                                     "STATIC"   -> STATIC
                                                     "TEMP"     -> TEMP
                                                     "CONSTANT" -> CONSTANT
                                                     _          -> error $ "Error : unrecognised segment : " ++ arg1
                                   in parsedCom parsedSeg (read arg2)
                              else let parsedCom = case map toUpper command of
                                                     "FUNCTION" -> FUNCTION
                                                     "CALL"     -> CALL
                                                     _         -> error $ "Error : unrecognised command : " ++ command
                                   in parsedCom arg1 (read arg2)
                     _ -> error $ "Error : no parse : " ++ s

fileBegin :: [String]
fileBegin = [ "@256"
            , "D=A"
            , "@SP"
            , "M=D"
            ]

fileEnd :: [String]
fileEnd = [ "(PROGRAM-END-LOOP-INF)"
          , "@PROGRAM-END-LOOP-INF"
          , "0;JMP"
          ]

vm2ASM :: Filename -> String -> String
vm2ASM filename = unlines . vmCommands2ASM filename . map parseVMLine . filter isNonEmpty . map noComment . lines
  where isNonEmpty s = "" /= filter (`notElem` "\r\t\n ") s

noComment :: String -> String
noComment s = take (noComLength s) s
  where noComLength []          = 0
        noComLength ('/':'/':_) = 0
        noComLength (c:s)       = 1 + noComLength s

sanitizeFilename :: String -> String
sanitizeFilename = map (\c -> if c `elem` "/\\" then '.' else c) . flip replaceExtension ""

renameVM2ASM :: FilePath -> FilePath
renameVM2ASM = flip replaceExtension "asm"
