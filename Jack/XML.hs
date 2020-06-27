module Jack.XML where

import Jack.Types

import Data.List

type XML = String

jackClass2XML :: Class -> XML
jackClass2XML (Class name vars subs) = "<class>\n"
                                     ++ "  <keyword> class </keyword>\n"
                                     ++ "  <identifier> " ++ name ++ " </identifier>\n"
                                     ++ "  <symbol> { </symbol>\n"
                                     ++ shwvars ++ shwsubs
                                     ++ "  <symbol> } </symbol>\n</class>\n"
  where shwvars = addIndent $ concatMap jackClassVarDec2XML   vars
        shwsubs = addIndent $ concatMap jackSubroutineDec2XML subs

jackClassVarDec2XML :: ClassVarDec -> XML
jackClassVarDec2XML (ClassVarDec cvd_ typ vars) = "<classVarDec>\n"
                                                ++ "  <keyword> " ++ show cvd_ ++ " </keyword>\n" 
                                                ++ addIndent (jackType2XML typ)
                                                ++ addIndent shwvars
                                                ++ "  <symbol> ; </symbol>\n</classVarDec>\n"
  where shwvars = intercalate "<symbol> , </symbol>\n" $ map (\v -> "<identifier> " ++ v ++ " </identifier>\n") vars

jackSubroutineDec2XML :: SubroutineDec -> XML
jackSubroutineDec2XML (SubroutineDec sd_ typ name params vars sts) =
  "<subroutineDec>\n"
  ++ "  <keyword> " ++ show sd_ ++ " </keyword>\n"
  ++ addIndent (jackType2XML typ)
  ++ "  <identifier> " ++ name ++ " </identifier>\n"
  ++ "  <symbol> ( </symbol>\n"
  ++ "  <parameterList>\n"
  ++ addIndent (addIndent shwparams)
  ++ "  </parameterList>\n"
  ++ "  <symbol> ) </symbol>\n"
  ++ "  <subroutineBody>\n    <symbol> { </symbol>\n"
  ++ addIndent shwvars
  ++ addIndent shwsts
  ++ "    <symbol> } </symbol>\n  </subroutineBody>\n</subroutineDec>\n"
  where shwparams = intercalate "<symbol> , </symbol>\n" $ flip map params
                      $ \(t, n) -> jackType2XML t ++ "<identifier> " ++ n ++ " </identifier>\n"
        shwvars   = addIndent $ concatMap jackVarDec2XML vars
        shwsts    = addIndent $ jackStatements2XML sts

jackVarDec2XML :: VarDec -> XML
jackVarDec2XML (VarDec typ vars) = "<varDec>\n"
                                 ++ "  <keyword> var </keyword>\n" 
                                 ++ addIndent (jackType2XML typ)
                                 ++ addIndent shwvars 
                                 ++ "  <symbol> ; </symbol>\n"
                                 ++ "</varDec>\n"
  where shwvars = intercalate "<symbol> , </symbol>\n" $ map (\s -> "<identifier> " ++ s ++ " </identifier>\n") vars

jackStatements2XML :: Statements -> XML
jackStatements2XML sts = "<statements>\n" ++ addIndent (concatMap jackStatement2XML sts) ++ "</statements>\n"

jackType2XML :: Type -> XML
jackType2XML (Type s) = "<identifier> " ++ s ++ " </identifier>\n"
jackType2XML typ      = "<keyword> " ++ show typ ++ " </keyword>\n"

jackStatement2XML :: Statement -> XML
jackStatement2XML (LetStatement var marr expr) = "<letStatement>\n"
                                               ++ "  <keyword> let </keyword>\n"
                                               ++ "  <identifier> " ++ var ++ " </identifier>\n"
                                               ++ addIndent shwmarr
                                               ++ "  <symbol> = </symbol>\n"
                                               ++ addIndent (jackExpression2XML expr)
                                               ++ "  <symbol> ; </symbol>\n"
                                               ++ "</letStatement>\n"
  where shwmarr = case marr of
                    Nothing  -> ""
                    Just xpr -> "<symbol> [ </symbol>\n"
                             ++ jackExpression2XML xpr
                             ++ "<symbol> ] </symbol>\n"
jackStatement2XML (DoStatement subcall)  = "<doStatement>\n"
                                         ++ "  <keyword> do </keyword>\n"
                                         ++ addIndent (jackSubroutineCall2XML subcall)
                                         ++ "  <symbol> ; </symbol>\n"
                                         ++ "</doStatement>\n"
jackStatement2XML (ReturnStatement mvar) = "<returnStatement>\n"
                                         ++ "  <keyword> return </keyword>\n"
                                         ++ case mvar of
                                             Nothing -> ""
                                             Just e  -> addIndent (jackExpression2XML e)
                                         ++ "  <symbol> ; </symbol>\n"
                                         ++ "</returnStatement>\n"
jackStatement2XML (WhileStatement cond sts) = "<whileStatement>\n"
                                            ++ "  <keyword> while </keyword>\n"
                                            ++ "  <symbol> ( </symbol>\n"
                                            ++ addIndent (jackExpression2XML cond)
                                            ++ "  <symbol> ) </symbol>\n"
                                            ++ "  <symbol> { </symbol>\n"
                                            ++ addIndent (jackStatements2XML sts)
                                            ++ "  <symbol> } </symbol>\n"
                                            ++ "</whileStatement>\n"
jackStatement2XML (IfStatement cond sts mels) = "<ifStatement>\n"
                                              ++ "  <keyword> if </keyword>\n"
                                              ++ "  <symbol> ( </symbol>\n"
                                              ++ addIndent (jackExpression2XML cond)
                                              ++ "  <symbol> ) </symbol>\n"
                                              ++ "  <symbol> { </symbol>\n"
                                              ++ addIndent (jackStatements2XML sts)
                                              ++ "  <symbol> } </symbol>\n"
                                              ++ case mels of
                                                  Nothing  -> ""
                                                  Just els -> "  <keyword> else </keyword>\n"
                                                           ++ "  <symbol> { </symbol>\n"
                                                           ++ addIndent (jackStatements2XML els)
                                                           ++ "  <symbol> } </symbol>\n"
                                              ++ "</ifStatement>\n"

jackSubroutineCall2XML :: SubroutineCall -> XML
jackSubroutineCall2XML (SubroutineCall mid name exps) = shwmid
                                                      ++ "<identifier> " ++ name ++ " </identifier>\n"
                                                      ++ "<symbol> ( </symbol>\n"
                                                      ++ "<expressionList>\n"
                                                      ++ addIndent shwexps
                                                      ++ "</expressionList>\n"
                                                      ++ "<symbol> ) </symbol>\n"
  where shwmid = case mid of
                   Nothing -> ""
                   Just id -> "<identifier> " ++ id ++ " </identifier>\n" ++ "<symbol> . </symbol>\n"
        shwexps = intercalate "<symbol> , </symbol>\n" $ map jackExpression2XML exps

jackExpression2XML :: Expression -> XML
jackExpression2XML (Expression term rest) = "<expression>\n"
                                          ++ addIndent (jackTerm2XML term)
                                          ++ addIndent shwrest
                                          ++ "</expression>\n"
  where shwrest = concat $ flip map rest $ \(o, t) -> "<symbol> " ++ jackOpToXML o ++ " </symbol>\n" ++ jackTerm2XML t

jackTerm2XML :: Term -> XML
jackTerm2XML t = "<term>\n" ++ addIndent (trm t) ++ "</term>\n"
  where trm (IntegerConstant i) = "<integerConstant> " ++ show i ++ " </integerConstant>\n"
        trm (StringConstant  s) = "<stringConstant> " ++ s ++ " </stringConstant>\n"
        trm (KeywordConstant k) = "<keyword> " ++ show k ++ " </keyword>\n"
        trm (VarTerm         v) = "<identifier> " ++ v ++ " </identifier>\n"
        trm (VarArrayTerm n e)  = "<identifier> " ++ n ++ " </identifier>\n"
                                ++ "<symbol> [ </symbol>\n"
                                ++ jackExpression2XML e
                                ++ "<symbol> ] </symbol>\n"
        trm (SubroutineTerm sc) = jackSubroutineCall2XML sc
        trm (ExpressionTerm ex) = "<symbol> ( </symbol>\n"
                                ++ jackExpression2XML ex
                                ++ "<symbol> ) </symbol>\n"
        trm (UnaryOpTerm op tr) = "<symbol> " ++ show op ++ " </symbol>\n"
                                ++ jackTerm2XML tr

jackOpToXML :: Op -> XML
jackOpToXML Gt  = "&gt;"
jackOpToXML Lt  = "&lt;"
jackOpToXML And = "&amp;"
jackOpToXML op  = show op
