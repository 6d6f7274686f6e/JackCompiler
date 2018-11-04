module Jack.Parse where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Jack.Types

-- Parser

languageDef = emptyDef { Token.commentStart    = "/*"
                       , Token.commentEnd      = "*/"
                       , Token.commentLine     = "//"
                       , Token.identStart      = letter <|> char '_'
                       , Token.identLetter     = alphaNum
                       , Token.reservedOpNames = [ "+", "-", "*", "/", "&", "|", "<", ">", "=", "~"]
                       , Token.reservedNames   = [ "if"
                                                 , "else"
                                                 , "do"
                                                 , "return"
                                                 , "true"
                                                 , "false"
                                                 , "null"
                                                 , "this"
                                                 , "let"
                                                 , "return"
                                                 , "while"
                                                 , "void"
                                                 , "boolean"
                                                 , "char"
                                                 , "int"
                                                 , "var"
                                                 , "static"
                                                 , "field"
                                                 , "method"
                                                 , "function"
                                                 , "constructor"
                                                 , "class"
                                                 ]
                       }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier    lexer
reserved   = Token.reserved      lexer
reservedOp = Token.reservedOp    lexer
parens     = Token.parens        lexer
brackets   = Token.brackets      lexer
braces     = Token.braces        lexer
integer    = Token.integer       lexer
semicolon  = Token.semi          lexer
colon      = Token.colon         lexer
comma      = Token.comma         lexer
dot        = Token.dot           lexer
whiteSpace = Token.whiteSpace    lexer
symbol     = Token.symbol        lexer
stringLit  = Token.stringLiteral lexer

parseClass :: Parser Class
parseClass = do whiteSpace
                reserved "class"
                className <- identifier
                (classVarDecs, subroutineDecs) <- braces $ (,) <$> many parseClassVarDec
                                                               <*> many parseSubroutineDec
                return $ Class className classVarDecs subroutineDecs

parseClassVarDec :: Parser ClassVarDec
parseClassVarDec = ClassVarDec <$> parseClassVarDec_
                               <*> parseType
                               <*> (sepBy1 identifier comma >>= (semicolon >>) . return)

parseClassVarDec_ :: Parser ClassVarDec_
parseClassVarDec_ =   (reserved "static" >> return Static)
                  <|> (reserved "field" >> return Field)

parseSubroutineDec :: Parser SubroutineDec
parseSubroutineDec = do sub <- parseSubroutineDec_
                        typ <- parseType
                        nam <- identifier
                        lis <- parens parseParameterList
                        (vars, sts) <- braces parseSubroutineBody
                        return $ SubroutineDec sub typ nam lis vars sts

parseParameterList :: Parser ParameterList
parseParameterList = sepBy ((,) <$> parseType <*> identifier) comma

parseSubroutineBody :: Parser ([VarDec], Statements)
parseSubroutineBody = (,) <$> many parseVarDec <*> parseStatements

parseSubroutineDec_ :: Parser SubroutineDec_
parseSubroutineDec_ = (reserved "constructor" >> return Constructor)
                    <|> (reserved "function" >> return Function)
                    <|> (reserved "method" >> return Method)

parseType :: Parser Type
parseType = (reserved "int" >> return Int_)
          <|> (reserved "char" >> return Char_)
          <|> (reserved "boolean" >> return Boolean)
          <|> (reserved "void" >> return Void)
          <|> (Type <$> identifier)

parseVarDec :: Parser VarDec
parseVarDec = do reserved "var"
                 typ <- parseType
                 lis <- sepBy1 identifier comma
                 semicolon
                 return $ VarDec typ lis

parseStatements :: Parser Statements
parseStatements = many parseStatement

parseStatement :: Parser Statement
parseStatement = parseWhile <|> parseIf <|> parseDo <|> parseLet <|> parseReturn
  where parseWhile  = do reserved "while"
                         exp <- parens parseExpression
                         sts <- braces parseStatements
                         return $ WhileStatement exp sts
        parseReturn = do reserved "return"
                         a <- optionMaybe $ parseExpression
                         semicolon
                         return $ ReturnStatement a
        parseLet = do reserved "let"
                      nam <- identifier
                      arr <- optionMaybe (brackets parseExpression)
                      reservedOp "="
                      exp <- parseExpression
                      semicolon
                      return $ LetStatement nam arr exp
        parseDo = do reserved "do" 
                     cal <- parseSubroutineCall 
                     semicolon
                     return $ DoStatement cal
        parseIf = reserved "if" >> IfStatement <$> parens parseExpression
                                                   <*> braces parseStatements
                                                   <*> optionMaybe (reserved "else" >> braces parseStatements)

parseSubroutineCall = SubroutineCall <$> (optionMaybe $ try $ identifier >>= (dot >>) . return)
                                          <*> identifier
                                          <*> (parens $ sepBy parseExpression comma)

parseExpression :: Parser Expression
parseExpression = Expression <$> parseTerm <*> parseTerms_

parseTerm :: Parser Term
parseTerm =   try (StringConstant <$> stringLit)
          <|> try (SubroutineTerm <$> parseSubroutineCall)
          <|> try (KeywordConstant <$> parseKeywordConstant_)
          <|> try (ExpressionTerm <$> parens parseExpression)
          <|> try (UnaryOpTerm <$> parseUnaryOp <*> parseTerm)
          <|> try (IntegerConstant . fromInteger <$> integer)
          <|> try (VarArrayTerm <$> identifier <*> brackets parseExpression) 
          <|> try (VarTerm <$> identifier)

parseTerms_ :: Parser [(Op, Term)]
parseTerms_ = many $ (,) <$> parseOp <*> parseTerm

parseOp :: Parser Op
parseOp =   (symbol "+" >> return Plus)
        <|> (symbol "-" >> return Minus)
        <|> (symbol "*" >> return Mult)
        <|> (symbol "/" >> return Div)
        <|> (symbol "&" >> return And)
        <|> (symbol "|" >> return Or)
        <|> (symbol "<" >> return Lt)
        <|> (symbol ">" >> return Gt)
        <|> (symbol "=" >> return Eq_)

parseUnaryOp :: Parser UnaryOp
parseUnaryOp =   (symbol "-" >> return UnaryMinus)
             <|> (symbol "~" >> return Tilde)

parseKeywordConstant_ :: Parser KeywordConstant_
parseKeywordConstant_ =   (reserved "true" >> return True_)
                      <|> (reserved "false" >> return False_)
                      <|> (reserved "null" >> return Null)
                      <|> (reserved "this" >> return This)

parseJack :: String -> Either ParseError Class
parseJack = parse parseClass ""
