module WaccParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Control.Applicative hiding ( (<|>) , many )

import WaccDataTypes 
import WaccLanguageDef 

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <program> ::= 'begin' <func>* <stat> 'end'
pProgram :: Parser Program 
pProgram = do 
  waccReserved "begin"
  funcs <- many pFunc
  stat  <- pStat
  waccReserved "end"
  return $ Program funcs stat


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <func> ::= <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end' 
pFunc :: Parser Func 
pFunc = do 
  typez <- pType   
  ident <- waccIdentifier
  pList <- waccParens pParamList
  waccReserved "is"
  stat  <- pStat
  waccReserved "end"
  return $ Func typez ident pList stat


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <param-list> ::= <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end'
pParamList :: Parser ParamList
pParamList = fail "TODO: Implement!"


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <param> ::= <type> <ident>
pParam :: Parser Param  
pParam = do
  typez <- pType
  ident <- waccIdentifier
  return $ Param typez ident


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <stat> ::= pSkipStat | pFreeStat | ...
pStat :: Parser Stat
pStat =  pSkipStat
     <|> pFreeStat
     <|> fail "TODO: Implement!"


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT 
-- 'skip' 
-- 'free' <expr>
-- 'return' <expr>
-- 'exit' <expr>
-- 'print' <expr>
-- 'println' <expr>
-- 'begin' <stat> 'end'
-- 'read' <assign-lhs>
-- 'while' <expr> 'do' <stat> 'done'
-- <stat> ';' <stat>
-- <assign-lhs> '=' <assign-rhs>
-- 'if' <expr> 'then' <stat> 'else' <stat> 'fi'
-- <type> <ident> '=' <assign-rhs>
pSkipStat :: Parser Stat
pSkipStat = fail "TODO: Implement!" 
pFreeStat :: Parser Stat   
pFreeStat = fail "TODO: Implement!"
pReturnStat :: Parser Stat
pReturnStat = fail "TODO: Implement!"
pExitStat :: Parser Stat
pExitStat = fail "TODO: Implement!"
pPrintStat :: Parser Stat  
pPrintStat = fail "TODO: Implement!"
pPrintlnStat :: Parser Stat
pPrintlnStat = fail "TODO: Implement!"
pScopedStat :: Parser Stat
pScopedStat = fail "TODO: Implement!" 
pReadStat :: Parser Stat
pReadStat = fail "TODO: Implement!" 
pWhileStat :: Parser Stat  
pWhileStat = fail "TODO: Implement!"
pSeqStat :: Parser Stat   
pSeqStat = fail "TODO: Implement!"
pAssignStat :: Parser Stat
pAssignStat = fail "TODO: Implement!"
pIfStat :: Parser Stat    
pIfStat = fail "TODO: Implement!"   
pDeclareStat :: Parser Stat
pDeclareStat = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pAssignLhs :: Parser AssignLhs
pAssignLhs = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pAssignRhs :: Parser AssignRhs
pAssignRhs = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pPairElem :: Parser PairElem
pPairElem =  
  do  waccReserved "fst"
      expr <- pExpr
      return (Fst expr)
 <|>  
  do  waccReserved "snd"
      expr <- pExpr
      return (Snd expr)


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pType :: Parser Type
pType =  
  do  varType <- pBaseType
      return (TypeBase  varType)
 <|>  
  do  varType <- pPairType
      return (TypePair  varType)
 <|>  
  do  varType <- pArrayType
      return (TypeArray varType) 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pBaseType :: Parser BaseType
pBaseType = 
  do  waccReserved "int"
      return IntBaseType
  <|> 
  do  waccReserved "bool"
      return BoolBaseType
  <|> 
  do  waccReserved "char"
      return CharBaseType
  <|> 
  do  waccReserved "string"
      return StringBaseType



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pArrayType :: Parser ArrayType
pArrayType = 
  do  varType <- pType
      return (ArrayType varType)


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pPairType :: Parser PairType
pPairType =  
  do  waccReserved "pair"
      waccReserved "("
      pairElem1 <- pPairElemType
      waccReserved ","
      pairElem2 <- pPairElemType
      waccReserved ")"
      return (pairElem1, pairElem2)

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pPairElemType :: Parser PairElemType
pPairElemType = 
  do  waccReserved "pair"
      return PairPairElemType
 <|>  
  do  varType <- pBaseType
      return (BasePairElemType varType)
 <|>  
  do  varType <- pArrayType
      return (ArrayPairElemType varType)


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pExpr :: Parser Expr
pExpr = buildExpressionParser waccOperators pExpr' 
 

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pExpr' :: Parser Expr
pExpr' =  pBoolLiterExpr
      <|> pCharLiterExpr
      <|> fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
-- <bool-liter>
-- <char-liter>
-- <ident>
-- <unary-oper> <expr>
-- '(' <expr> ')'
-- <int-liter>
-- <str-liter>
-- <pair-liter>
-- <array-elem>
-- <expr> <binary-oper> <expr>
pBoolLiterExpr :: Parser Expr
pBoolLiterExpr = fail "TODO: Implement!" 
pCharLiterExpr :: Parser Expr
pCharLiterExpr = fail "TODO: Implement!" 
pIdentExpr :: Parser Expr
pIdentExpr = fail "TODO: Implement!"     
pUnaryOperExpr :: Parser Expr
pUnaryOperExpr = fail "TODO: Implement!" 
pParenthesised :: Parser Expr
pParenthesised = fail "TODO: Implement!" 
pIntLiterExpr :: Parser Expr
pIntLiterExpr = fail "TODO: Implement!"  
pStrLiterExpr :: Parser Expr
pStrLiterExpr = fail "TODO: Implement!"  
pPairLiterExpr :: Parser Expr
pPairLiterExpr = fail "TODO: Implement!" 
pArrayElemExpr :: Parser Expr
pArrayElemExpr = fail "TODO: Implement!" 
pBinaryOperExpr :: Parser Expr
pBinaryOperExpr = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pArrayElem :: Parser ArrayElem
pArrayElem = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pIntLiter :: Parser IntLiter
pIntLiter = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pIntSign :: Parser IntSign
pIntSign = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TEST PARSER ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

--This function will run the parser, but additionally fail if it doesn't
--consume all the input.
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse ( p <* eof ) ""

--This function will apply the parser, then also return any left over
--input which wasn't parsed.
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ( (,) <$> p <*> leftOver ) ""
  where leftOver = manyTill anyToken eof

