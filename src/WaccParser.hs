module WaccParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
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
-- :: <assign-lhs> 
pAssignLhs :: Parser AssignLhs
pAssignLhs =  pSimple waccIdentifier LhsIdent     -- <ident>
          <|> pSimple pPairElem      LhsPairElem  -- <pair-elem> 
          <|> pSimple pArrayElem     LhsArrayElem -- <array-elem>


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <assign-rhs>
pAssignRhs :: Parser AssignRhs
pAssignRhs =  pSimple pExpr       RhsExpr       -- <expr>
          <|> pSimple pPairElem   RhsPairElem   -- <pair-elem>
          <|> pSimple pArrayLiter RhsArrayLiter -- <array-liter>
          <|> pRhsNewPair
          <|> pRhsCall

-- ::= 'newpair' '(' <expr> ',' <expr> ')'
pRhsNewPair :: Parser AssignRhs
pRhsNewPair = do 
  waccReserved "newpair" 
  char '('
  expr  <- pExpr         
  char ','
  expr' <- pExpr         
  char ')'
  return $ RhsNewPair expr expr'

-- ::= 'call' <ident> '(' <arg-list>? ')'
pRhsCall :: Parser AssignRhs
pRhsCall = do 
  waccReserved "call"
  ident   <- waccIdentifier 
  char '('
  argList <- many pExpr     
  char ')'
  return $ RhsCall ident argList


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pPairElem :: Parser PairElem
pPairElem = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pType :: Parser Type
pType = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pBaseType :: Parser BaseType
pBaseType = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pArrayType :: Parser ArrayType
pArrayType = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pPairType :: Parser PairType
pPairType = fail "TODO: Implement!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TODO COMMENT
pPairElemType :: Parser PairElemType
pPairElemType = fail "TODO: Implement!" 


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
-- :: <array-elem> ::= <ident> '[' <expr> ']'
pArrayElem :: Parser ArrayElem
pArrayElem = do
  ident <- waccIdentifier
  char '['
  expr <- pExpr
  char ']'
  return $ ArrayElem ident expr


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <int-liter> ::= <int-sign>? <digit>+ 
pIntLiter :: Parser IntLiter
pIntLiter = do
  intSign <- pIntSign
  digits  <- many1 digit
  return $ IntLiter intSign $ read digits


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <int-sign> ::= '+' | '-'
pIntSign :: Parser ( Maybe IntSign )
pIntSign =  waccReservedOp "+" `ifSuccess` Just Plus 
        <|> waccReservedOp "-" `ifSuccess` Just Minus 
        <|> return Nothing


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']'
pArrayLiter :: Parser ArrayLiter
pArrayLiter = many pExpr


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-liter> ::= 'null'
pPairLiter :: Parser PairLiter
pPairLiter = waccReserved "null" `ifSuccess` Null 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Utils ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- Given a parser and a Type construtor, try to parse a token suing the given
-- parser and uses the result to contructo a value of the type provided
pSimple :: Parser a -> ( a -> b ) -> Parser b
pSimple p t = p >>= \e -> return $ t e


-- Perfoms the action provided and returns the value provided if the 
-- action didnt' fail
ifSuccess :: Monad m => m a -> b -> m b 
ifSuccess acc e = do acc ; return e 


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

