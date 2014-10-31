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
pStat = pSkipStat
		 <|> pStat' "free" FreeStat 
	   <|> pStat' "return" ReturnStat
		 <|> pStat' "exit" ExitStat
		 <|> pStat' "print" PrintStat
		 <|> pStat' "println" PrintlnStat
		 <|> pScopedStat
		 <|> pReadStat
		 <|> pWhileStat
		 <|> pSeqStat
		 <|> pAssignStat
		 <|> pIfStat
		 <|> pDeclareStat
		 			 		
pStat' :: String -> (Expr -> Stat) -> Parser Stat
pStat' key stat = do
  waccReserved key
  expr <- pExpr
  return $ stat expr
								   		
								   		
pSkipStat :: Parser Stat
pSkipStat = do
  waccReserved "skip"
  return $ SkipStat

pScopedStat :: Parser Stat
pScopedStat = do
  waccReserved "begin"
  stat <- pStat
  waccReserved "end"
  return $ ScopedStat stat
		 	
pReadStat :: Parser Stat
pReadStat = do
  waccReserved "read"
  assignLhs <- pAssignLhs
  return $ ReadStat assignLhs
		 	
pWhileStat :: Parser Stat
pWhileStat = do
  waccReserved "while"
  expr <- pExpr
  waccReserved "do"
  stat <- pStat
  waccReserved "done"
  return $ WhileStat expr stat
		 	    
pSeqStat :: Parser Stat
pSeqStat = do
  stat1 <- pStat
  waccSemi
  stat2 <- pStat
  return $ SeqStat stat1 stat2
		 	  
pAssignStat :: Parser Stat
pAssignStat = do
  assignLhs <- pAssignLhs
  waccReservedOp "="
  assignRhs <- pAssignRhs
  return $ AssignStat assignLhs assignRhs

pIfStat :: Parser Stat
pIfStat = do
  waccReserved "if"
  expr <- pExpr
  waccReserved "then"
  stat1 <- pStat
  waccReserved "else"
  stat2 <- pStat
  waccReserved "fi"
  return $ IfStat expr stat1 stat2

pDeclareStat :: Parser Stat
pDeclareStat = do
  typez <- pType
  ident <- waccIdentifier
  waccReservedOp "="
  assignRhs <- pAssignRhs
  return $ DeclareStat typez ident assignRhs          				  
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

