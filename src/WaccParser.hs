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
      <|> pIdentExpr
      <|> pUnaryOperExpr
      <|> pParenthesised 
      <|> pIntLiter
      <|> pStrLiter
      <|> pPairLiter
      <|> pArrayElemLiter
      <|> pBinOperLiter

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
pBoolLiterExpr 
  =  do
     waccReserved "true"
     return $ BoolLiterExpr True
 <|> do
     waccReserved "false"
     return $ BoolLiterExpr False


pCharLiterExpr :: Parser Expr
pCharLiterExpr = anyChar >>= \ch -> return $ CharLiterExpr ch


pIdentExpr :: Parser Expr
pIdentExpr 
  = do
    ident  <- waccIdentifier
    return $ IdentExpr ident


pUnaryOperExpr :: Parser Expr
pUnaryOperExpr 
  =  pUnaryOperExp' "!"   NotUnOp
 <|> pUnaryOperExp' "len" LenUnOp
 <|> pUnaryOperExp' "ord" OrdUnOp
 <|> pUnaryOperExp' "chr" ChrUnOp
 <|> pUnaryOperExp' "-"   NegUnOp

pUnaryOperExp' string op 
  = do
    waccReservedOp string
    expr   <- pExpr
    return $ UnaryOperExpr op expr


pParenthesised :: Parser Expr
pParenthesised = waccParens pExpr


pIntLiterExpr :: Parser Expr
pIntLiterExpr = pIntLiter >>= \x -> return $ IntLiterExpr x
 

pStrLiterExpr :: Parser Expr
pStrLiterExpr = many anyChar >>= \s -> return $ StrLiterExpr s


pPairLiterExpr :: Parser Expr
pPairLiterExpr = pPairLiter >>= \p -> return $ PairLiterExpr p


pArrayElemExpr :: Parser Expr
pArrayElemExpr = pArrayElem >>= \a -> return $ ArrayElemExpr a


pBinaryOperExpr :: Parser Expr
pBinaryOperExpr 
  =  pBinaryOperExp' "+"  AddBinOp
 <|> pBinaryOperExp' "-"  SubBinOp
 <|> pBinaryOperExp' "*"  MulBinOp
 <|> pBinaryOperExp' "/"  DivBinOp
 <|> pBinaryOperExp' "%"  ModBinOp
 <|> pBinaryOperExp' "&&" AndBinOp
 <|> pBinaryOperExp' "||" OrrBinOp
 <|> pBinaryOperExp' "<"  LsBinOp
 <|> pBinaryOperExp' ">"  GtBinOp
 <|> pBinaryOperExp' "<=" LEBinOp
 <|> pBinaryOperExp' ">=" GEBinOp
 <|> pBinaryOperExp' "==" EqBinOp
 <|> pBinaryOperExp' "!=" NEBinOp

pBinaryOperExp' string op 
  = do
    waccReservedOp string
    expr   <- pExpr
    expr'  <- pExpr
    return $ BinaryOperExpr op expr expr

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
-- :: <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']'
pArrayLiter :: Parser ArrayLiter
pArrayLiter = fail "TODO: Implement!"

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-liter> ::= 'null'
pPairLiter :: Parser PairLiter
pPairLiter = fail "TODO: Implement!"


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

