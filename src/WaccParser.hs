module WaccParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Control.Applicative hiding ( (<|>) , many )
import Control.Monad ( liftM , liftM2 )

import WaccDataTypes
import WaccLanguageDef

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <program> ::= 'begin' <func>* <stat> 'end' :::::::::::::::::::::::::::: --
pProgram :: Parser Program
pProgram = do
    waccReserved "begin"
    funcs <- many pFunc
    stat  <- pStat
    waccReserved "end"
    return $ Program funcs stat


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <func> ::= <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end' ::::: --
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
-- :: <param-list> ::= <param> (',' <param>)* ::::::::::::::::::::::::::::::: --
pParamList :: Parser ParamList
pParamList = sepBy pParam $ char ','


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <param> ::= <type> <ident> :::::::::::::::::::::::::::::::::::::::::::: --
pParam :: Parser Param
pParam = liftM2 Param pType waccIdentifier


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <stat> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pStat :: Parser Stat
pStat
    =  pWaccWord "skip"    SkipStat
   <|> pWaccLift "read"    ReadStat    pAssignLhs 
   <|> pWaccLift "free"    FreeStat    pExpr      
   <|> pWaccLift "return"  ReturnStat  pExpr      
   <|> pWaccLift "exit"    ExitStat    pExpr      
   <|> pWaccLift "print"   PrintStat   pExpr      
   <|> pWaccLift "println" PrintlnStat pExpr
   <|> pDeclareStat
   <|> pAssignStat
   <|> pIfStat
   <|> pWhileStat
   <|> pScopedStat
   <|> pSeqStat

    where

        pDeclareStat = do -- <type> <ident> '=' <assign-rhs>
          sType <- pType
          ident <- waccIdentifier
          waccReservedOp "="
          rhs   <- pAssignRhs
          return $ DeclareStat sType ident rhs
          
        pAssignStat = do -- <assign-lhs> = <assign-rhs>
          lhs <- pAssignLhs
          waccReservedOp "="
          rhs <- pAssignRhs
          return $ AssignStat lhs rhs
             
        pIfStat = do -- 'if' <expr> 'then' <stat> 'else' <stat> 'fi'
          waccReserved "if"
          expr  <- pExpr
          waccReserved "then"
          stat1 <- pStat
          waccReserved "else"
          stat2 <- pStat
          waccReserved "fi"
          return $ IfStat expr stat1 stat2
               
        pWhileStat = do -- 'while' <expr> 'do' <stat> 'done'
          waccReserved "while"
          expr <- pExpr
          waccReserved "do"
          stat <- pStat
          waccReserved "done"
          return $ WhileStat expr stat
    
        pScopedStat = do -- 'begin' <stat> 'end
          waccReserved "begin"
          stat <- pStat
          waccReserved "end"
          return $ ScopedStat stat
           
        pSeqStat = do -- <stat> ';' <stat>
          stat1 <- pStat
          waccSemi
          stat2 <- pStat
          return $ SeqStat stat1 stat2


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <assign-lhs> ::= <ident> | <pair-elem> | <array-elem> ::::::::::::::::: --
pAssignLhs :: Parser AssignLhs
pAssignLhs
    =  liftM LhsIdent     waccIdentifier
   <|> liftM LhsPairElem  pPairElem
   <|> liftM LhsArrayElem pArrayElem


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <assign-rhs> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pAssignRhs :: Parser AssignRhs
pAssignRhs
    =  liftM RhsExpr       pExpr          -- <expr>
   <|> liftM RhsPairElem   pPairElem      -- <pair-elem>
   <|> liftM RhsArrayLiter pArrayLiter    -- <array-liter>
   <|> pRhsNewPair
   <|> pRhsCall

    where 

        pRhsNewPair = do                    -- 'newpair' '(' <expr> ',' <expr> ')'
          waccReserved "newpair" 
          char '('
          expr1 <- pExpr         
          char ','
          expr2 <- pExpr         
          char ')'
          return $ RhsNewPair expr1 expr2
    
        pRhsCall = do                       -- 'call' <ident> '(' <arg-list>? ')' TODO liftM2
          waccReserved "call" 
          ident <- waccIdentifier
          aList <- waccParens $ many pExpr
          return $ RhsCall ident aList 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-elem> ::= 'fst' <expr> | 'snd' <expr' ::::::::::::::::::::::::::: --
pPairElem :: Parser PairElem
pPairElem
    =  pWaccLift "fst" Fst pExpr
   <|> pWaccLift "snd" Snd pExpr 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <type> ::= <base-type> | <array-type> | <pair-type> ::::::::::::::::::: --
pType :: Parser Type
pType
    =  liftM TypeBase  pBaseType
   <|> liftM TypeArray pArrayType
   <|> liftM TypePair  pPairType


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <base-type> ::= 'int' | 'bool' | 'char' | 'string' :::::::::::::::::::: --
pBaseType :: Parser BaseType
pBaseType
    =  pWaccWord "int"    IntBaseType    
   <|> pWaccWord "bool"   BoolBaseType   
   <|> pWaccWord "char"   CharBaseType   
   <|> pWaccWord "string" StringBaseType 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <array-type> ::= <type> '[' ']' ::::::::::::::::::::::::::::::::::::::: --
pArrayType :: Parser ArrayType
pArrayType = do
  typez <- pType 
  string "[]"
  return $ ArrayType typez


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')' :::::::::::::::::: --
pPairType :: Parser PairType
pPairType = do
    waccReserved "pair"
    char '('
    pairElem1 <- pPairElemType
    waccReserved ","
    pairElem2 <- pPairElemType
    char ')'   
    return (pairElem1, pairElem2)


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-elem-type> ::= <base-type> | <array-type> | 'pair' :::::::::::::: --
pPairElemType :: Parser PairElemType
pPairElemType
    =  liftM BasePairElemType  pBaseType
   <|> liftM ArrayPairElemType pArrayType
   <|> pWaccWord "null" PairPairElemType 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <expr> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pExpr :: Parser Expr
pExpr = buildExpressionParser waccOperators pExpr' 
  
  where  
    
    pExpr' 
      =  pParensExpr      -- '(' <expr> ')'
     <|> pIntLiterExpr    -- <int-liter>
     <|> pCharLiterExpr   -- <char-liter>
     <|> pStrLiterExpr    -- <str-liter>
     <|> pPairLiterExpr   -- <pair-liter>
     <|> pIdentExpr       -- <ident>
     <|> pArrayElemExpr   -- <array-elem>
     <|> pBoolLiterExpr   -- <bool-liter>
     <|> pUnaryOperExpr   -- <unary-oper> <expr>
     <|> pBinaryOperExpr  -- <expr> <binary-oper> <expr>
   
    pParensExpr    = waccParens pExpr

    pIntLiterExpr  = liftM IntLiterExpr   pIntLiter      
    pCharLiterExpr = liftM CharLiterExpr  anyChar        
    pStrLiterExpr  = liftM StrLiterExpr $ many anyChar   
    pPairLiterExpr = liftM PairLiterExpr  pPairLiter           
    pIdentExpr     = liftM IdentExpr      waccIdentifier 
    pArrayElemExpr = liftM ArrayElemExpr  pArrayElem     

    pBoolLiterExpr                                       
      =  "true"  `pWaccWord` BoolLiterExpr True     
     <|> "false" `pWaccWord` BoolLiterExpr False 

    pUnaryOperExpr 
      =  pUnOp "!"   NotUnOp
     <|> pUnOp "len" LenUnOp
     <|> pUnOp "ord" OrdUnOp
     <|> pUnOp "chr" ChrUnOp
     <|> pUnOp "-"   NegUnOp
     
    pUnOp str op = waccReservedOp str >> liftM ( UnaryOperExpr op ) pExpr   

    pBinaryOperExpr 
      =  pBinOp "+"  AddBinOp
     <|> pBinOp "-"  SubBinOp
     <|> pBinOp "*"  MulBinOp
     <|> pBinOp "/"  DivBinOp
     <|> pBinOp "%"  ModBinOp
     <|> pBinOp "&&" AndBinOp
     <|> pBinOp "||" OrrBinOp
     <|> pBinOp "<"  LsBinOp
     <|> pBinOp ">"  GtBinOp
     <|> pBinOp "<=" LEBinOp
     <|> pBinOp ">=" GEBinOp
     <|> pBinOp "==" EqBinOp
     <|> pBinOp "!=" NEBinOp
     
    pBinOp str op = waccReserved str >> liftM2 ( BinaryOperExpr op ) pExpr pExpr


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <array-elem> ::= <ident> '[' <expr> ']' ::::::::::::::::::::::::::::::: --
pArrayElem :: Parser ArrayElem
pArrayElem = liftM2 ArrayElem waccIdentifier $ pBrackets pExpr


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <int-liter> ::= <int-sign>? <digit>+ :::::::::::::::::::::::::::::::::: --
pIntLiter :: Parser IntLiter
pIntLiter = liftM2 IntLiter pIntSign waccInteger {-*-}


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <int-sign> ::= '+' | '-' :::::::::::::::::::::::::::::::::::::::::::::: --
pIntSign :: Parser ( Maybe IntSign )
pIntSign
    =  "+" `pWaccOp` Just Plus  
   <|> "-" `pWaccOp` Just Minus 
   <|> return Nothing


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']' ::::::::::::::::::: --
pArrayLiter :: Parser ArrayLiter
pArrayLiter = pBrackets $ sepBy pExpr $ char ','


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-liter> ::= 'null' ::::::::::::::::::::::::::::::::::::::::::::::: --
pPairLiter :: Parser PairLiter
pPairLiter = pWaccWord "null" Null 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- | <comment> ::= '#' (any-character-except-EOL)* (EOL) :::::::::::::::::::: --
pComment :: Parser Comment 
pComment = do
  char '#'
  comment <- many $ noneOf "\n"
  char '\n'
  return comment {-*-}


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Utils ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- If the word provided is a reserved word of the wacc language then its 
-- representation in the AST is just the value provided
-- This function is used to simplify expressions of the sort:
-- pData key value = do
--   waccReserved key
--   return value
pWaccWord :: String -> a -> Parser a
pWaccWord word value = waccReserved word >> return value

-- Similar to pWaccWord except it uses waccReservedOp
pWaccOp :: String -> a -> Parser a
pWaccOp op value = waccReservedOp op >> return value

-- Matches the reserved word and perfrorms liftM
-- This function is used to simplify expressions of the sort:
-- pData = do
--   waccReserved word
--   value <- parser
--   return $ Data value
pWaccLift :: String -> ( a -> b ) -> Parser a -> Parser b
pWaccLift word f p = waccReserved word >> liftM f p


pBrackets :: Parser a -> Parser a
pBrackets p = do
  char '['
  e <- p
  char ']'
  return e

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: TEST PARSER ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

-- |This function will run the parser, but additionally fail if it doesn't
-- consume all the input.
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse ( p <* eof ) ""

-- |This function will apply the parser, then also return any left over
-- input which wasn't parsed.
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ( (,) <$> p <*> leftOver ) ""
  where leftOver = manyTill anyToken eof
