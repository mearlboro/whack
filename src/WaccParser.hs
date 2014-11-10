-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 3.1. WACC Parsers ::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

module WaccParser where

import WaccDataTypes
import WaccLanguageDef

import Text.Parsec.Prim ( parserZero )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Language ( haskellDef )
import Control.Applicative hiding ( (<|>) , many )
import Control.Monad ( liftM , liftM2 )
import Control.Monad.Fix


-- |3.1.1 program .......................................................  26 --    
-- |3.1.2 Statements ....................................................  66 -- 
-- |3.1.3 Types ......................................................... 183 -- 
-- |3.1.4 Expressions ................................................... 232 -- 
-- |3.1.5 Identifiers, literals ......................................... 314 -- 
 

-- |3.1.1 Program 

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
    fType <- pType
    fName <- waccIdentifier
    pList <- waccParens pParamList
    waccReserved "is"
    stat  <- pStat
    waccReserved "end"
    return $ Func fType fName pList stat


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <param-list> ::= <param> (',' <param>)* ::::::::::::::::::::::::::::::: --
pParamList :: Parser ParamList
pParamList = waccCommaSep1 pParam


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <param> ::= <type> <ident> :::::::::::::::::::::::::::::::::::::::::::: --
pParam :: Parser Param
pParam = liftM2 Param pType waccIdentifier


-- 3.2. Statement

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


pDeclareStat = do    -- <type> <ident> '=' <assign-rhs>
    sType <- pType
    ident <- waccIdentifier
    waccReservedOp "="
    rhs   <- pAssignRhs
    return $ DeclareStat sType ident rhs
    
pAssignStat = do     -- <assign-lhs> = <assign-rhs>
    lhs <- pAssignLhs
    waccReservedOp "="
    rhs <- pAssignRhs
    return $ AssignStat lhs rhs
       
pIfStat = do         -- 'if' <expr> 'then' <stat> 'else' <stat> 'fi'
    waccReserved "if"
    expr  <- pExpr
    waccReserved "then"
    stat1 <- pStat
    waccReserved "else"
    stat2 <- pStat
    waccReserved "fi"
    return $ IfStat expr stat1 stat2
         
pWhileStat = do      -- 'while' <expr> 'do' <stat> 'done'
    waccReserved "while"
    expr <- pExpr
    waccReserved "do"
    stat <- pStat
    waccReserved "done"
    return $ WhileStat expr stat

pScopedStat = do     -- 'begin' <stat> 'end
    waccReserved "begin"
    stat <- pStat
    waccReserved "end"
    return $ ScopedStat stat
     
pSeqStat =  do       -- <stat> ';' <stat>
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

        pRhsNewPair = do                  -- 'newpair' '(' <expr> ',' <expr> ')'
          waccReserved "newpair" 
          waccParens $ do
              expr1 <- pExpr         
              waccComma
              expr2 <- pExpr         
              return $ RhsNewPair expr1 expr2
    
        pRhsCall = do                     -- 'call' <ident> '(' <arg-list>? ')' TODO liftM2
          waccReserved "call" 
          fident <- waccIdentifier
          args   <- waccParens $ pArgList
          return $ RhsCall fident args 



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <arg-list> ::= <expr> (',' <expr>)* ::::::::::::::::::::::::::::::::::: --
pArgList :: Parser ArgList
pArgList = sepBy pExpr $ waccComma


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-elem> ::= 'fst' <expr> | 'snd' <expr' ::::::::::::::::::::::::::: --
pPairElem :: Parser PairElem
pPairElem
    =  pWaccLift "fst" Fst pExpr
   <|> pWaccLift "snd" Snd pExpr 


-- 3.3. Types

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <type> ::= <base-type> | <array-type> | <pair-type> ::::::::::::::::::: --
pType :: Parser Type
pType = do
    subtype <-  liftM TypeBase pBaseType 
            <|> liftM TypePair pPairType
    fix (\f -> (string "[]" >> fmap (TypeArray . ArrayType) f) <|> return subtype) 

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <base-type> ::= 'int' | 'bool' | 'char' | 'string' :::::::::::::::::::: --
pBaseType :: Parser BaseType
pBaseType
    =  try ( pWaccWord "int"    IntBaseType    )    
   <|> try ( pWaccWord "bool"   BoolBaseType   )
   <|> try ( pWaccWord "char"   CharBaseType   )
   <|> try ( pWaccWord "string" StringBaseType )


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')' :::::::::::::::::: --

-- |For easier management of the nested pair case, we chose a Maybe approach on
-- the pair type,y thus considering the nested pair just another graphical form of
-- the same type.
pPairType :: Parser PairType
pPairType = do
    waccReserved "pair"
    waccParens $ do
    pairElem1 <- pPairElemType
    waccComma
    pairElem2 <- pPairElemType
    return $ Just (pairElem1, pairElem2) 

      where

        pPairElemType :: Parser Type
        pPairElemType
            =  try ( pWaccWord "pair" $ TypePair Nothing )
           <|> pType

-- 3.4. Expressions

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <expr> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pExpr :: Parser Expr
pExpr = buildExpressionParser waccOperators pExpr' 

        where

            pExpr' :: Parser Expr
            pExpr'
                =  pIntLiterExpr
               <|> pBoolLiterExpr
               <|> pCharLiterExpr
               <|> pStrLiterExpr
               <|> pPairLiterExpr
               <|> pIdentExpr
               <|> pUnaryOperExpr
               <|> pParenthesised
               <|> pArrayElemExpr
               <|> pBinaryOperExpr
    

-- pIntLiterExpr, pBoolLiterExpr, pCharLiterExpr, pStrLiterExpr,  pPairLiterExpr, 
-- pIdentExpr,    pUnaryOperExpr, pParenthesised, pArrayElemExpr, pBinaryOperExpr
-- :: Parser Expr

pBoolLiterExpr = liftM BoolLiterExpr pBoolLiter                                       

pIntLiterExpr  = liftM IntLiterExpr  pIntLiter

pCharLiterExpr = liftM CharLiterExpr pCharLiter

pStrLiterExpr  = liftM StrLiterExpr  pStrLiter
 
pPairLiterExpr = liftM PairLiterExpr pPairLiter

pIdentExpr     = liftM IdentExpr     waccIdentifier

pArrayElemExpr = liftM ArrayElemExpr pArrayElem

pUnaryOperExpr
    =  pUnaryOperExp' "!"   NotUnOp
   <|> pUnaryOperExp' "len" LenUnOp
   <|> pUnaryOperExp' "ord" OrdUnOp
   <|> pUnaryOperExp' "chr" ChrUnOp
   <|> pUnaryOperExp' "-"   NegUnOp

    where

        pUnaryOperExp' string op =
            waccReservedOp string >> liftM (UnaryOperExpr op) pExpr

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

    where

        pBinaryOperExp' str op =
            waccReserved str >> liftM2 ( BinaryOperExpr op ) pExpr pExpr 

pParenthesised = waccParens pExpr


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <array-elem> ::= <ident> '[' <expr> ']' ::::::::::::::::::::::::::::::: --
pArrayElem :: Parser ArrayElem
pArrayElem = liftM2 ArrayElem waccIdentifier $ waccBrackets pExpr


-- 3.5. Literals

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <int-liter> ::= <int-sign>? <digit>+ :::::::::::::::::::::::::::::::::: --
pIntLiter :: Parser IntLiter
pIntLiter = liftM IntLiter waccInteger 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <bool-liter> ::= 'true' | 'false' ::::::::::::::::::::::::::::::::::::: --
pBoolLiter 
    =  ( waccReserved "true"  >> return True  )  
   <|> ( waccReserved "false" >> return False ) 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <char-liter> ::= ''' <char> ''' ::::::::::::::::::::::::::::::::::::::: --
pCharLiter :: Parser CharLiter
pCharLiter = charLiteral . makeTokenParser $ haskellDef


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <str-liter> ::= ''' <char>* ''' ::::::::::::::::::::::::::::::::::::::: --
pStrLiter :: Parser StrLiter
pStrLiter = stringLiteral . makeTokenParser $ haskellDef


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']' ::::::::::::::::::: --
pArrayLiter :: Parser ArrayLiter
pArrayLiter = waccBrackets $ sepBy pExpr $ waccComma


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-liter> ::= 'null' ::::::::::::::::::::::::::::::::::::::::::::::: --
pPairLiter :: Parser PairLiter
pPairLiter = pWaccWord "null" Null 


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


