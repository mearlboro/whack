module WaccParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Control.Applicative hiding ( (<|>) , many )
import Control.Monad ( liftM )

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
pParamList = do
    pList <- sepBy pParam $ char ','
    return $ pList


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <param> ::= <type> <ident> :::::::::::::::::::::::::::::::::::::::::::: --
pParam :: Parser Param
pParam = do
    typez <- pType
    ident <- waccIdentifier
    return $ Param typez ident


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <stat> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pStat :: Parser Stat
pStat
    =  pSkipStat
   <|> pDeclareStat
   <|> pAssignStat
   <|> pReadStat
   <|> pStat' "free"    FreeStat
   <|> pStat' "return"  ReturnStat
   <|> pStat' "exit"    ExitStat
   <|> pStat' "print"   PrintStat
   <|> pStat' "println" PrintlnStat
   <|> pIfStat
   <|> pWhileStat
   <|> pScopedStat
   <|> pSeqStat


pStat' :: String -> (Expr -> Stat) -> Parser Stat
pStat' key stat = waccReserved key >> liftM stat pExpr


pSkipStat :: Parser Stat     -- 'skip'
pSkipStat = waccReserved "skip" >> return SkipStat


pDeclareStat :: Parser Stat  -- <type> <ident> '=' <assign-rhs>
pDeclareStat = do
    typez <- pType
    ident <- waccIdentifier
    waccReservedOp "="
    assignRhs <- pAssignRhs
    return $ DeclareStat typez ident assignRhs


pAssignStat :: Parser Stat   -- <assign-lhs> = <assign-rhs>
pAssignStat = do
    assignLhs <- pAssignLhs
    waccReservedOp "="
    assignRhs <- pAssignRhs
    return $ AssignStat assignLhs assignRhs


pReadStat :: Parser Stat     -- 'read' <assign-lhs>
pReadStat = waccReserved "read" >> liftM ReadStat pAssignLhs


pIfStat :: Parser Stat       -- 'if' <expr> 'then' <stat> 'else' <stat> 'fi'
pIfStat = do
    waccReserved "if"
    expr <- pExpr
    waccReserved "then"
    stat1 <- pStat
    waccReserved "else"
    stat2 <- pStat
    waccReserved "fi"
    return $ IfStat expr stat1 stat2


pWhileStat :: Parser Stat    -- 'while' <expr> 'do' <stat> 'done'
pWhileStat = do
    waccReserved "while"
    expr <- pExpr
    waccReserved "do"
    stat <- pStat
    waccReserved "done"
    return $ WhileStat expr stat


pScopedStat :: Parser Stat   -- 'begin' <stat> 'end'
pScopedStat = do
    waccReserved "begin"
    stat <- pStat
    waccReserved "end"
    return $ ScopedStat stat


pSeqStat :: Parser Stat      -- <stat> ';' <stat>
pSeqStat = do
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
    =  liftM RhsExpr       pExpr         -- <expr>
   <|> liftM RhsPairElem   pPairElem     -- <pair-elem>
   <|> liftM RhsArrayLiter pArrayLiter   -- <array-liter>
   <|> pRhsNewPair
   <|> pRhsCall


pRhsNewPair :: Parser AssignRhs          -- 'newpair' '(' <expr> ',' <expr> ')'
pRhsNewPair = do
    waccReserved "newpair"
    char '('
    expr  <- pExpr
    char ','
    expr' <- pExpr
    char ')'
    return $ RhsNewPair expr expr'


pRhsCall :: Parser AssignRhs             -- 'call' <ident> '(' <arg-list>? ')'
pRhsCall = do
    waccReserved "call"
    ident   <- waccIdentifier
    argList <- waccParens $ many pExpr
    return $ RhsCall ident argList


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-elem> ::= 'fst' <expr> | 'snd' <expr' ::::::::::::::::::::::::::: --
pPairElem :: Parser PairElem
pPairElem
    =  (waccReserved "fst" >> liftM Fst pExpr)
   <|> (waccReserved "snd" >> liftM Snd pExpr)


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
    =  (waccReserved "int"    >> return IntBaseType    )
   <|> (waccReserved "bool"   >> return BoolBaseType   )
   <|> (waccReserved "char"   >> return CharBaseType   )
   <|> (waccReserved "string" >> return StringBaseType )


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <array-type> ::= <type> '[' ']' ::::::::::::::::::::::::::::::::::::::: --
pArrayType :: Parser ArrayType
pArrayType = liftM ArrayType pType


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')' :::::::::::::::::: --
pPairType :: Parser PairType
pPairType = do
    waccReserved "pair"
    waccReserved "("
    pairElem1 <- pPairElemType
    waccReserved ","
    pairElem2 <- pPairElemType
    waccReserved ")"
    return (pairElem1, pairElem2)


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-elem-type> ::= <base-type> | <array-type> | 'pair' :::::::::::::: --
pPairElemType :: Parser PairElemType
pPairElemType
    =  liftM BasePairElemType  pBaseType
   <|> liftM ArrayPairElemType pArrayType
   <|> ( waccReserved "null" >> return PairPairElemType )


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <expr> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pExpr :: Parser Expr
pExpr = buildExpressionParser waccOperators pExpr'

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


pIntLiterExpr :: Parser Expr         -- <int-liter>
pIntLiterExpr = liftM IntLiterExpr pIntLiter


pBoolLiterExpr :: Parser Expr        -- <bool-liter>
pBoolLiterExpr
    =  ( waccReserved "true"  >> return ( BoolLiterExpr True  ) )
   <|> ( waccReserved "false" >> return ( BoolLiterExpr False ) )


pCharLiterExpr :: Parser Expr        -- <char-liter>
pCharLiterExpr = liftM CharLiterExpr anyChar


pStrLiterExpr :: Parser Expr         -- <str-liter>
pStrLiterExpr = liftM StrLiterExpr $ many anyChar


pPairLiterExpr :: Parser Expr        -- <pair-liter>
pPairLiterExpr = liftM PairLiterExpr pPairLiter


pIdentExpr :: Parser Expr            -- <ident>
pIdentExpr = liftM IdentExpr waccIdentifier


pArrayElemExpr :: Parser Expr        -- <array-elem>
pArrayElemExpr = liftM ArrayElemExpr pArrayElem


pUnaryOperExpr :: Parser Expr        -- <unary-oper> <expr>
pUnaryOperExpr
    =  pUnaryOperExp' "!"   NotUnOp
   <|> pUnaryOperExp' "len" LenUnOp
   <|> pUnaryOperExp' "ord" OrdUnOp
   <|> pUnaryOperExp' "chr" ChrUnOp
   <|> pUnaryOperExp' "-"   NegUnOp

pUnaryOperExp' string op =
    waccReservedOp string >> liftM (UnaryOperExpr op) pExpr

pBinaryOperExpr :: Parser Expr       -- <expr> <binary-oper> <expr>
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

pBinaryOperExp' string op = do
    waccReservedOp string
    expr   <- pExpr
    expr'  <- pExpr
    return $ BinaryOperExpr op expr expr


pParenthesised :: Parser Expr        -- '(' <expr> ')'
pParenthesised = waccParens pExpr


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <array-elem> ::= <ident> '[' <expr> ']' ::::::::::::::::::::::::::::::: --
pArrayElem :: Parser ArrayElem
pArrayElem = do
    ident <- waccIdentifier
    char '['
    expr <- pExpr
    char ']'
    return $ ArrayElem ident expr


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <int-liter> ::= <int-sign>? <digit>+ :::::::::::::::::::::::::::::::::: --
pIntLiter :: Parser IntLiter
pIntLiter = do
    intSign <- pIntSign
    digits  <- waccInteger
    return $ IntLiter intSign digits


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <int-sign> ::= '+' | '-' :::::::::::::::::::::::::::::::::::::::::::::: --
pIntSign :: Parser ( Maybe IntSign )
pIntSign
    =  ( waccReservedOp "+" >> return ( Just Plus  ) )
   <|> ( waccReservedOp "-" >> return ( Just Minus ) )
   <|> return Nothing


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']' ::::::::::::::::::: --
pArrayLiter :: Parser ArrayLiter
pArrayLiter = many pExpr


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <pair-liter> ::= 'null' ::::::::::::::::::::::::::::::::::::::::::::::: --
pPairLiter :: Parser PairLiter
pPairLiter = waccReserved "null" >> return Null



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

