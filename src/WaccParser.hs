-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 3.1. WACC Parsers ::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

module WaccParser where

import WaccDataTypes
import WaccLanguageDef

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Language       ( haskellDef )
import Control.Applicative hiding ( (<|>) , many )
import Control.Monad              ( liftM , liftM2 )
import Control.Monad.IO.Class     ( liftIO )

--(⋟﹏⋞) = error 

--(✿_✿) = error 

-- |3.1.1 program .......................................................  26 --    
-- |3.1.2 Statements ....................................................  66 -- 
-- |3.1.3 Types ......................................................... 183 -- 
-- |3.1.4 Expressions ................................................... 232 -- 
-- |3.1.5 Identifiers, literals ......................................... 314 -- 


-- |3.1.1 Program 

-- :: <program> ::= 'begin' <func>* <stat> 'end' :::::::::::::::::::::::::::: --
pProgram :: Parser Program
pProgram = do
   waccWhiteSpace
   waccReserved "begin"
   waccWhiteSpace
   funcs <- try $ many ( try pFunc )
   waccWhiteSpace
   stat  <- pStat
   waccReserved "end"
   return $ Program funcs stat -- funcs stat

-- :: <func> ::= <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end' ::::: --
pFunc :: Parser Func
pFunc = do
    ftype <- pType
    waccWhiteSpace
    ident <- waccIdentifier
    --waccWhiteSpace
    pList <- waccParens pParamList
    waccReserved "is"
    stat  <- pStat
    waccReserved "end"
    return $ Func ftype ident pList stat

-- :: <param-list> ::= <param> (',' <param>)* ::::::::::::::::::::::::::::::: --
pParamList :: Parser ParamList
pParamList = waccCommaSep pParam

-- :: <param> ::= <type> <ident> :::::::::::::::::::::::::::::::::::::::::::: --
pParam :: Parser Param
pParam = do
  parType <- pType
  waccWhiteSpace
  ident <- waccIdentifier
  return $ Param parType ident

-- :: <stat> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pStat :: Parser Stat
pStat = pStat' >>= \stat -> 
  -- option x p tries to apply parser p. If p fails without consuming input, 
  -- it returns the value x, otherwise the value returned by p.
  option stat $ waccSemicolon >> liftM ( SeqStat stat ) pStat 

    where

      pStat' 
        = choice 
        [ pWaccWord "skip"    SkipStat
        , pWaccLift "free"    FreeStat    pExpr
        , pWaccLift "return"  ReturnStat  pExpr
        , pWaccLift "exit"    ExitStat    pExpr
        , pWaccLift "print"   PrintStat   pExpr
        , pWaccLift "println" PrintlnStat pExpr
        , pWaccLift "read"    ReadStat    pAssignLhs
        , pDeclareStat
        , pAssignStat
        , pIfStat
        , pWhileStat
        , pScopedStat ]

      pDeclareStat = do    -- <type> <ident> '=' <assign-rhs>
        sType <- pType
        waccWhiteSpace
        ident <- waccIdentifier 
        waccWhiteSpace
        waccReservedOp "="
        waccWhiteSpace
        rhs   <- pAssignRhs
        return $ DeclareStat sType ident rhs

      pAssignStat = do     -- <assign-lhs> = <assign-rhs>
        lhs <- pAssignLhs
        waccWhiteSpace
        waccReservedOp "="
        waccWhiteSpace
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

      pScopedStat = do     -- 'begin' <stat> 'end'
        waccReserved "begin"
        stat <- pStat
        waccReserved "end"
        return $ ScopedStat stat


-- :: <assign-lhs> ::= <ident> | <pair-elem> | <array-elem> ::::::::::::::::: --
pAssignLhs :: Parser AssignLhs
pAssignLhs
    =  choice [ try $ liftM LhsArrayElem pArrayElem
              , try $ liftM LhsIdent     waccIdentifier
              , try $ liftM LhsPairElem  pPairElem ]

-- :: <assign-rhs> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pAssignRhs :: Parser AssignRhs
pAssignRhs
    =  choice 
    [ try $ liftM RhsArrayLiter pArrayLiter    -- <array-liter>
    , try $ liftM RhsPairElem   pPairElem      -- <pair-elem>
    , try $ liftM RhsExpr       pExpr          -- <expr>
    , try pRhsNewPair
    , try pRhsCall ]

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

-- :: <arg-list> ::= <expr> (',' <expr>)* ::::::::::::::::::::::::::::::::::: --
pArgList :: Parser ArgList
pArgList = waccCommaSep pExpr

-- :: <pair-elem> ::= 'fst' <expr> | 'snd' <expr' ::::::::::::::::::::::::::: --
pPairElem :: Parser PairElem
pPairElem
    =  pWaccLift "fst" Fst pExpr
   <|> pWaccLift "snd" Snd pExpr

-- :: <type> ::= <base-type> | <array-type> | <pair-type> ::::::::::::::::::: --
pType :: Parser Type
pType = do 
  base <- liftM TypePair pPairType 
      <|> liftM TypeBase pBaseType 
  dims <- length <$> many ( string "[]" ) 
  return $ if   dims == 0 
           then base 
           else iterate TypeArray base !! dims

--   = Maybe ( Type , Type )
pPairType :: Parser PairType
pPairType = do
    waccReserved "pair"
    waccParens $ do
    pairElem1 <- pPairElemType
    waccComma
    pairElem2 <- pPairElemType
    return $ Just (pairElem1, pairElem2) 

      where

        pPairElemType  :: Parser Type
        pPairElemType  = try $ pWaccWord "pair" ( TypePair Nothing )
                     <|> pType

        --pPairPairElemType :: Parser PairType
        --pPairPairElemType = 
        --    waccReserved "pair" >>  return Nothing


-- :: <pair-type> ::= 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')' :: --   
--pPairType :: Parser PairType  -- Mayeb ( tyoe , type )
--pPairType = do
--    waccReserved "pair"
--    waccParens $ do
--        pairElem  <- pPairElemType 
--        waccComma
--        pairElem' <- pPairElemType
--        return Just $ ( pairElem , pairElem' ) 
--    where
--        pPairElemType :: Parser ( Maybe Type )
--        pPairElemType = try $ pWaccWord "pair" Nothing 
--                     <|> liftM Just pType



-- :: <base-type> ::= 'int' | 'bool' | 'char' | 'string' :::::::::::::::::::: --
pBaseType :: Parser BaseType
pBaseType =  choice [ pWaccWord "int"    IntBaseType    
                    , pWaccWord "bool"   BoolBaseType   
                    , pWaccWord "char"   CharBaseType   
                    , pWaccWord "string" StringBaseType ]

-- :: <expr> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pExpr :: Parser Expr
pExpr = buildExpressionParser waccOperators pExpr'

  where
    pExpr' :: Parser Expr
    pExpr' 
      = choice 
      [ try $ waccParens pExpr
      , try $ liftM ArrayElemExpr pArrayElem      
      , try $ liftM BoolLiterExpr pBoolLiter
      , try $ liftM IntLiterExpr  pIntLiter
      , try $ liftM CharLiterExpr pCharLiter
      , try $ liftM StrLiterExpr  pStrLiter
      , try $ liftM PairLiterExpr pPairLiter
      , try $ liftM IdentExpr     waccIdentifier
      , try pBinaryOperExpr
      , try pUnaryOperExpr ] <?> "pExpr"

-- :: <array-elem> ::= <ident> ('[' <expr> ']')+ ::::::::::::::::::::::::::::::: --
pArrayElem :: Parser ArrayElem
pArrayElem = do 
  ident      <- waccIdentifier
  dimensions <- many1 ( waccBrackets pExpr )
  return $ ArrayElem ident dimensions

-- :: <int-liter> ::= <int-sign>? <digit>+ :::::::::::::::::::::::::::::::::: --
pIntLiter :: Parser IntLiter
pIntLiter = waccInteger

-- :: <bool-liter> ::= 'true' | 'false' ::::::::::::::::::::::::::::::::::::: --
pBoolLiter :: Parser BoolLiter
pBoolLiter =  pWaccWord "true" True <|> pWaccWord "false" False 

-- :: <char-liter> ::= ''' <char> ''' ::::::::::::::::::::::::::::::::::::::: --
pCharLiter :: Parser CharLiter
pCharLiter = waccCharLiter

-- :: <str-liter> ::= ''' <char>* ''' ::::::::::::::::::::::::::::::::::::::: --
pStrLiter :: Parser StrLiter
pStrLiter = waccStrLiter

-- :: <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']' ::::::::::::::::::: --
pArrayLiter :: Parser ArrayLiter
pArrayLiter = waccBrackets $ waccCommaSep pExpr 

-- :: <pair-liter> ::= 'null' ::::::::::::::::::::::::::::::::::::::::::::::: --
pPairLiter :: Parser PairLiter
pPairLiter = pWaccWord "null" Null


-- pIntLiterExpr, pBoolLiterExpr, pCharLiterExpr, pStrLiterExpr,  pPairLiterExpr, 
-- pIdentExpr,    pUnaryOperExpr, pParenthesised, pArrayElemExpr, pBinaryOperExpr
-- :: Parser Expr


pUnaryOperExpr
    = choice
    [ try $ pUnaryOperExp' "!"   NotUnOp
    , try $ pUnaryOperExp' "len" LenUnOp
    , try $ pUnaryOperExp' "ord" OrdUnOp
    , try $ pUnaryOperExp' "chr" ChrUnOp
    , try $ pUnaryOperExp' "-"   NegUnOp ]

    where

        pUnaryOperExp' string op =
            waccReservedOp string >> liftM (UnaryOperExpr op) pExpr

pBinaryOperExpr
    = choice 
    [ try $ pBinaryOperExp' "+"  AddBinOp
    , try $ pBinaryOperExp' "-"  SubBinOp
    , try $ pBinaryOperExp' "*"  MulBinOp
    , try $ pBinaryOperExp' "/"  DivBinOp
    , try $ pBinaryOperExp' "%"  ModBinOp
    , try $ pBinaryOperExp' "&&" AndBinOp
    , try $ pBinaryOperExp' "||" OrrBinOp
    , try $ pBinaryOperExp' "<=" LEBinOp
    , try $ pBinaryOperExp' "<"  LsBinOp
    , try $ pBinaryOperExp' ">"  GtBinOp
    , try $ pBinaryOperExp' ">=" GEBinOp
    , try $ pBinaryOperExp' "==" EqBinOp
    , try $ pBinaryOperExp' "!=" NEBinOp ]
    where
        pBinaryOperExp' str op =
            waccReservedOp str >> liftM2 ( BinaryOperExpr op ) pExpr pExpr




pComment :: Parser Comment 
pComment = do 
    char '#'
    skipMany ( noneOf "\r\n" )
    return ""

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
