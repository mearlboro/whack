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
    funcs <- many $ try pFunc 
    waccWhiteSpace
    body  <- pStat
    waccReserved "end"
    return $ Program funcs body 


-- :: <func> ::= <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end' ::::: --
pFunc :: Parser Func
pFunc = do
    ftype  <- pType
    waccWhiteSpace
    name   <- waccIdentifier
    params <- waccParens pParamList
    waccReserved "is"
    body   <- pStat
    waccReserved "end"

    -- Control flow in functions must eventually reach a return or exit stat
    let returnsOrExits x = case x of                
            ReturnStat  _      -> True  
            ExitStat    _      -> True   
            ScopedStat  s      -> returnsOrExits s        
            WhileStat   _ s    -> returnsOrExits s
            SeqStat     _ s    -> returnsOrExits s
            IfStat      e s s' -> and $ map returnsOrExits $ s:s':[]
            _                  -> False 

    if   returnsOrExits body 
    then return $ Func ftype name params body
    else fail "No Reachable Return/Exit Statement" 


-- :: <param-list> ::= <param> (',' <param>)* ::::::::::::::::::::::::::::::: --
pParamList :: Parser ParamList
pParamList = waccCommaSep pParam


-- :: <param> ::= <type> <ident> :::::::::::::::::::::::::::::::::::::::::::: --
pParam :: Parser Param
pParam = do
    ptype <- pType
    waccWhiteSpace
    pname <- waccIdentifier
    return $ Param ptype pname


-- :: <stat> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pStat :: Parser Stat
pStat = pStat' >>= \stat -> 
    -- First parse a statement `stat` with `pStat'`.
    -- Then `option` will try to parse a `waccSemicolon` and if it 
    -- finds one then it will go on and parse a second statement with `pStat`
    -- and place it inside a `SeqStat` together with `stat`.
    -- If it can't parse a `waccSemicolon` it will fail and return `stat`.
    option stat $ waccSemicolon >> liftM ( SeqStat stat ) pStat 

    where

        pStat' = choice 
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
    
        pDeclareStat = do -- <type> <ident> '=' <assign-rhs>
            stype <- pType
            waccWhiteSpace
            ident <- waccIdentifier 
            waccWhiteSpace
            waccReservedOp "="
            waccWhiteSpace
            rhs   <- pAssignRhs
            return $ DeclareStat stype ident rhs
    
        pAssignStat = do -- <assign-lhs> = <assign-rhs>
            lhs <- pAssignLhs
            waccWhiteSpace
            waccReservedOp "="
            waccWhiteSpace
            rhs <- pAssignRhs
            return $ AssignStat lhs rhs
    
        pIfStat = do -- 'if' <expr> 'then' <stat> 'else' <stat> 'fi'
            waccReserved "if"
            expr  <- pExpr
            waccReserved "then"
            stat  <- pStat
            waccReserved "else"
            stat' <- pStat
            waccReserved "fi"
            return $ IfStat expr stat stat'
    
        pWhileStat = do -- 'while' <expr> 'do' <stat> 'done'
            waccReserved "while"
            expr <- pExpr
            waccReserved "do"
            stat <- pStat
            waccReserved "done"
            return $ WhileStat expr stat
    
        pScopedStat = do -- 'begin' <stat> 'end'
            waccReserved "begin"
            stat <- pStat
            waccReserved "end"
            return $ ScopedStat stat


-- :: <assign-lhs> ::= <ident> | <pair-elem> | <array-elem> ::::::::::::::::: --
pAssignLhs :: Parser AssignLhs
pAssignLhs = choice 
    [ try $ liftM LhsArrayElem pArrayElem
    ,       liftM LhsIdent     waccIdentifier
    ,       liftM LhsPairElem  pPairElem ]

-- :: <assign-rhs> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pAssignRhs :: Parser AssignRhs
pAssignRhs = choice 
    [ liftM RhsArrayLiter pArrayLiter 
    , liftM RhsPairElem   pPairElem   
    , liftM RhsExpr       pExpr       
    , pRhsNewPair
    , pRhsCall ]

    where

        pRhsNewPair = do 
            waccReserved "newpair"
            waccParens $ do
                expr  <- pExpr
                waccComma
                expr' <- pExpr
                return $ RhsNewPair expr expr'
        
        pRhsCall = do 
          waccReserved "call"
          fname <- waccIdentifier
          args  <- waccParens pArgList
          return $ RhsCall fname args


-- :: <arg-list> ::= <expr> (',' <expr>)* ::::::::::::::::::::::::::::::::::: --
pArgList :: Parser ArgList
pArgList = waccCommaSep pExpr


-- :: <pair-elem> ::= 'fst' <expr> | 'snd' <expr' ::::::::::::::::::::::::::: --
pPairElem :: Parser PairElem
pPairElem = pWaccLift "fst" Fst pExpr <|> pWaccLift "snd" Snd pExpr


-- :: <type> ::= <base-type> | <array-type> | <pair-type> ::::::::::::::::::: --
pType :: Parser Type
pType = do 
    base <- liftM TypePair pPairType <|> liftM TypeBase pBaseType 
    dims <- length <$> many ( string "[]" ) 

    if   dims == 0 
    then return   base 
    else return $ iterate TypeArray base !! dims


--   = Maybe ( Type , Type )
pPairType :: Parser PairType
pPairType = do
    waccReserved "pair"
    waccParens $ do
        pelem  <- pPairElemType
        waccComma
        pelem' <- pPairElemType
        return $ Just ( pelem , pelem' ) 

    where

        pPairElemType  :: Parser Type
        pPairElemType  =  pWaccWord "pair" ( TypePair Nothing ) <|> pType



-- :: <base-type> ::= 'int' | 'bool' | 'char' | 'string' :::::::::::::::::::: --
pBaseType :: Parser BaseType
pBaseType = choice 
    [ pWaccWord "int"    IntBaseType    
    , pWaccWord "bool"   BoolBaseType   
    , pWaccWord "char"   CharBaseType   
    , pWaccWord "string" StringBaseType ]

-- :: <expr> :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
pExpr :: Parser Expr
pExpr = buildExpressionParser waccOperators pExpr'

    where

        pExpr' = choice 
            [ waccParens pExpr
            , try $ liftM ArrayElemExpr pArrayElem      
            ,       liftM BoolLiterExpr pBoolLiter
            ,       liftM IntLiterExpr  pIntLiter
            ,       liftM CharLiterExpr pCharLiter
            ,       liftM StrLiterExpr  pStrLiter
            ,       liftM PairLiterExpr pPairLiter
            ,       liftM IdentExpr     waccIdentifier
            , pBinaryOperExpr
            , pUnaryOperExpr 
            ] <?> "pExpr"


        pUnaryOperExpr = choice
            [ pUnOp "!"   NotUnOp
            , pUnOp "len" LenUnOp
            , pUnOp "ord" OrdUnOp
            , pUnOp "chr" ChrUnOp
            , pUnOp "-"   NegUnOp ]
        
            where
        
                pUnOp string op = do 
                    waccReservedOp string
                    liftM ( UnaryOperExpr op ) pExpr
        
        pBinaryOperExpr = choice 
            [ pBinOp "+"  AddBinOp
            , pBinOp "-"  SubBinOp
            , pBinOp "*"  MulBinOp
            , pBinOp "/"  DivBinOp
            , pBinOp "%"  ModBinOp
            , pBinOp "&&" AndBinOp
            , pBinOp "||" OrrBinOp
            , pBinOp "<=" LEBinOp
            , pBinOp "<"  LsBinOp
            , pBinOp ">"  GtBinOp
            , pBinOp ">=" GEBinOp
            , pBinOp "==" EqBinOp
            , pBinOp "!=" NEBinOp ]
            
            where
                
                pBinOp str op = do 
                    waccReservedOp str 
                    liftM2 ( BinaryOperExpr op ) pExpr pExpr




-- :: <array-elem> ::= <ident> ('[' <expr> ']')+ ::::::::::::::::::::::::::::::: --
pArrayElem :: Parser ArrayElem
pArrayElem = do 
  ident <- waccIdentifier
  dims  <- many1 $ waccBrackets pExpr 
  return $ ArrayElem ident dims


-- :: <int-liter> ::= <int-sign>? <digit>+ :::::::::::::::::::::::::::::::::: --
pIntLiter :: Parser IntLiter
pIntLiter = do
  -- −2^31 to 2^31 − 1 inclusive.
  int <- waccInteger
  if   int >= -2^31 && int <= 2^31-1
  then return int
  else fail "Integer Out Of Bounds" 


-- :: <bool-liter> ::= 'true' | 'false' ::::::::::::::::::::::::::::::::::::: --
pBoolLiter :: Parser BoolLiter
pBoolLiter =  pWaccWord "true" True <|> pWaccWord "false" False 


-- :: <char-liter> ::= ''' <char> ''' ::::::::::::::::::::::::::::::::::::::: --
pCharLiter :: Parser CharLiter
pCharLiter = waccCharLiter 
--do 
--  c <- waccCharLiter
--  if   c `elem` "\"\'\\"
--  then fail "Unescaped Character"
--  else return c

-- :: <str-liter> ::= ''' <char>* ''' ::::::::::::::::::::::::::::::::::::::: --
pStrLiter :: Parser StrLiter
pStrLiter = waccStrLiter

-- :: <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']' ::::::::::::::::::: --
pArrayLiter :: Parser ArrayLiter
pArrayLiter = waccBrackets $ waccCommaSep pExpr 

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
