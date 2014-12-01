module Wacc.Syntax.Parser where

import Wacc.Data.DataTypes
import Wacc.Syntax.LanguageDef

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Control.Applicative hiding ( (<|>) , many   )
import Control.Monad              ( liftM , liftM2 )
import Control.Monad.Fix          ( fix            )


-- ************************************************************************** --
-- **************************                   ***************************** --
-- **************************    WACC Parser    ***************************** --
-- **************************                   ***************************** -- 
-- ************************************************************************** --
-- A set of Parsec lexers and parsers to construct the AST of a WACC program. --


-- | Program ............................................................  31 --    
-- | Statements .........................................................  88 -- 
-- | Types .............................................................. 217 -- 
-- | Expressions ........................................................ 256 -- 
-- | Identifiers, literals .............................................. 312 -- 

-- | Utils .............................................................. 377 --
-- | Final parser function .............................................. 405 --


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Program ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- <program> ::= 'begin' <func>* <stat> 'end'
pProgram :: Parser Program
pProgram = do
    waccWhiteSpace
    waccReserved "begin"
    funcs <- many ( try pFunc <?> "function" ) 
    waccWhiteSpace
    body  <- pStat
    waccReserved "end"
    return $ Program funcs body -- Empty

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- <func> ::= <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end'
pFunc :: Parser Func
pFunc = do
    ftype  <- pType <?> "type"
    waccWhiteSpace
    name   <- waccIdentifier
    params <- waccParens pParamList
    waccReserved "is"
    body   <- pStat
    waccReserved "end"

    -- | Control flow in functions must eventually reach a return or exit stat
    let returnsOrExits x = case x of                
            ReturnStat  _ _      -> True  
            ExitStat    _ _      -> True   
            ScopedStat  s        -> returnsOrExits s        
            WhileStat   _ s _    -> returnsOrExits s
            SeqStat     _ s      -> returnsOrExits s
            IfStat      _ s s' _ -> and $ map returnsOrExits [ s , s' ]
            _                    -> False 
    
    if returnsOrExits body 
        then return $ Func ftype name params body Empty
        else fail "No Reachable Return/Exit Statement" 
    
-- <param-list> ::= <param> (',' <param>)* 
pParamList :: Parser ParamList
pParamList = waccCommaSep pParam


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- <param> ::= <type> <ident> 
pParam :: Parser Param
pParam = do
    ptype <- pType
    waccWhiteSpace
    pname <- waccIdentifier
    return $ Param ptype pname


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Statements :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- <stat> 
pStat :: Parser Stat
pStat = pStat' >>= \stat -> 
    -- | First parse a statement `stat` with `pStat'`.
    --   Then `option` will try to parse a `waccSemicolon` and if it 
    --   finds one then it will go on and parse a second statement with `pStat`
    --   and place it inside a `SeqStat` together with `stat`.
    --   If it can't parse a `waccSemicolon` it will fail and return `stat`.
    option stat $ waccSemicolon >> liftM ( SeqStat stat ) pStat 

    where

        pStat' = choice 
            [ pWaccWord   "skip"    SkipStat
            , pSimpleStat "free"    FreeStat   
            , pSimpleStat "return"  ReturnStat 
            , pSimpleStat "exit"    ExitStat   
            , pSimpleStat "print"   PrintStat  
            , pSimpleStat "println" PrintlnStat
            , pReadStat
            , pDeclareStat
            , pAssignStat
            , pIfStat
            , pWhileStat
            , pScopedStat ]
        
        pSimpleStat key constr = do
            waccReserved key 
            expr <- pExpr
            return $ constr expr Empty

        pReadStat = do 
            waccReserved "read"
            lhs <- pAssignLhs
            return $  ReadStat lhs Empty

        pDeclareStat = do -- <type> <ident> '=' <assign-rhs>
            stype <- pType
            waccWhiteSpace
            ident <- waccIdentifier 
            waccWhiteSpace
            waccReservedOp "="
            waccWhiteSpace
            rhs   <- pAssignRhs
            return $ DeclareStat stype ident rhs Empty
    
        pAssignStat = do -- <assign-lhs> = <assign-rhs>
            lhs <- pAssignLhs
            waccWhiteSpace
            waccReservedOp "="
            waccWhiteSpace
            rhs <- pAssignRhs
            return $ AssignStat lhs rhs Empty
    
        pIfStat = do -- 'if' <expr> 'then' <stat> 'else' <stat> 'fi'
            waccReserved "if"
            expr  <- pExpr
            waccReserved "then"
            stat  <- pStat
            waccReserved "else"
            stat' <- pStat
            waccReserved "fi"
            return $ IfStat expr stat stat' Empty
    
        pWhileStat = do -- 'while' <expr> 'do' <stat> 'done'
            waccReserved "while"
            expr <- pExpr
            waccReserved "do"
            stat <- pStat
            waccReserved "done"
            return $ WhileStat expr stat Empty
    
        pScopedStat = do -- 'begin' <stat> 'end'
            waccReserved "begin"
            stat <- pStat
            waccReserved "end"
            return $ ScopedStat stat


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- <assign-lhs> ::= <ident> | <pair-elem> | <array-elem> 
pAssignLhs :: Parser AssignLhs
pAssignLhs = choice 
    [ try $ liftM LhsArrayElem pArrayElem
    ,       liftM LhsIdent     waccIdentifier
    ,       liftM LhsPairElem  pPairElem ]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- <assign-rhs>
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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- <arg-list> ::= <expr> (',' <expr>)*
pArgList :: Parser ArgList
pArgList = waccCommaSep pExpr

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- <pair-elem> ::= 'fst' <expr> | 'snd' <expr'
pPairElem :: Parser PairElem
pPairElem
    =  pWaccLift "fst" Fst pExpr
   <|> pWaccLift "snd" Snd pExpr 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Types ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- <type> ::= <base-type> | <array-type> | <pair-type>
-- | For easier management of datatypes, they are represented linearly.

pType :: Parser Type
pType = do 
    base  <-  pBaseType <|> pPairType
    fix ( \f -> ( string "[]" >> fmap ArrayType f ) <|> return base )

        where

            -- <base-type> ::= 'int' | 'bool' | 'char' | 'string'
            pBaseType :: Parser Type
            pBaseType = choice 
                [ try $ pWaccWord "int"    IntType    
                      , pWaccWord "bool"   BoolType   
                      , pWaccWord "char"   CharType   
                      , pWaccWord "string" StringType ]

            -- 'pair' '(' <type> ',' <type> ')'
            -- | For easier management of the nested pair case, we chose a Maybe
            -- approach on the pair type, thus considering the nested pair just
            -- another graphical form of the same type.
            pPairType :: Parser Type
            pPairType = do
                waccReserved "pair"
                waccParens $ do
                    pairElem  <- pPairElemType
                    waccComma
                    pairElem' <- pPairElemType
                    return $ PairType ( Just ( pairElem , pairElem' ) )

            pPairElemType :: Parser Type
            pPairElemType = pWaccWord "pair" ( PairType Nothing ) <|> pType


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Expressions ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- <expr>
pExpr :: Parser Expr
pExpr = buildExpressionParser waccOperators pExpr'

    where

        pExpr' = choice 
            [ waccParens pExpr
            , try $ liftM ArrayElemExpr pArrayElem      
            ,       liftM IdentExpr     waccIdentifier
            ,       liftM BoolLiterExpr pBoolLiter
            ,       liftM CharLiterExpr pCharLiter
            ,       liftM StrLiterExpr  pStrLiter
            ,       pWaccWord "null"    PairLiterExpr 
            ,       pBinaryOperExpr
            ,       pUnaryOperExpr 
            ,       liftM IntLiterExpr  pPosIntLiter
            ] <?> "pExpr"

        pUnaryOperExpr = choice
            [ pUnOp    "!"   NotUnOp
            , pUnOp    "len" LenUnOp
            , pUnOp    "ord" OrdUnOp
            , pUnOp    "chr" ChrUnOp
            , pNegUnOp "-"   NegUnOp ]
                  
        pPosIntLiter :: Parser IntLiter
        pPosIntLiter = do
          int <- waccInteger
          if   int > 2^31
              then fail $ "Positive Integer Overflow @" ++ show int 
              else return int

        pNegUnOp str op = do 
            waccReservedOp str
            expr <- pExpr

            case expr of 
                IntLiterExpr int -> 
                    if int > 2^31 
                        then fail   $ "Negative Integer Overflow @" ++ show int 
                        else return $ UnaryOperExpr op expr 
                _                ->  return $ UnaryOperExpr op expr 

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


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- :: <array-elem> ::= <ident> '[' <expr> ']'
pArrayElem :: Parser ArrayElem
pArrayElem = do 
  ident <- waccIdentifier
  dims  <- many1 $ waccBrackets pExpr 
  return $ ArrayElem ident dims


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Literals :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- <bool-liter> ::= 'true' | 'false' 
pBoolLiter :: Parser BoolLiter
pBoolLiter 
	  =  pWaccWord "true" True 
   <|> pWaccWord "false" False 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- <char-liter> ::= ''' <char> ''' 
pCharLiter :: Parser CharLiter
pCharLiter = do
  c <- lookAhead waccCharLiter
  if      c `elem` "\\\"\'" -- If char was one of "\' 
      then do char  '\''    -- Then make sure it was escaped correctly
              char  '\\'
              oneOf "\\\"\'"
              char  '\''
              waccWhiteSpace
              return c
      else do waccCharLiter >>= return 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- <str-liter> ::= ''' <char>* '''
pStrLiter :: Parser StrLiter
pStrLiter = waccStrLiter

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']' 
pArrayLiter :: Parser ArrayLiter
pArrayLiter = waccBrackets $ waccCommaSep pExpr 




-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Utils ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | If the word provided is a reserved word of the wacc language then its 
--   representation in the AST is just the value provided
--   This function is used to simplify expressions of the sort:
--   pData key value = do
--     waccReserved key
--     return value
pWaccWord :: String -> a -> Parser a
pWaccWord word value = waccReserved word >> return value

-- | Similar to pWaccWord except it uses waccReservedOp
pWaccOp :: String -> a -> Parser a
pWaccOp op value = waccReservedOp op >> return value

-- | Matches the reserved word and perfrorms liftM
--   This function is used to simplify expressions of the sort:
--   pData = do
--     waccReserved word
--     value <- parser
--     return $ Data value
pWaccLift :: String -> ( a -> b ) -> Parser a -> Parser b
pWaccLift word f p = waccReserved word >> liftM f p



-- ************************************************************************** --
-- **************************                   ***************************** --
-- **************************   Final Parsers   ***************************** --
-- **************************                   ***************************** -- 
-- ************************************************************************** --

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


-- | This function will run the parser, but additionally fail if it doesn't
-- consume all the input.
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse ( p <* eof ) ""


-- | This function will apply the parser, then also return any left over
-- input which wasn't parsed.
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ( (,) <$> p <*> leftOver ) ""
  where leftOver = manyTill anyToken eof



-- (⋟﹏⋞) = error 
-- (✿__✿) = error
