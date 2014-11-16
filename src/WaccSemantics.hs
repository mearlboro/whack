module WaccSemantics where

import Data.Maybe ( isNothing , fromMaybe , fromJust )
import Data.Map   ( empty )
import Control.Applicative hiding ( empty )
import Data.Char  ( isSpace )

import WaccParser
import WaccExamplesTester
import WaccDataTypes
import WaccSymbolTable

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Data Definitions :::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | PROGRAM represents the Program AST augmented with the global scope
data PROGRAM = 
    PROGRAM [ FUNC ] STAT Scope 
    deriving ( Show )


-- | FUNC also contains the AST for Function and introduces its own scope
data FUNC = 
    FUNC Type Ident ParamList STAT Scope 
    deriving ( Show )

-- | Some statement (while, if, scoped) introduce a new scope.
-- | All statements have a reference to the current scope they are in
data STAT 
  = SKIPstat      
  | FREEstat    Expr      Scope                                               
  | RETURNstat  Expr      Scope                                              
  | EXITstat    Expr      Scope                                               
  | PRINTstat   Expr      Scope                                               
  | PRINTLNstat Expr      Scope                        
  | SCOPEDstat  STAT                             
  | READstat    AssignLhs Scope                         
  | WHILEstat   Expr      STAT      Scope                            
  | SEQstat     STAT      STAT                                  
  | ASSIGNstat  AssignLhs AssignRhs Scope                    
  | IFstat      Expr      STAT      STAT      Scope     
  | DECLAREstat Type      Ident     AssignRhs Scope 
  deriving ( Show )

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Building the PROGRAM :::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- 1. The Global Scope in all of its majesty
-- 2. Build the main statement thus updating the global scope
-- 3. Add all function identifiers
-- 4. Build all the program functions
buildPROGRAM :: Program -> PROGRAM 
buildPROGRAM p@( Program funcs main ) = PROGRAM funcs' main' globalScope''
    where
        globalScope              = ST Empty empty
        ( globalScope' , main' ) = buildSTAT main globalScope
        globalScope''            = foldr addFunc globalScope' funcs
        funcs'                   = map ( flip buildFUNC globalScope'' ) funcs 
   

-- | Given a function and its parent scope (the global scope), build its 
--   semantically-augmented version (FUNC) and in the process create 
--   its own childScope(s). Also updates the global scope
--   1. Create function scope, parented by the global scope
--   2. Build the body STATement, updating the function scope 
--   3. Add function parameters to the function scope
buildFUNC :: Func -> Scope -> FUNC 
buildFUNC func@( Func ftype name plist body ) globalScope = func'
    where
        functionScope              = ST globalScope empty -- New scope
        ( functionScope' , body' ) = buildSTAT body functionScope 
        functionScope''            = foldr addParam functionScope' plist
        func'                      = FUNC ftype name plist body' functionScope''


-- | Given a statement and the currentScope it is in, produce a STAT 
--   with its own scope, if needed. The current scope gets populated by 
--   the identifiers, if any, declared in a DeclareStat statement.
buildSTAT                            :: Stat -> Scope -> ( Scope , STAT )  
buildSTAT   ( SkipStat            )  =  flip (,)          SKIPstat 
buildSTAT   ( FreeStat    expr    )  =  buildSimpleStat ( FREEstat    expr    )
buildSTAT   ( ReturnStat  expr    )  =  buildSimpleStat ( RETURNstat  expr    )
buildSTAT   ( ExitStat    expr    )  =  buildSimpleStat ( EXITstat    expr    )
buildSTAT   ( PrintStat   expr    )  =  buildSimpleStat ( PRINTstat   expr    )
buildSTAT   ( PrintlnStat expr    )  =  buildSimpleStat ( PRINTLNstat expr    )
buildSTAT   ( ReadStat    lhs     )  =  buildSimpleStat ( READstat    lhs     )
buildSTAT   ( AssignStat  lhs rhs )  =  buildSimpleStat ( ASSIGNstat  lhs rhs )
buildSTAT s@( DeclareStat _ _ _   )  =  buildDECLAREstat  s                 
buildSTAT s@( SeqStat     _ _     )  =  buildSEQstat      s                 
buildSTAT s@( ScopedStat  _       )  =  buildSCOPEDstat   s                 
buildSTAT s@( WhileStat   _ _     )  =  buildWHILEstat    s                 
buildSTAT s@( IfStat      _ _ _   )  =  buildIFstat       s                 


buildSimpleStat stat parentScope = ( parentScope , stat parentScope )


-- A scoped statement introduces its own local childScope, enclosed
-- by the parentScope, and leaves the parentScope unchanged.
buildSCOPEDstat ( ScopedStat body ) parentScope = 
    let childScope = ST parentScope empty 
        ( _childScope' , body' ) = buildSTAT body childScope
    in  ( parentScope  , SCOPEDstat body' )


-- Here we add a new variable to the parentScope and return it updated
buildDECLAREstat ( DeclareStat dtype ident rhs ) parentScope = 
    let   parentScope' = addVariable ident dtype parentScope
    in  ( parentScope', DECLAREstat dtype ident rhs parentScope' )


buildWHILEstat ( WhileStat expr body ) parentScope = 
    let loopScope = ST parentScope empty 
        ( loopScope'  , body' ) = buildSTAT body loopScope 
    in  ( parentScope , WHILEstat expr body' parentScope )


buildIFstat ( IfStat expr thenBody elseBody ) parentScope =
    let thenScope = ST parentScope empty 
        elseScope = ST parentScope empty
        ( thenScope'  , thenBody' ) = buildSTAT thenBody thenScope
        ( elseScope'  , elseBody' ) = buildSTAT elseBody elseScope
    in  ( parentScope , IFstat expr thenBody' elseBody' parentScope ) 


buildSEQstat ( SeqStat first second ) parentScope = 
    let ( parentScope'  , first'  ) = buildSTAT first  parentScope
        ( parentScope'' , second' ) = buildSTAT second parentScope'
    in  ( parentScope'' , SEQstat first' second' )


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Semantic Analisys ::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- * To look in the current scope use >findIdent ident scope 
-- * To look in the parent's scope as well use >findIdent' ident scope

-- | Check semantics of a program
checkProgram                                     :: PROGRAM -> SemanticErr 
checkProgram ( PROGRAM funcs body globalScope )  =  error "TODO"


-- | Check semantics of a function
checkFunc                                               :: FUNC -> SemanticErr 
checkFunc ( FUNC ftype name plist body functionScope )  =  error "TODO" 


-- | Check semantics of a statement 
checkStat       :: STAT -> SemanticErr 
checkStat stat  =  case stat of 
    SKIPstat                              -> error "TODO"
    FREEstat      expr  scope             -> error "TODO"                
    RETURNstat    expr  scope             -> error "TODO"                
    EXITstat      expr  scope             -> error "TODO"                
    PRINTstat     expr  scope             -> error "TODO"                
    PRINTLNstat   expr  scope             -> error "TODO"        
    SCOPEDstat    stat                    -> error "TODO"        
    READstat      lhs   table             -> error "TODO"         
    WHILEstat     expr  body  scope       -> error "TODO"          
    SEQstat       stat  stat'             -> error "TODO"          
    ASSIGNstat    lhs   rhs   scope       -> error "TODO"          
    IFstat        expr  sthen selse scope -> error "TODO"   
    DECLAREstat   stype ident rhs   scope -> error "TODO"


-- | Check semantics of an expression
checkExpr             :: Expr -> Scope -> SemanticErr 
checkExpr expr scope  =  case expr of 
    BoolLiterExpr     bool             -> Nothing     
    CharLiterExpr     char             -> Nothing     
    IntLiterExpr      int              -> Nothing
    StrLiterExpr      str              -> Nothing
    PairLiterExpr     pair             -> Nothing
    IdentExpr         ident            -> if not $ isNothing $ findIdent' ident scope then Nothing else Just "Variable undeclared"
    UnaryOperExpr     op    expr       -> checkUnaryOp op expr scope
    ParenthesisedExpr expr             -> checkExpr expr scope
    ArrayElemExpr     arr              -> checkArrayElem arr scope
    BinaryOperExpr    op    expr expr' -> checkBinaryOp op expr expr' scope

checkBinaryOp :: BinaryOper -> Expr -> Expr -> Scope -> SemanticErr
checkBinaryOp op expr expr' scope
  | elem op [AddBinOp, SubBinOp, MulBinOp, DivBinOp, ModBinOp] 
                   = if (checkBinaryExprs expr expr' scope &&
                          getExprType expr scope == Just IntType)
                      then Nothing else Just "Ill-formed Binary Operator Expression" 
  | elem op [AndBinOp, OrrBinOp]
                   = if (checkBinaryExprs expr expr' scope &&
                          getExprType expr scope == Just BoolType)
                      then Nothing else Just "Ill-formed Binary Operator Expression"
  | elem op [LsBinOp, GtBinOp, LEBinOp, GEBinOp, EqBinOp, NEBinOp]
                   = if (checkBinaryExprs expr expr' scope &&
                          (getExprType expr scope == Just IntType ||
                           getExprType expr scope == Just CharType))
                      then Nothing else Just "Ill-formed Binary Operator Expression"

checkBinaryExprs :: Expr -> Expr -> Scope -> Bool
checkBinaryExprs expr expr' scope 
  = (isNothing $ checkExpr expr  scope) &&
    (isNothing $ checkExpr expr' scope) &&
    (getExprType expr scope == getExprType expr' scope) &&
    (getExprType expr scope == Just IntType)


checkUnaryOp :: UnaryOper -> Expr -> Scope -> SemanticErr
checkUnaryOp op expr scope
  | op == NotUnOp = if (getExprType expr scope == Just BoolType) && 
                                             (isNothing $ checkExpr expr scope) 
                      then Nothing else Just "Bad use of NOT Unary Operator"
  | op == LenUnOp = if (getExprType expr scope == Just ArrayType {}) &&
                                             (isNothing $ checkExpr expr scope)
                      then Nothing else Just "Bad use of LEN Unary Operator"
  | op == OrdUnOp = if (getExprType expr scope == Just CharType) &&
                                              (isNothing $ checkExpr expr scope)
                      then Nothing else Just "Bad use of ORD Unary Operator"
  | op == ChrUnOp = if (getExprType expr scope == Just IntType)  &&
                                              (isNothing $ checkExpr expr scope)
                      then Nothing else Just "Bad use of CHR Unary Operator"
  | op == NegUnOp = if (getExprType expr scope == Just IntType)  && 
                                              (isNothing $ checkExpr expr scope)
                      then Nothing else Just "Bad use of NEG Unary Operator"

checkArrayElem :: ArrayElem -> Scope -> SemanticErr
checkArrayElem (ArrayElem ident exprs) scope
 = if ( checkArrExprs exprs scope ) && ( countNestedArray (fromMaybe IntType (findType ident scope)) == length exprs )
    then Nothing else Just "Invalid ArrayElemExpr"


--TODO: This is a placeholder
--findType :: Ident -> Scope -> Maybe Type
--findType ident scope = Just ArrayType IntType
--PLACEHOLDER

countNestedArray :: Type -> Int
countNestedArray ( ArrayType arrType ) = countNestedArray + 1
countNestedArray _ = 0

checkArrExprs :: [Expr] -> Scope -> Bool
checkArrExprs [] _ = True
checkArrExprs (expr:exprs) scope
  | checkExpr expr scope      == Nothing 
    && getExprType expr scope == Just IntType
                                             = checkArrExprs exprs scope
  | otherwise                                = False  


-- |Gets the type of an expression
getExprType :: Expr -> Scope -> Maybe Type
getExprType expr scope = case expr of
    BoolLiterExpr     bool               -> Just BoolType 
    CharLiterExpr     char               -> Just CharType   
    IdentExpr         ident              -> findFirst $ findIdent' ident scope
    UnaryOperExpr     NotUnOp    expr    -> Just BoolType
    UnaryOperExpr     LenUnOp    expr    -> Just IntType
    UnaryOperExpr     OrdUnOp    expr    -> Just IntType  
    UnaryOperExpr     ChrUnOp    expr    -> Just CharType
    UnaryOperExpr     NegUnOp    expr    -> Just IntType
    ParenthesisedExpr expr               -> getExprType expr scope   
    IntLiterExpr      int                -> Just IntType   
    StrLiterExpr      str                -> Just StringType   
    PairLiterExpr     pair               -> Just NullType
    ArrayElemExpr (ArrayElem ident expr) -> findFirst $ findIdent' ident scope
    BinaryOperExpr AddBinOp _ _          -> Just IntType
    BinaryOperExpr SubBinOp _ _          -> Just IntType                                  
    BinaryOperExpr MulBinOp _ _          -> Just IntType                                   
    BinaryOperExpr DivBinOp _ _          -> Just IntType                                    
    BinaryOperExpr ModBinOp _ _          -> Just IntType                                    
    BinaryOperExpr _        _ _          -> Just BoolType

findFirst :: Maybe Identifier -> Maybe Type
findFirst Nothing              = Nothing
findFirst ( Just (myType, _) ) = Just myType

-- Nothing means no semantic error, Just contains the semantic error message
type SemanticErr = Maybe String 

printError      :: SemanticErr -> IO ()
printError err  =  putStrLn $ case err of
    Nothing  -> "No Semantic Errors"
    Just msg -> "SemanticError: " ++ msg 

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Testing ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --


begin = do 
    putStrLn $ "Enter the name of a .wacc program to read, parse and test."
    putStrLn $ "The file should be located in wacc_examples/semanticErr"
    path <- getLine 
    program <- parseOne' $ "wacc_examples/semanticErr/" ++ path
    let pROGRAM = buildPROGRAM program 
    putStrLn $ "semantically-augmented PROGRAM:\n" ++ show pROGRAM 
