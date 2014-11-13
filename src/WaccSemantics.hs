module WaccSemantics where

import Data.Maybe ( isNothing , fromMaybe , fromJust )
import Data.Map ( empty )
import Control.Applicative hiding ( (<$>) , empty )

import WaccParser
import WaccExamplesTester
import WaccDataTypes
import WaccSymbolTable

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Data Definitions :::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | PROGRAM represents the Program AST augmented with the global scope
data PROGRAM = PROGRAM [ FUNC ] STAT Scope 

-- | FUNC also contains the AST for Function and introduces its own scope
data FUNC = FUNC Type Ident ParamList STAT Scope 

-- | Some statement (while, if, scoped) introduce a new scope.
-- | All statements have a reference to the current scope.
data STAT 
  = SKIPstat      
  | FREEstat      Expr        Scope                                               
  | RETURNstat    Expr        Scope                                              
  | EXITstat      Expr        Scope                                               
  | PRINTstat     Expr        Scope                                               
  | PRINTLNstat   Expr        Scope                        
  | SCOPEDstat    STAT                               
  | READstat      AssignLhs   Scope                         
  | WHILEstat     Expr        STAT        Scope                            
  | SEQstat       STAT        STAT                                    
  | ASSIGNstat    AssignLhs   AssignRhs   Scope                    
  | IFstat        Expr        STAT        STAT        Scope     
  | DECLAREstat   Type        Ident       AssignRhs   Scope 

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Building the PROGRAM :::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- 1. The Global Scope in all of its majesty
-- 2. Build the main statement thus updating the global scope
buildPROGRAM :: Program -> PROGRAM 
buildPROGRAM p@( Program funcs main ) = PROGRAM funcs' main' globalScope''
    where
 
        globalScope              = ST Empty empty
        ( globalScope' , main' ) = buildSTAT main globalScope

        -- TODO implement with a fold or something clever
        addFuncs []     _funcs scope = ( scope   , _funcs )
        addFuncs (f:fs) _funcs scope = addFuncs fs (_FUNC:_funcs) scope'
            where ( scope'  , _FUNC  ) = buildFUNC f scope

        --g = foldr addFunc func globalScope globalScope' funcs 
        -- Build the program functions, once again updating the global
        -- scope since the function name identifiers will be added by
        -- buildFUNC
        ( globalScope'' , funcs' ) = addFuncs funcs [] globalScope'


-- | Given a function and its parent scope (the global scope), build its 
--   semantically-augmented version (FUNC) and in the process create 
--   its own childScope(s). Also updates the global scope
--   1. Add funcion name to the global scope
--   2. Create function scope, parented by the global scope
--   3. Build the body STATement, updating the function scope 
--   4. Add function parameters to the function scope
buildFUNC :: Func -> Scope -> ( Scope , FUNC )
buildFUNC func@( Func ftype name plist body ) globalScope = result
    where
        globalScope'             = addFunc func globalScope
        functionScope            = ST globalScope' empty -- New scope
        ( functionScope' , body' ) = buildSTAT body functionScope 
        functionScope''          = foldr addParam functionScope' plist
        func'                    = FUNC ftype name plist body' functionScope''
        result = ( globalScope' , func' )


-- | Given a statement and the currentScope it is in, produce a STAT 
--   with its own scope, if needed. The current scope gets populated by 
--   the identifiers, if any, declared or used in the statement.
buildSTAT :: Stat -> Scope -> ( Scope , STAT )  
buildSTAT stat currentScope = case stat of 
    SkipStat            -> ( currentScope , SKIPstat                         )
    FreeStat    expr    -> ( currentScope , FREEstat    expr    currentScope ) 
    ReturnStat  expr    -> ( currentScope , RETURNstat  expr    currentScope ) 
    ExitStat    expr    -> ( currentScope , EXITstat    expr    currentScope ) 
    PrintStat   expr    -> ( currentScope , PRINTstat   expr    currentScope ) 
    PrintlnStat expr    -> ( currentScope , PRINTLNstat expr    currentScope ) 
    ReadStat    lhs     -> ( currentScope , READstat    lhs     currentScope )
    AssignStat  lhs rhs -> ( currentScope , ASSIGNstat  lhs rhs currentScope ) 
    DeclareStat _ _ _   ->   buildDECLAREstat stat currentScope 
    SeqStat     _ _     ->   buildSEQstat     stat currentScope 
    ScopedStat  _       ->   buildSCOPEDstat  stat currentScope 
    WhileStat   _ _     ->   buildWHILEstat   stat currentScope 
    IfStat      _ _ _   ->   buildIFstat      stat currentScope 

-- A scoped statement introduces its own local childScope, enclosed
-- by the parentScope, and leaves the parentscope unchanged.
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
        ( thenScope' , thenBody' ) = buildSTAT thenBody thenScope
        ( elseScope' , elseBody' ) = buildSTAT elseBody elseScope
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
    BoolLiterExpr     bool             -> error "TODO"  
    CharLiterExpr     char             -> error "TODO"   
    IdentExpr         ident            -> error "TODO"  
    UnaryOperExpr     op    expr       -> error "TODO"   
    ParenthesisedExpr expr             -> error "TODO"   
    IntLiterExpr      int              -> error "TODO"   
    StrLiterExpr      str              -> error "TODO"   
    PairLiterExpr     pair             -> error "TODO"     
    ArrayElemExpr     arr              -> error "TODO"
    BinaryOperExpr    op    expr expr' -> error "TODO"     


-- Nothing means no semantic error, Just contains the semantic error message
type SemanticErr = Maybe String 

printError      :: SemanticErr -> IO ()
printError err  =  putStrLn $ case err of
    Nothing  -> "No Semantic Errors"
    Just msg -> "SemanticError: " ++ msg 



