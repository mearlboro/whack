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
-- 3. Add all function identifiers
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
--   1. Add funcion name to the global scope
--   2. Create function scope, parented by the global scope
--   3. Build the body STATement, updating the function scope 
--   4. Add function parameters to the function scope
buildFUNC :: Func -> Scope -> FUNC 
buildFUNC func@( Func ftype name plist body ) globalScope = func'
    where
        functionScope              = ST globalScope empty -- New scope
        ( functionScope' , body' ) = buildSTAT body functionScope 
        functionScope''            = foldr addParam functionScope' plist
        func'                      = FUNC ftype name plist body' functionScope''


-- | Given a statement and the currentScope it is in, produce a STAT 
--   with its own scope, if needed. The current scope gets populated by 
--   the identifiers, if any, declared or used in the statement.
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



