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

-- | PROGRAM contains an AST with semantic attributes and the global scope
data PROGRAM = PROGRAM [ FUNC ] STAT IdentTable 

-- | FUNC als contains an AST for a function and introduces its own scope
data FUNC = FUNC Type Ident ParamList STAT IdentTable 

-- | Some statement (while, if, scoped) introduce a new scope
data STAT 
  = SKIPstat      IdentTable 
  | FREEstat      Expr        IdentTable                                               
  | RETURNstat   Expr        IdentTable                                              
  | EXITstat      Expr        IdentTable                                               
  | PRINTstat     Expr        IdentTable                                               
  | PRINTLNstat   Expr        IdentTable                        
  | SCOPEDstat    STAT        IdentTable                        
  | READstat      AssignLhs   IdentTable                         
  | WHILEstat     Expr        STAT        IdentTable                            
  | SEQstat       STAT        STAT        IdentTable                             
  | ASSIGNstat    AssignLhs   AssignRhs   IdentTable                    
  | IFstat        Expr        STAT        STAT        IdentTable       
  | DECLAREstat   Type        Ident       AssignRhs   IdentTable 

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Building the PROGRAM :::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

buildPROGRAM :: Program -> PROGRAM 
buildPROGRAM p@( Program funcs main ) = PROGRAM funcs' main' globalScope''
    where
        -- The global scope 
        globalScope = ST Empty empty
        -- Build the main statement thus updating the global scope
        ( globalScope' , main' ) = buildSTAT main globalScope

        -- TODO implement with a fold or something clever
        addFuncs []     _funcs scope = ( scope   , _funcs )
        addFuncs (f:fs) _funcs scope = addFuncs fs (_FUNC:_funcs) scope'
            where ( scope'  , _FUNC  ) = buildFUNC f scope

        -- Build the program functions, once again updating the global
        -- scope since the function name identifiers will be added by
        -- buildFUNC
        ( globalScope'' , funcs' ) = addFuncs funcs [] globalScope'


-- | Given a function and its parent scope (the global scope), build its 
--   semantically-augmented version (FUNC) and in the process create 
--   its own childScope
buildFUNC :: Func -> It -> ( It , FUNC )
buildFUNC func@( Func ftype name plist body ) globalScope = result
    where
        -- Add funcion name to the global scope
        globalScope' = addFunc func globalScope
        -- Create function scope, parented to global scope
        functionScope = ST globalScope' empty -- New scope
        -- Build the statement, updating the function scope and
        -- obtaining the new body
        ( functionScope' , body' ) = buildSTAT body functionScope 
        -- Add function arguments to the function scope
        functionScope'' = foldr addParam functionScope' plist
        -- Finally build the function
        func' = FUNC ftype name plist body' functionScope''
        -- It doesn't fit on one line
        result = ( globalScope' , func' )


-- | Given a statement and its parent (enclosing) scope produce a statement 
--   with its own scope, if needed. Also add itentifiers to parent scope here
buildSTAT :: Stat -> It -> ( It , STAT )  
buildSTAT stat parentScope = case stat of 
    -- Parent scope unchanged , Skip statement doesn't have its own scope
    SkipStat         -> ( parentScope               , SKIPstat         Empty )
    -- Scan expression for identifiers and update parent scope  
    -- Free statement doesn't have its own scope. Samve goes for the rest
    FreeStat    expr -> ( scanExpr expr parentScope , FREEstat    expr Empty ) 
    ReturnStat  expr -> ( scanExpr expr parentScope , RETURNstat  expr Empty ) 
    ExitStat    expr -> ( scanExpr expr parentScope , EXITstat    expr Empty ) 
    PrintStat   expr -> ( scanExpr expr parentScope , PRINTstat   expr Empty ) 
    PrintlnStat expr -> ( scanExpr expr parentScope , PRINTLNstat expr Empty ) 

    ReadStat    lhs             -> error "TODO" 
    AssignStat  lhs   rhs       -> error "TODO"
    DeclareStat dtype ident rhs -> error "TODO"

    -- Scoped statements
    ScopedStat  _     -> buildSCOPEDstat stat parentScope 
    WhileStat   _ _   -> buildWHILEstat  stat parentScope 
    SeqStat     _ _   -> buildSEQstat    stat parentScope 
    IfStat      _ _ _ -> buildIFstat     stat parentScope


buildSCOPEDstat ( ScopedStat body ) parentScope = 
    -- build a statement that is scoped, providing an empty table whose 
    -- parent is the eclosing table provided
    let ( childScope , stat' ) = buildSTAT body ( ST parentScope empty ) -- New scope
    -- Return the original table which will have not been modified and the
    -- statment just build, with its new inner scope table'
    in  ( parentScope , SCOPEDstat stat' childScope )


buildWHILEstat ( WhileStat expr body ) parentScope = 
    -- Populate new scope with expression 
    let childScope = scanExpr expr ( ST parentScope empty ) 
    -- Use updated table to build the statement
        ( childScope' , stat' ) = buildSTAT body childScope
    -- Return the new semantically-augmented while statement wow 
    in  ( parentScope , WHILEstat expr stat' childScope' )


buildIFstat ( IfStat expr stat1 stat2 ) parentScope = 
    -- Add stuff to parent scope from expression
    let parentScope' = scanExpr expr parentScope
    -- Create first child scope with new parentscope
        ( _childScope1 , stat1' ) = buildSTAT stat1 ( ST parentScope' empty )
    -- Create second child sope
        ( _childScope2 , stat2' ) = buildSTAT stat2 ( ST parentScope' empty )
    -- Return ffs -- If stmt itself dont introduce a new scope, hence Empty
    in  ( parentScope' , IFstat expr stat1' stat2' Empty ) 


buildSEQstat ( SeqStat stat1 stat2 ) parentScope = 
    let ( parentScope'  , stat1' ) = buildSTAT stat1 parentScope
        ( parentScope'' , stat2' ) = buildSTAT stat2 parentScope'
    in  ( parentScope'' , SEQstat stat1' stat2' Empty )


-- | Search an expression for identifiers to put in the identifier table
scanExpr :: Expr -> It -> It 
scanExpr expr table = case expr of
    BoolLiterExpr     _         -> table  
    CharLiterExpr     _         -> table   
    IdentExpr         ident     -> addVariable ident Nothing table
    UnaryOperExpr     _     e   -> scanExpr e table 
    ParenthesisedExpr e         -> scanExpr e table 
    IntLiterExpr      _         -> table   
    StrLiterExpr      _         -> table   
    PairLiterExpr     _         -> table     
    ArrayElemExpr     a         -> error "TODO"
    BinaryOperExpr    _    e e' -> scanExpr e' ( scanExpr e table )   


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Semantic Analisys ::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | Check semantics of a program
checkProgram :: PROGRAM -> Bool 
checkProgram ( PROGRAM funcs body table ) = error "TODO"


-- | Check semantics of a function
checkFunc :: FUNC -> Bool 
checkFunc ( FUNC ftype name plist body table ) = error "TODO" 


-- | Check semantics of a statement 
checkStat :: STAT -> Bool 
checkStat stat = case stat of 
    SKIPstat      table                   -> error "TODO"
    FREEstat      expr  table             -> error "TODO"                
    RETURNstat    expr  table             -> error "TODO"                
    EXITstat      expr  table             -> error "TODO"                
    PRINTstat     expr  table             -> error "TODO"                
    PRINTLNstat   expr  table             -> error "TODO"        
    SCOPEDstat    stat  table             -> error "TODO"        
    READstat      lhs   table             -> error "TODO"         
    WHILEstat     expr  stat1 table       -> error "TODO"          
    SEQstat       stat1 stat2 table       -> error "TODO"          
    ASSIGNstat    lhs   rhs   table       -> error "TODO"          
    IFstat        expr  stat1 stat2 table -> error "TODO"   
    DECLAREstat   ttype ident rhs   table -> error "TODO"


-- | Check semantics of an expression
checkExpr :: Expr -> It -> Bool 
checkExpr expr table = case expr of 
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



