module WaccSemantics where

import Data.Maybe                 ( isNothing , fromMaybe , fromJust )
import Data.Map                   ( empty                            )
import Control.Applicative hiding ( empty                            )
import Data.Char                  ( isSpace                          )

import WaccParser
import WaccDataTypes
--import WaccExamplesTester
import WaccSymbolTable

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Data Definitions :::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

---- | PROGRAM represents the Program AST augmented with the global scope
--data PROGRAM = PROGRAM [ FUNC ] STAT It deriving ( Show )


---- | FUNC also contains the AST for Function 
--data FUNC = FUNC Type Ident ParamList STAT deriving ( Show )

---- | Some statement (while, if, scoped) introduce a new scope.
---- | All statements have a reference to the current scope they are in
--data STAT 
--  = SKIPstat      
--  | FREEstat    Expr      It                                              
--  | RETURNstat  Expr      It                                             
--  | EXITstat    Expr      It                                              
--  | PRINTstat   Expr      It                                              
--  | PRINTLNstat Expr      It                       
--  | SCOPEDstat  STAT                             
--  | READstat    AssignLhs It                         
--  | WHILEstat   Expr      STAT      It                            
--  | SEQstat     STAT      STAT                                  
--  | ASSIGNstat  AssignLhs AssignRhs It                    
--  | IFstat      Expr      STAT      STAT      It     
--  | DECLAREstat Type      Ident     AssignRhs It 
--  deriving ( Show )


--instance Show STAT where
--  show stat = case stat of 
--    SKIPstat        -> "skip"  
--    FREEstat    _ t -> "free ◊ "    ++ show t                              
--    RETURNstat  _ t -> "return ◊ "  ++ show t                        
--    EXITstat    _ t -> "exit ◊ "    ++ show t                             
--    PRINTstat   _ t -> "print ◊ "   ++ show t                              
--    PRINTLNstat _ t -> "println ◊ " ++ show t                       
--    SCOPEDstat  s   -> "begin\n\t"  ++ show s ++ "end\n"                          
--    READstat    _ t -> "read ◊"     ++ show t                   
--    WHILEstat   _  s t -> "while ◊ do\n\t" ++ show s ++ "done\n"                    
--    SEQstat     s s'   -> show s ++ "\n" ++ show s'                           
--    ASSIGNstat  _ _ t  -> "◊ = ◊ " ++ show t                    
--    IFstat      _ s s' t -> "if ◊ then\n\t" ++ show s ++ "\n\telse\n\t" ++ show s' ++ "fi" ++ show t     
--    DECLAREstat t i a t  -> show t ++ " " ++ show i ++ " = " ++ show a ++ " | " ++ show t
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Building the PROGRAM :::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

--buildPROGRAM                         :: Program -> PROGRAM 
--buildPROGRAM ( Program funcs main )  =  PROGRAM funcs' main' globalScope'
--  where
--    -- Create the global scope and add function names to it 
--    globalScope  =  addFuncs emptyTable funcs
--    -- Build the main body which may indroduce new global variables
--    ( globalScope' , main' )  =  buildSTAT main globalScope
--    -- Build all the functions. Each function will have their
--    -- own scope but will need to reference the global scope
--    funcs'  =  map ( buildFUNC globalScope' ) funcs 
   

--buildFUNC                                             :: It -> Func -> FUNC 
--buildFUNC globalScope ( Func ftype name plist body )  =  func'
--  where
--    -- Function scope is an intermediate layer that contains
--    -- the function parameters and is enclosed by the global scope
--    functionScope  =  encloseIn globalScope 
--    -- Add all the parameters to the funcion scope 
--    functionScope'  =  addParams functionScope plist
--    -- The body of a function is evaluated in its own scope. The body 
--    -- is just a sequence of statements. Each statement will have its own 
--    -- 'scope' - 'identifier table', parented to the funcionScope that 
--    -- contains the most updated identifier objects. We ignore the identifier 
--    -- table returned by buildSTAT because none is ever going to need it.
--    -- That's something to think about...  
--    ( _lastStatIt , body' )  =  buildSTAT body functionScope'
--    -- FUNC doesn't need a scope, because the statements in its body 
--    -- will have their own that references the funcionScope
--    func' = FUNC ftype name plist body' 


---- Given a stament and a parent table of identifiers, return the table
---- of identifiers for the NEXT statement together with the newly-built STAT 
--buildSTAT :: Stat -> It -> ( It , STAT ) 
--buildSTAT stat parentIt = 
--  case stat of 
--    SkipStat            -> (,) parentIt     SKIPstat 
--    FreeStat    expr    -> buildSimpleSTAT  FREEstat    expr    
--    ReturnStat  expr    -> buildSimpleSTAT  RETURNstat  expr                                     
--    ExitStat    expr    -> buildSimpleSTAT  EXITstat    expr                                      
--    PrintStat   expr    -> buildSimpleSTAT  PRINTstat   expr                                       
--    PrintlnStat expr    -> buildSimpleSTAT  PRINTLNstat expr                                    
--    ReadStat    lhs     -> (,) parentIt   $ READstat    lhs     parentIt                                    
--    AssignStat  lhs rhs -> (,) parentIt   $ ASSIGNstat  lhs rhs parentIt                                       
--    DeclareStat _ _ _   -> buildDECLAREstat stat 
--    ScopedStat  _       -> buildSCOPEDstat  stat
--    WhileStat   _ _     -> buildWHILEstat   stat
--    IfStat      _ _ _   -> buildIFstat      stat
--    SeqStat     _ _     -> buildSEQstat     stat  
--  where
--    -- Build a simple statement that doesn't introduce new identifiers
--    buildSimpleSTAT constr expr  =  ( parentIt , constr expr $ encloseIn parentIt )

--    -- So we arrived at a declare statement.
--    -- we have the parent scope... we need to return the table for the next 
--    -- statement. This is the only case where the table is updated
--    buildDECLAREstat ( DeclareStat dtype ident rhs ) = 
--      let localIt    =  encloseIn parentIt
--          parentIt'  =  addVariable localIt ident dtype 
--      in  ( parentIt' , DECLAREstat dtype ident rhs parentIt' )

--    -- scoped statement is scoped, we dont care what goes on in there,
--    -- everything can be redefined 
--    buildSCOPEDstat ( ScopedStat body ) = 
--      let childIt                =  encloseIn parentIt  
--          ( _childIt' , body' )  =  buildSTAT body childIt
--      in  ( parentIt  , SCOPEDstat body' )
  
--    buildWHILEstat ( WhileStat expr body ) = 
--      let loopIt                =  encloseIn parentIt 
--          ( loopIt'  , body' )  =  buildSTAT body loopIt 
--      in  ( parentIt , WHILEstat expr body' parentIt )
  
--    buildIFstat ( IfStat expr thenBody elseBody ) =
--      let thenIt                    =  encloseIn parentIt  
--          elseIt                    =  encloseIn parentIt 
--          ( thenIt'  , thenBody' )  =  buildSTAT thenBody thenIt
--          ( elseIt'  , elseBody' )  =  buildSTAT elseBody elseIt
--      in  ( parentIt , IFstat expr thenBody' elseBody' parentIt ) 
  
--    -- Where the magic happens 
--    buildSEQstat ( SeqStat first second ) = 
--      let ( parentIt'  , first'  )  =  buildSTAT first  parentIt
--          ( parentIt'' , second' )  =  buildSTAT second parentIt'
--      in  ( parentIt'' , SEQstat first' second' )


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Semantic Analisys ::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --


buildPROGRAM                         :: Program -> Program 
buildPROGRAM ( Program funcs main )  =  Program funcs' main'
  where
    -- Create the global scope and add function names to it 
    globalScope  =  addFuncs emptyTable funcs
    -- Build the main body which may indroduce new global variables
    ( globalScope' , main' )  =  buildSTAT main globalScope
    -- Build all the functions. Each function will have their
    -- own scope but will need to reference the global scope
    funcs'  =  map ( buildFUNC globalScope' ) funcs 
   

buildFUNC                                             :: It -> Func -> Func 
buildFUNC globalScope ( Func ftype name plist body )  =  func'
  where
    -- Function scope is an intermediate layer that contains
    -- the function parameters and is enclosed by the global scope
    functionScope  =  encloseIn globalScope 
    -- Add all the parameters to the funcion scope 
    functionScope'  =  addParams functionScope plist
    -- The body of a function is evaluated in its own scope. The body 
    -- is just a sequence of statements. Each statement will have its own 
    -- 'scope' - 'identifier table', parented to the funcionScope that 
    -- contains the most updated identifier objects. We ignore the identifier 
    -- table returned by buildSTAT because none is ever going to need it.
    -- That's something to think about...  
    ( _lastStatIt , body' )  =  buildSTAT body functionScope'
    -- FUNC doesn't need a scope, because the statements in its body 
    -- will have their own that references the funcionScope
    func' = Func ftype name plist body' 


-- Given a stament and a parent table of identifiers, return the table
-- of identifiers for the NEXT statement together with the newly-built STAT 
buildSTAT :: Stat -> It -> ( It , Stat ) 
buildSTAT stat parentIt = 
  case stat of 
    SkipStat            -> (,) parentIt      SkipStat 
    FreeStat    expr _   -> buildSimpleSTAT  FreeStat    expr    
    ReturnStat  expr _   -> buildSimpleSTAT  ReturnStat  expr                                     
    ExitStat    expr _   -> buildSimpleSTAT  ExitStat    expr                                      
    PrintStat   expr _   -> buildSimpleSTAT  PrintStat   expr                                       
    PrintlnStat expr _   -> buildSimpleSTAT  PrintlnStat expr                                    
    ReadStat    lhs  _   -> (,) parentIt   $ ReadStat    lhs     parentIt                                    
    AssignStat  lhs rhs _ -> (,) parentIt  $ AssignStat  lhs rhs parentIt                                       
    DeclareStat _ _ _ _  -> buildDECLAREstat stat 
    ScopedStat  _       -> buildSCOPEDstat  stat
    WhileStat   _ _  _   -> buildWHILEstat   stat
    IfStat      _ _ _ _  -> buildIFstat      stat
    SeqStat     _ _     -> buildSEQstat     stat  
  where
    -- Build a simple statement that doesn't introduce new identifiers
    buildSimpleSTAT constr expr  =  ( parentIt , constr expr $ encloseIn parentIt )

    -- So we arrived at a declare statement.
    -- we have the parent scope... we need to return the table for the next 
    -- statement. This is the only case where the table is updated
    buildDECLAREstat ( DeclareStat dtype ident rhs _ ) = 
      let localIt    =  encloseIn parentIt
          parentIt'  =  addVariable localIt ident dtype 
      in  ( parentIt' , DeclareStat dtype ident rhs parentIt' )

    -- scoped statement is scoped, we dont care what goes on in there,
    -- everything can be redefined 
    buildSCOPEDstat ( ScopedStat body ) = 
      let childIt                =  encloseIn parentIt  
          ( _childIt' , body' )  =  buildSTAT body childIt
      in  ( parentIt  , ScopedStat body' )
  
    buildWHILEstat ( WhileStat expr body _ ) = 
      let loopIt                =  encloseIn parentIt 
          ( loopIt'  , body' )  =  buildSTAT body loopIt 
      in  ( parentIt , WhileStat expr body' parentIt )
  
    buildIFstat ( IfStat expr thenBody elseBody _  ) =
      let thenIt                    =  encloseIn parentIt  
          elseIt                    =  encloseIn parentIt 
          ( thenIt'  , thenBody' )  =  buildSTAT thenBody thenIt
          ( elseIt'  , elseBody' )  =  buildSTAT elseBody elseIt
      in  ( parentIt , IfStat expr thenBody' elseBody' parentIt ) 
  
    -- Where the magic happens 
    buildSEQstat ( SeqStat first second ) = 
      let ( parentIt'  , first'  )  =  buildSTAT first  parentIt
          ( parentIt'' , second' )  =  buildSTAT second parentIt'
      in  ( parentIt'' , SeqStat first' second' )




-- * To look in the current scope use >findIdent ident scope 
-- * To look in the parent's scope as well use >findIdent' ident scope

-- | Check semantics of a program
--checkProgram                                     :: PROGRAM -> SemanticErr 
--checkProgram ( PROGRAM funcs body globalScope )  =  error "TODO"


---- | Check semantics of a function
--checkFunc                                               :: FUNC -> SemanticErr 
--checkFunc ( FUNC ftype name plist body )  =  error "TODO" 


---- | Check semantics of a statement 
--checkStat       :: STAT -> SemanticErr 
--checkStat stat  =  case stat of 
--    SKIPstat                              -> error "TODO"
--    FREEstat      expr  scope             -> error "TODO"                
--    RETURNstat    expr  scope             -> error "TODO"                
--    EXITstat      expr  scope             -> error "TODO"                
--    PRINTstat     expr  scope             -> error "TODO"                
--    PRINTLNstat   expr  scope             -> error "TODO"        
--    SCOPEDstat    stat                    -> error "TODO"        
--    READstat      lhs   scope             -> error "TODO"         
--    WHILEstat     expr  body  scope       -> error "TODO"          
--    SEQstat       stat  stat'             -> error "TODO"          
--    ASSIGNstat    lhs   rhs   scope       -> error "TODO"          
--    IFstat        expr  sthen selse scope -> error "TODO"   
--    DECLAREstat   stype ident rhs   scope -> error "TODO"


---- | Check semantics of an expression
--checkExpr             :: Expr -> It -> SemanticErr 
--checkExpr expr scope  =  case expr of 
--    BoolLiterExpr     bool             -> error "TODO"  
--    CharLiterExpr     char             -> error "TODO"   
--    IdentExpr         ident            -> error "TODO"  
--    UnaryOperExpr     op    expr       -> error "TODO"   
--    ParenthesisedExpr expr             -> error "TODO"   
--    IntLiterExpr      int              -> error "TODO"   
--    StrLiterExpr      str              -> error "TODO"   
--    PairLiterExpr     pair             -> error "TODO"     
--    ArrayElemExpr     arr              -> error "TODO"
--    BinaryOperExpr    op    expr expr' -> error "TODO"     


-- Nothing means no semantic error, Just contains the semantic error message
type SemanticErr = Maybe String 

printError      :: SemanticErr -> IO ()
printError err  =  putStrLn $ case err of
    Nothing  -> "No Semantic Errors"
    Just msg -> "SemanticError: " ++ msg 

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Testing ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --


--begin = do 
--    putStrLn $ "Enter the name of a .wacc program to read, parse and test."
--    putStrLn $ "The file should be located in wacc_examples/semanticErr"
--    path <- getLine 
--    program <- parseOne' $ "wacc_examples/semanticErr/" ++ path
--    let pROGRAM = buildPROGRAM program 
--    putStrLn $ "semantically-augmented PROGRAM:\n" ++ show pROGRAM 
