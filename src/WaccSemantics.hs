module WaccSemantics where

import Data.Maybe                 ( isNothing , fromMaybe , fromJust , isJust )
import Data.Map                   ( empty                                     )
import Control.Applicative hiding ( empty                                     )
import Data.Char                  ( isSpace                                   )
import Data.List                  ( group , sort                              )
import Control.Monad              ( when                                      )

import Utils
import WaccParser
import WaccDataTypes
import WaccSymbolTable


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Semantic Augmentation ::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

augmentProgram 
  :: Program -- | The Program AST
  -> Program -- | The agumented Program AST 
-- | There is no "global scope" in wacc. Each function has its own little scope.
--   Nonetheless, we refer to he globalScope as the identifier table that 
--   contains the funcion names, since the only "globally accessible" objects 
--   are the functions defined at the beginning of a WACC program.
--   1. Create the globalScope and add the function names to it 
--   2. Augment the main body, passing a reference to the globalScope
--   3. Augment all the functions. Each function will have its own scope but 
--      will need to reference the globalScope in case other funcions are called
--      from within its body
augmentProgram ( Program funcs main )  =  Program funcs' main'
  where
    globalScope    =  addFuncs    funcs emptyTable
    ( _ , main' )  =  augmentStat main  globalScope
    funcs'         =  map ( augmentFunc globalScope ) funcs 
   
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

augmentFunc 
  :: It   -- | The globalScope
  -> Func -- | The Func AST with Empty identifier table
  -> Func -- | The Func AST with the funcionScope
-- | 1. The functionScope is an intermediate layer (table) that contains the 
--      function name and parameters and is enclosed by the globalScope
--   2. Add the funcion name and all its parameters to the functionScope 
--   3. The body of a function is evaluated in its own scope, enclosed by the 
--      functionScope.
augmentFunc globalScope func@( Func ftype fname plist body _ )  =  func'
  where
    funcScope     =  addParams   plist . addFunc func $ encloseIn globalScope
    ( _ , body' ) =  augmentStat body                   funcScope
    func'         =  Func ftype fname plist body' funcScope

 -- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

augmentStat
  :: Stat          -- | The Stat AST with Empty identifier table
  -> It            -- | The identifier table of the *previous* statement, 
                   --   or the functionScope if this is the first statement
                   --   in a function body
  -> ( It , Stat ) -- | A pair containing the identifier table for the *next* 
                   --   statement and the augmented input Stat
-- | Given a statement and the table of identifiers for the *previous* 
--   statement, return the table of identifiers for the *next* statement 
--   together with the newly-built Stat
augmentStat stat prevIt  = 
  case stat of 
    SkipStat              -> ( prevIt , SkipStat                   )
    FreeStat    expr    _ -> ( prevIt , FreeStat    expr    prevIt ) 
    ReturnStat  expr    _ -> ( prevIt , ReturnStat  expr    prevIt )                                  
    ExitStat    expr    _ -> ( prevIt , ExitStat    expr    prevIt )                                   
    PrintStat   expr    _ -> ( prevIt , PrintStat   expr    prevIt )                                    
    PrintlnStat expr    _ -> ( prevIt , PrintlnStat expr    prevIt )                                 
    ReadStat    lhs     _ -> ( prevIt , ReadStat    lhs     prevIt )                                   
    AssignStat  lhs rhs _ -> ( prevIt , AssignStat  lhs rhs prevIt )                                      
    DeclareStat _ _ _   _ -> augmentDeclare stat 
    ScopedStat  _         -> augmentScoped  stat
    WhileStat   _ _     _ -> augmentWhile   stat
    IfStat      _ _ _   _ -> augmentIf      stat
    SeqStat     _ _       -> augmentSeq     stat  
  where
    -- We have the previous table and we need to return the table for the next 
    -- statement. This is the only case where the table is updated, since we 
    -- just introduced a new variable that can be used by subsequent statements
    augmentDeclare ( DeclareStat vtype vname rhs _ )  = 
      --let localIt  =  encloseIn   prevIt
      let nextIt  =  addVariable vname vtype $ encloseIn prevIt 
      in  ( nextIt , DeclareStat vtype vname rhs nextIt )

    augmentScoped ( ScopedStat body )  = 
      --let childIt        =  encloseIn   prevIt  
      let ( _ , body' )  =  augmentStat body prevIt --   childIt
      in  ( prevIt  , ScopedStat body' )
  
    augmentWhile ( WhileStat expr body _ )  = 
      --let loopIt         =  encloseIn   prevIt 
      let ( _ , body' )  =  augmentStat body prevIt --   loopIt 
      in  ( prevIt , WhileStat expr body' prevIt )
  
    augmentIf ( IfStat expr tbody ebody _  )  =
      --let thenIt             =  encloseIn   prevIt  
      --    elseIt             =  encloseIn   prevIt 
      let ( _ , tbody' )  =  augmentStat tbody prevIt --thenIt
          ( _ , ebody' )  =  augmentStat ebody prevIt -- elseIt
      in  ( prevIt , IfStat expr tbody' ebody' prevIt ) 
  
    -- Where the magic happens 
    augmentSeq ( SeqStat first second )  = 
      let ( nextIt  , first'  )  =  augmentStat first  prevIt
          ( nextIt' , second' )  =  augmentStat second nextIt
      in  ( nextIt' , SeqStat first' second' )

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Semantic Analisys ::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- * To look in the current scope use >findIdent ident scope 
-- * To look in the parent's scope as well use >findIdent' ident scope

-- | Check semantics of a statement 
checkStat       :: STAT -> SemanticErr 
checkStat stat  =  case stat of 
    SKIPstat                                     -> Nothing
      
    FREEstat      (PairLiterExpr pair)  scope    -> checkExpr (PairLiterExpr pair) scope
    FREEstat      (ArrayElemExpr arr)   scope    -> checkExpr (ArrayElemExpr arr)  scope
    FREEstat      _ _                            -> Just "Cannot free that type of memory"                
      
    RETURNstat    expr  scope                    -> error "TODO"   
  
    EXITstat      expr  scope                    -> do  
      let err = checkExpr expr scope
      if (isNothing err) 
        then do
          let eType = getTypeExpr expr scope
          if(eType /= IntType) 
            then return "Exit Statement expects int type"
            else Nothing
        else err
  
    PRINTstat     expr  scope                    -> checkExpr expr scope                
    PRINTLNstat   expr  scope                    -> checkExpr expr scope
       
    SCOPEDstat    stat                           -> checkStat stat  

    READstat      (LhsIdent  ident)   scope      -> checkExpr (IdentExpr ident) scope
    READstat      (LhsPairElem (Fst expr)) scope -> checkExpr expr scope  
    READstat      (LhsPairElem (Snd expr)) scope -> checkExpr expr scope
    READstat      (LhsArrayElem arrayElem) scope -> checkExpr (ArrayElemExpr arrayElem) scope

    WHILEstat     expr  body  scope              -> do
      let err = checkExpr expr scope 
      if (isNothing err) 
        then do
          let eType = getTypeExpr expr scope
          if(eType /= BoolType) 
            then return "Conditional expression should be of type Bool" 
            else checkStat body
        else err

    SEQstat       stat  stat'                    -> do
      let err = checkStat stat
      err' <- checkStat stat' 
      if (isJust err) 
        then err
        else return err'
    
    ASSIGNstat    lhs   rhs   scope              -> error "TODO"          
    
    IFstat        expr  sthen selse scope        -> do
      let err = checkExpr expr scope
      if(isNothing err)
        then do
          let eType = getTypeExpr expr scope
          if(eType /= BoolType)
            then return "Conditional expression should be of type Bool"
            else checkStat (SEQstat sthen selse)
        else err

--    DECLAREstat   stype ident rhs   scope        -> do
--        let aType = getTypeAssignRhs rhs scope
--        if(stype /= aType) 
--          then return "Assignment should have the same type as the identifier"
--          else Nothing


 

checkAssignRhs  :: AssignRhs -> Scope -> SemanticErr
checkAssignRhs assignRhs scope = case assignRhs of
    RhsExpr       expr        scope         ->  checkExpr expr scope  
    RhsPairElem   (Fst expr)  scope         ->  checkExpr expr scope  
    RhsPairElem   (Snd expr)  scope         ->  checkExpr expr scope 
    RhsArrayLiter exprs       scope         ->  do
      let checkedExps = map (`checkExpr` scope) exprs
      let typeExpr    = map (`getTypeExpr` scope) exprs
      if(all (==Nothing) checkExpr && all (==head typeExpr) typeExpr) --all exprs have to be correct and have the same type 
        then Nothing
        else return "Array literal error" 

    RhsNewPair    expr expr'  scope         ->  do
      let err  = checkExpr expr  scope
      let err' = checkExpr expr' scope
      if(isJust err) 
        then err
        else err'

    RhsCall       ident exprs scope         ->  do
      let typeAndContext = findIdent' ident scope
      if(isNothing typeAndContext) 
        then return "Function identifier not found"
        else do
          let iContext = case snd (fromJust typeAndContext) of
            Function (Func _ _ paramList _) -> do   
              let typeExpr = map (`getTypeExpr` scope) exprs 
              if(paramList == typeExpr)
                then Nothing
                else return "Function parameters have wrong types"
            _                               -> return "Not a function" 

-- |Gets the type of an expression
getTypeExpr :: Expr -> Scope -> Maybe Type
getTypeExpr expr scope = case expr of
    BoolLiterExpr     bool             -> BoolType 
    CharLiterExpr     char             -> CharType   
    IdentExpr         ident            -> fst (fromJust (findIdent ident scope))
    UnaryOperExpr     NotUnOp    expr  -> BoolType
    UnaryOperExpr     LenUnOp    expr  -> IntType
    UnaryOperExpr     OrdUnOp    expr  -> IntType  
    UnaryOperExpr     ChrUnOp    expr  -> CharType
    UnaryOperExpr     NegUnOp    expr  -> IntType
    ParenthesisedExpr expr             -> getTypeExpr expr scope   
    IntLiterExpr      int              -> IntType   
    StrLiterExpr      str              -> StringType   
    PairLiterExpr     pair             -> NullType 
 -- ArrayElemExpr     ArrayType (arrIdent exprs) -> ArrayType (findIdent arrIdent scope) TODO
    
    BinaryOperExpr AddBinOp _ _        -> IntType
    BinaryOperExpr SubBinOp _ _        -> IntType                                  
    BinaryOperExpr MulBinOp _ _        -> IntType                                   
    BinaryOperExpr DivBinOp _ _        -> IntType                                    
    BinaryOperExpr ModBinOp _ _        -> IntType                                    
    BinaryOperExpr _        _ _        -> BoolType  
        
-- |Gets the type of a RHS Assignment 
--getTypeAssignRhs :: AssignRhs -> Scope -> Type
--getTypeAssignRhs ass scope = case ass of 
--    RhsExpr       expr                 -> getTypeExpr expr scope
--    RhsPairElem   (Fst expr)           -> getTypeExpr expr scope 
--    RhsPairElem   (Snd expr)           -> getTypeExpr expr scope 
--  | RhsArrayLiter ArrayLiter                    -- <array-liter>
--  | RhsNewPair    Expr       Expr               -- 'newpair' '(' <expr> ',' <expr> ')'
--  | RhsCall       Ident      ArgList            -- 'call' <ident> '(' <arg-list>? ')'
        
-- Nothing means no semantic error, Just contains the semantic error message
type SemanticErr = Maybe String 

printError      :: SemanticErr -> IO ()
printError err  =  putStrLn $ case err of
    Nothing  -> "No Semantic Errors"
    Just msg -> "SemanticError: " ++ msg 

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Testing ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

type SemanticError = [ Char ]


-- | Check for semantic errors in a program
checkProgram                         :: Program -> [ SemanticError ]
checkProgram ( Program funcs main )  =  
  checkDuplFuncs funcs            ++ 
  concatMap checkDuplParams funcs ++ 
  concatMap checkFuncParamClash funcs 

 
checkFuncParamClash                             :: Func -> [ SemanticError ]
checkFuncParamClash ( Func _ fname plist _ _ )  = 
  if   not . and . map ( (/=) fname . pnameOf ) $ plist
  then [ "Function and parameter share the same name @" ++ fname ] 
  else [] 


checkDuplParams  :: Func -> [ SemanticError ]
checkDuplParams  = reportDupl "Parameter " . map pnameOf . paramsOf  
  

checkDuplFuncs  :: [ Func ] -> [ SemanticError ] 
checkDuplFuncs  =  reportDupl "Function " . map nameOf  
  
 
reportDupl    :: String -> [ String ] -> [ SemanticError ]
reportDupl t  =  map ( logDupl t ) . filter ( (<) 1 . length ) . group . sort


logDupl :: String -> [ String ] -> SemanticError 
logDupl t l = t++" "++(head l)++" defined "++(show$length l)++" times!" 



checkStat = undefined
