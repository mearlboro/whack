module WaccSemChecker where

import Data.Maybe                 ( isNothing , fromMaybe , fromJust , isJust )
import Data.Map                   ( empty                                     )
import Control.Applicative hiding ( empty                                     )
import Data.Char                  ( isSpace                                   )
import Data.List                  ( group , sort                              )
import Control.Monad              ( when                                      )

import WaccDataTypes
import WaccSymbolTable

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Program Semantic Analisys ::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | Check for semantic errors in a program BEFORE it is augmented 
checkProgram                         :: Program -> [ SemanticError ]
checkProgram ( Program funcs main )  =  
  checkDuplFuncs                funcs ++ 
  concatMap checkDuplParams     funcs ++ 
  concatMap checkFuncParamClash funcs 


-- | Check that there is no paramter that shares the function name
checkFuncParamClash                             :: Func -> [ SemanticError ]
checkFuncParamClash ( Func _ fname plist _ _ )  = 
  if   not . and . map ( (/=) fname . pnameOf ) $ plist
  then [ "Function and parameter share the same name @" ++ fname ] 
  else [] 


-- | Check for duplicate parameter names in a funcion parameter list
checkDuplParams  :: Func -> [ SemanticError ]
checkDuplParams  = reportDupl "Parameter " . map pnameOf . paramsOf  
  

-- | Check for duplicate funcion names in a program
checkDuplFuncs  :: [ Func ] -> [ SemanticError ] 
checkDuplFuncs  =  reportDupl "Function " . map nameOf  
  

-- | Check for duplicate elements in a list and output a semantic error message
reportDupl    :: String -> [ String ] -> [ SemanticError ]
reportDupl t  =  map ( logDupl t ) . filter ( (<) 1 . length ) . group . sort
  where
    logDupl t l = t++" "++(head l)++" defined "++(show$length l)++" times!" 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Statement Semantic Analisys ::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

---- | Check semantics of a statement 
--checkStat       :: STAT -> SemanticErr 
--checkStat stat  =  case stat of 
--    SkipStat                                     -> Nothing
      
--    FreeStat      (PairLiterExpr pair)  scope    -> checkExpr (PairLiterExpr pair) scope
--    FreeStat      (ArrayElemExpr arr)   scope    -> checkExpr (ArrayElemExpr arr)  scope
--    FreeStat      _ _                            -> Just "Cannot free that type of memory"                
      
--    ReturnStat    expr  scope                    -> error "TODO"   
  
--    ExitStat      expr  scope                    -> do  
--      let err = checkExpr expr scope
--      if (isNothing err) 
--        then do
--          let eType = getTypeExpr expr scope
--          if(eType /= IntType) 
--            then return "Exit Statement expects int type"
--            else Nothing
--        else err
  
--    PrintStat     expr  scope                    -> checkExpr expr scope                
--    PrintlnStat   expr  scope                    -> checkExpr expr scope
       
--    ScopedStat    stat                           -> checkStat stat  

--    ReadStat      (LhsIdent  ident)   scope      -> checkExpr (IdentExpr ident) scope
--    ReadStat      (LhsPairElem (Fst expr)) scope -> checkExpr expr scope  
--    ReadStat      (LhsPairElem (Snd expr)) scope -> checkExpr expr scope
--    ReadStat      (LhsArrayElem arrayElem) scope -> checkExpr (ArrayElemExpr arrayElem) scope

--    WhileStat     expr  body  scope              -> do
--      let err = checkExpr expr scope 
--      if (isNothing err) 
--        then do
--          let eType = getTypeExpr expr scope
--          if(eType /= BoolType) 
--            then return "Conditional expression should be of type Bool" 
--            else checkStat body
--        else err

--    SeqStat       stat  stat'                    -> do
--      let err = checkStat stat
--      err' <- checkStat stat' 
--      if (isJust err) 
--        then err
--        else return err'
    
--    AssignStat    lhs   rhs   scope              -> error "TODO"          
    
--    IfStat        expr  sthen selse scope        -> do
--      let err = checkExpr expr scope
--      if(isNothing err)
--        then do
--          let eType = getTypeExpr expr scope
--          if(eType /= BoolType)
--            then return "Conditional expression should be of type Bool"
--            else checkStat (SeqStat sthen selse)
--        else err



--checkAssignRhs  :: AssignRhs -> Scope -> SemanticErr
--checkAssignRhs assignRhs scope = case assignRhs of
--    RhsExpr       expr        scope         ->  checkExpr expr scope  
--    RhsPairElem   (Fst expr)  scope         ->  checkExpr expr scope  
--    RhsPairElem   (Snd expr)  scope         ->  checkExpr expr scope 
--    RhsArrayLiter exprs       scope         ->  do
--      let checkedExps = map (`checkExpr` scope) exprs
--      let typeExpr    = map (`getTypeExpr` scope) exprs
--      if(all (==Nothing) checkExpr && all (==head typeExpr) typeExpr) --all exprs have to be correct and have the same type 
--        then Nothing
--        else return "Array literal error" 

--    RhsNewPair    expr expr'  scope         ->  do
--      let err  = checkExpr expr  scope
--      let err' = checkExpr expr' scope
--      if(isJust err) 
--        then err
--        else err'

--    RhsCall       ident exprs scope         ->  do
--      let typeAndContext = findIdent' ident scope
--      if(isNothing typeAndContext) 
--        then return "Function identifier not found"
--        else do
--          let iContext = case snd (fromJust typeAndContext) of
--            Function (Func _ _ paramList _) -> do   
--              let typeExpr = map (`getTypeExpr` scope) exprs 
--              if(paramList == typeExpr)
--                then Nothing
--                else return "Function parameters have wrong types"
--            _                               -> return "Not a function" 

---- |Gets the type of an expression
--getTypeExpr :: Expr -> Scope -> Maybe Type
--getTypeExpr expr scope = case expr of
--    BoolLiterExpr     bool             -> BoolType 
--    CharLiterExpr     char             -> CharType   
--    IdentExpr         ident            -> fst (fromJust (findIdent ident scope))
--    UnaryOperExpr     NotUnOp    expr  -> BoolType
--    UnaryOperExpr     LenUnOp    expr  -> IntType
--    UnaryOperExpr     OrdUnOp    expr  -> IntType  
--    UnaryOperExpr     ChrUnOp    expr  -> CharType
--    UnaryOperExpr     NegUnOp    expr  -> IntType
--    ParenthesisedExpr expr             -> getTypeExpr expr scope   
--    IntLiterExpr      int              -> IntType   
--    StrLiterExpr      str              -> StringType   
--    PairLiterExpr     pair             -> NullType 
-- -- ArrayElemExpr     ArrayType (arrIdent exprs) -> ArrayType (findIdent arrIdent scope) TODO
    
--    BinaryOperExpr AddBinOp _ _        -> IntType
--    BinaryOperExpr SubBinOp _ _        -> IntType                                  
--    BinaryOperExpr MulBinOp _ _        -> IntType                                   
--    BinaryOperExpr DivBinOp _ _        -> IntType                                    
--    BinaryOperExpr ModBinOp _ _        -> IntType                                    
--    BinaryOperExpr _        _ _        -> BoolType  
        
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




checkStat = undefined

