module WaccSemantics where

import Data.Maybe                 ( isNothing , fromMaybe , fromJust )
import Data.Map                   ( empty                            )
import Control.Applicative hiding ( empty                            )
import Data.Char                  ( isSpace                          )
import Data.List                  ( group , sort                     )

import WaccParser
import WaccDataTypes
import WaccSymbolTable



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Semantic Analisys ::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

augmentProgram 
  :: Program  -- | The Program AST with Empty identifier tables
  -> Program  -- | The agumented Program AST with the globalScope
-- | 1. Create the global scope and add the function names to it 
--   2. Build the main body which may indroduce global variables
--   3. Build all the functions. Each function will have its own scope but will 
--      need to reference the globalScope
augmentProgram ( Program funcs main )  =  Program funcs' main'
  where
    globalScope               =  addFuncs    emptyTable funcs
    ( globalScope' , main' )  =  augmentStat main       globalScope
    funcs'                    =  map ( augmentFunc globalScope' ) funcs 
   

augmentFunc 
  :: It   -- | The globalScope
  -> Func -- | The Func AST with Empty identifier table
  -> Func -- | The Func AST with the funcionScope
-- | 1. The functionScope is an intermediate layer (table) that contains the 
--      function name and parameters and is enclosed by the global scope
--   2. Add all the parameters to the funcionScope 
--   3. The body of a function is evaluated in its own scope, enclosed by the 
--      funcion scope. We ignore the identifier table returned by buildSTAT 
--      because none is should be equal to funcionScope'
augmentFunc globalScope func@( Func ftype fname plist body _ )  =  func'
  where
    functionScope  =  addFunc     func        $ encloseIn globalScope 
    functionScope' =  addParams   functionScope plist
    ( _ , body' )  =  augmentStat body          functionScope'
    func'          =  Func ftype fname plist body' functionScope'

 
augmentStat
  :: Stat          -- | The Stat AST with Empty identifier table
  -> It            -- | The Stat AST with an identifier table populated with                 --   the most updated types of the variables in scope
  -> ( It , Stat ) -- | A pair containing the identifier table for the *next* 
                   --   statement and the augmented input Stat
-- | Given a stament and a parent table of identifiers, return the table
--   of identifiers for the NEXT statement together with the newly-built Stat
augmentStat stat parentIt  = 
  case stat of 
    SkipStat              -> (,) parentIt   SkipStat 
    FreeStat    expr    _ -> augmentSimple  FreeStat    expr    
    ReturnStat  expr    _ -> augmentSimple  ReturnStat  expr                                     
    ExitStat    expr    _ -> augmentSimple  ExitStat    expr                                      
    PrintStat   expr    _ -> augmentSimple  PrintStat   expr                                       
    PrintlnStat expr    _ -> augmentSimple  PrintlnStat expr                                    
    ReadStat    lhs     _ -> (,) parentIt $ ReadStat    lhs     parentIt                                    
    AssignStat  lhs rhs _ -> (,) parentIt $ AssignStat  lhs rhs parentIt                                       
    DeclareStat _ _ _   _ -> augmentDeclare stat 
    ScopedStat  _         -> augmentScoped  stat
    WhileStat   _ _     _ -> augmentWhile   stat
    IfStat      _ _ _   _ -> augmentIf      stat
    SeqStat     _ _       -> augmentSeq     stat  
  where
    -- Build a simple statement that doesn't introduce new identifiers
    augmentSimple constr expr  =  
          ( parentIt , constr expr $ encloseIn parentIt )

    -- We have the parent table and we need to return the table for the next 
    -- statement. This is the only case where the table is updated, since we 
    -- just introduced a new variable that can be used by subsequent statements
    augmentDeclare ( DeclareStat vtype vname rhs _ )  = 
      let localIt    =  encloseIn   parentIt
          parentIt'  =  addVariable localIt  vname vtype 
      in  ( parentIt' , DeclareStat vtype vname rhs parentIt' )

    -- Scoped statement introduces its own scope
    augmentScoped ( ScopedStat body )  = 
      let childIt        =  encloseIn   parentIt  
          ( _ , body' )  =  augmentStat body     childIt
      in  ( parentIt  , ScopedStat body' )
  
    augmentWhile ( WhileStat expr body _ )  = 
      let loopIt         =  encloseIn   parentIt 
          ( _ , body' )  =  augmentStat body     loopIt 
      in  ( parentIt , WhileStat expr body' parentIt )
  
    augmentIf ( IfStat expr thenBody elseBody _  )  =
      let thenIt             =  encloseIn   parentIt  
          elseIt             =  encloseIn   parentIt 
          ( _ , thenBody' )  =  augmentStat thenBody thenIt
          ( _ , elseBody' )  =  augmentStat elseBody elseIt
      in  ( parentIt , IfStat expr thenBody' elseBody' parentIt ) 
  
    -- Where the magic happens 
    augmentSeq ( SeqStat first second )  = 
      let ( parentIt'  , first'  )  =  augmentStat first  parentIt
          ( parentIt'' , second' )  =  augmentStat second parentIt'
      in  ( parentIt'' , SeqStat first' second' )


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
  checkDuplFuncs funcs ++ 
  concatMap checkDuplParams funcs ++ 
  concatMap checkFuncParamClash funcs 

 
-- | Check for semantic errors in a function
checkFuncParamClash :: Func -> [ SemanticError ]
checkFuncParamClash func = 
  let diffName ( Param _ pname )  =  pname /= nameOf func
      isUnique =  and . map diffName $ paramsOf func 
  in  if isUnique 
      then [] 
      else [ "Function and parameter share the same name @" ++ nameOf func ] 


checkDuplParams :: Func -> [ SemanticError ]
checkDuplParams func = 
  let genErr ps = duplParam ( head ps ) ( length ps )
      duplParam pname n = "Parameter " ++ pname ++ " defined " ++ show n ++ " times!"
      getName ( Param _ pname ) = pname
  in  map genErr . filter ( (<) 1 . length ) . group . sort . map getName $ paramsOf func
  

checkDuplFuncs :: [ Func ] -> [ SemanticError ] 
checkDuplFuncs funcs =  
  let genErr fs = duplFunc ( head fs ) ( length fs )
      duplFunc fname n = "Function " ++ fname ++ " defined " ++ show n ++ " times!"
  in  map genErr . filter ( (<) 1 . length ) . group . sort . map nameOf $ funcs
  
 







checkStat = undefined
