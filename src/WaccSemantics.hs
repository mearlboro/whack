module WaccSemantics where

import Data.Maybe                 ( isNothing , fromMaybe , fromJust )
import Data.Map                   ( empty                            )
import Control.Applicative hiding ( empty                            )
import Data.Char                  ( isSpace                          )
import Data.List                  ( group , sort                     )

import Utils
import WaccParser
import WaccDataTypes
import WaccSymbolTable



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Semantic Analisys ::::::::::::::::::::::::::::::::::::::::::::::::::::: --
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
