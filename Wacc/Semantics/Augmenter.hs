module Wacc.Semantics.Augmenter
( augmentProgram
) where

import Wacc.Data.DataTypes
import Wacc.Data.SymbolTable

-- ************************************************************************** --
-- ********************                           *************************** --
-- ********************   Semantic Augmentation   *************************** --
-- ********************                           *************************** --
-- ************************************************************************** --



augmentProgram
  :: Program -- | The Program AST
  -> Program -- | The agumented Program AST
-- | There is no "global scope" in WACC. Each function has its own little scope.
--   Nonetheless, we refer to the globalScope as the identifier table that
--   contains the function names, since the only "globally accessible" objects
--   are the functions defined at the beginning of a WACC program.
--   1. Create the globalScope and add the function names to it
--   2. Augment the main body, passing a reference to the globalScope
--   3. Augment all the functions. Each function will have its own scope but
--      will need to reference the globalScope in case other funcions are called
--      from within the function body
augmentProgram ( Program funcs main )  =  Program funcs' main'
  where
    fakeScope      =  addFuncs funcs emptyTable
    globalScope    =  encloseIn fakeScope True
    ( _ , main' )  =  augmentStat main globalScope
    funcs'         =  map ( augmentFunc globalScope ) funcs


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
augmentFunc
  :: It   -- | The globalScope
  -> Func -- | The Func AST with Empty identifier table
  -> Func -- | The Func AST with its funcionScope
-- | 1. The functionScope is an intermediate layer (table) that contains the
--      function name and parameters and is enclosed by the globalScope
--   2. Add the funcion name and all its parameters to the functionScope
--   3. The body of a function is evaluated in its own scope, enclosed by the
--      functionScope.
augmentFunc globalScope func@( Func ftype name params body _ )  =  func'
  where
    -- Function scope contains only function name
    funcScope      =  addFunc func $ encloseIn globalScope True

    -- Parameter scope contains the func args and is enclosed by func scope
    paramsScope    =  addParams params $ encloseIn funcScope True

    ( _ , body' )  =  augmentStat body $ encloseIn paramsScope True

    func'          =  Func ftype name params body' funcScope


 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
augmentStat
  :: Stat          -- | The Stat AST with Empty identifier table
  -> It            -- | The identifier table of the *previous* statement,
                   --   or the functionScope if this is the first statement
                   --   in a function body
  -> ( It , Stat ) -- | A pair containing the identifier table for the *next*
                   --   statement and the augmented Stat AST
-- | Given a statement and the table of identifiers of the *previous*
--   statement, return the table of identifiers for the *next* statement
--   together with the augmented Stat
augmentStat stat prevIt  = 

  case stat of

    SkipStat              -> (,) prevIt   $ SkipStat            
    FreeStat    expr    _ -> (,) prevIt   $ FreeStat    expr    prevIt
    ReturnStat  expr    _ -> (,) prevIt   $ ReturnStat  expr    prevIt
    ExitStat    expr    _ -> (,) prevIt   $ ExitStat    expr    prevIt
    PrintStat   expr    _ -> (,) prevIt   $ PrintStat   expr    prevIt
    PrintlnStat expr    _ -> (,) prevIt   $ PrintlnStat expr    prevIt
    ReadStat    lhs     _ -> (,) prevIt   $ ReadStat    lhs     prevIt
    AssignStat  _ _ _     -> augmentAssign  stat 
    DeclareStat _ _ _   _ -> augmentDeclare stat
    ScopedStat  _         -> augmentScoped  stat
    WhileStat   _ _     _ -> augmentWhile   stat
    IfStat      _ _ _   _ -> augmentIf      stat
    SeqStat     _ _       -> augmentSeq     stat

  where

    extractRhsExpr :: AssignRhs -> IdentName -> Expr 
    extractRhsExpr (RhsExpr       e      ) _   = e 
    extractRhsExpr  _                      id  = IdentExpr id 

    augmentAssign                                        :: Stat -> ( It , Stat )
    augmentAssign ( AssignStat ( LhsIdent id ) rhs it )  = 
      let nextIt  =  if   isDefined id it -- If not defined semantics will complain
                     then setExpr id (extractRhsExpr rhs id) $ encloseIn prevIt False
                     else prevIt
      in  ( nextIt , AssignStat ( LhsIdent id ) rhs nextIt )          

    augmentAssign ( AssignStat lhs rhs _ )  =  
      ( prevIt , AssignStat lhs rhs prevIt )

    -- |We have the previous table and we need to return the table for the next
    --  statement. This is the only case where the table is updated, since we
    --  just introduced a new variable that can be used by subsequent statements
    augmentDeclare                                   :: Stat -> ( It , Stat )
    augmentDeclare ( DeclareStat itype name rhs _ )  =
      let nextIt  =  addVariable name itype (extractRhsExpr rhs name) $ encloseIn prevIt False
      in  ( nextIt , DeclareStat itype name rhs nextIt )


    augmentScoped                      :: Stat -> ( It , Stat )
    augmentScoped ( ScopedStat body )  =
      -- Need to tell that the first statement in body is inside a new scope
      let ( _ , body' )  =  augmentStat body (encloseIn prevIt True) -- Begins a new scope
      in  ( prevIt , ScopedStat body' )


    augmentWhile                            :: Stat -> ( It , Stat )
    augmentWhile ( WhileStat expr body _ )  =
      let ( _ , body' )  =  augmentStat body (encloseIn prevIt True)
      in  ( prevIt , WhileStat expr body' prevIt )


    augmentIf                                 :: Stat -> ( It , Stat )
    augmentIf ( IfStat expr tbody ebody _  )  =
      let ( _ , tbody' )  =  augmentStat tbody (encloseIn prevIt True)
          ( _ , ebody' )  =  augmentStat ebody (encloseIn prevIt True)
      in  ( prevIt , IfStat expr tbody' ebody' prevIt )


    augmentSeq                           :: Stat -> ( It , Stat )
    augmentSeq ( SeqStat first second )  =
      -- * Where the magic happens * --
      let ( nextIt  , first'  )  =  augmentStat first  prevIt
          ( nextIt' , second' )  =  augmentStat second nextIt
      in  ( nextIt' , SeqStat first' second' )



