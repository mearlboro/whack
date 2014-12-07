module Wacc.Data.SymbolTable
( emptyTable
, encloseIn
, addParams
, addFuncs
, addFunc
, addVariable
, isDefined
, isRedefined
, findType
, findType'
, findContext
, findContext'
, isFunc
, isFunc'
, isVariable
, isVariable'
, isParam
, isParam'
, findEnclFunc
, findIdent
, findIdent'
, nonFunction
) where

import Wacc.Data.DataTypes
import Wacc.CodeGeneration.ARM11Instructions

import Control.Applicative ( (<$>)                                      )
import Data.Map            ( findWithDefault , Map (..) , insertWith
                           , lookup , toList , empty    , insert        )
import Data.Maybe          ( fromMaybe , fromJust , Maybe (..) , isJust
                           , isNothing                                  )
import Data.Tuple          ( swap                                       )
import Prelude hiding      ( lookup )--, empty                             )


-- ************************************************************************** --
-- **************************                  ****************************** --
-- **************************   Symbol Table   ****************************** --
-- **************************    Operations    ****************************** --
-- **************************                  ****************************** --
-- ************************************************************************** --


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Creation :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- | The global identifier table (global scope) that has no enclosing table
emptyTable  :: It
emptyTable  =  ST Empty empty True


-- | Create a new table enclosed by the given parent table
encloseIn                     :: It -> BeginsScope -> It
encloseIn parent beginsScope  =  ST parent empty beginsScope


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Insertion ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | Add a parameter list to the table
addParams            :: ParamList -> It -> It
addParams params it  =  foldr addParam it params

-- | Add a single paramter to the table
addParam                       :: Param -> It -> It
addParam ( Param ptype name )  =  addObject name ptype Parameter

-- | Add a list of functions to the table
addFuncs           :: [ Func ] -> It ->  It
addFuncs funcs it  =  foldr addFunc it funcs

-- | Add a single function to the table
addFunc                              :: Func -> It -> It
addFunc f@( Func ftype name _ _ _ )  =  addObject name ftype ( Function f )

-- | Add a variable to the table
addVariable             :: IdentName -> Type -> It -> It
addVariable name vtype  =  addObject name vtype Variable

-- | Add an object to the table
addObject                       :: IdentName -> Type -> Context -> It -> It
addObject name otype ctx table  =
  case table of
    -- Empty             -> ST Empty (insertIn empty TODO -- 
    ST encl dict bs   -> ST encl  (insertIn dict) bs
  where
    insertIn = insertWith onClash name $ IdentObj otype ctx -- (undefined, -1) 

-- | Handle case of re-declaration of a variable in the same scope
onClash          :: ( IdentObj -> IdentObj -> IdentObj )
onClash new old  =  new
-- error "Identifier Declared Twice In The Same Scope"


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Retrieval ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- Start looking from the exnclosed table
-- We want to start cehcking for redefinitions starting from the previoust 
-- statemetn (hence encl table)
isRedefined         :: IdentName -> It -> Bool 
isRedefined name it = isRedefined' (getEncl it) 
  where
    isRedefined'   Empty             =  False 
    isRedefined' ( ST _ dict True )  =  isJust $ lookup name dict 
    isRedefined'   it                =  case lookup name (getDict it) of 
                                          Nothing -> isRedefined' (getEncl it)
                                          Just _  -> True

getEncl                  :: It -> It 
getEncl   Empty          =  Empty
getEncl ( ST encl _ _ )  =  encl

getDict                  :: It -> Dictionary
getDict   Empty          =  error "NOOOOO"
getDict ( ST _ dict _ )  =  dict 


-- | Returns True iif the identifer is found in the table
isDefined          :: IdentName -> It -> Bool
isDefined name it  =  isJust $ findIdent name it

-- | Recursive version of isDefined
isDefined'          :: IdentName -> It -> Bool
isDefined' name it  =  isJust $ findIdent' name it

-- | Look for an identifier in the current table only
findIdent          :: IdentName -> It -> Maybe IdentObj
findIdent name it  =  lookup name `onDict` it

-- | Look for an identifier in the enclosed tables as well
findIdent'          :: IdentName -> It -> Maybe IdentObj
findIdent' name it  =
  -- If the lookup fails in this table, try the enclosed table
  -- If the lookup succeeds wrap its result in a Just.
  maybe ( findIdent' name `onEncl` it ) Just ( findIdent name it )

-- | Given an identifier table, finds the enclosing function.
--   The only case findEnclFunc should fail is when the table given is the
--   global scope or an empty table, otherwise it should always be able to
--   find the enclosing function.
findEnclFunc                       :: It -> Maybe Func
findEnclFunc      Empty            =  Nothing
findEnclFunc ( ST Empty _    _  )  =  Nothing -- Global scope
findEnclFunc ( ST encl  dict _  )  =
  case filter ( ~== Function {} ) . map objCtx . map snd $ toList dict of
    []             -> findEnclFunc encl
    [ Function f ] -> Just f
    _              -> Nothing
    -- error "Two Funcions In The Same Scope: This Should Never Happen"

-- | Find the type of an identifier in the table provided.
findType          :: IdentName -> It -> Maybe Type
findType name it  =  objType <$> findIdent name it

-- | Recursively find the type of an identifier in the table provided
findType'          :: IdentName -> It -> Maybe Type
findType' name it  =  objType <$> findIdent' name it

-- | Find the context of an identifier in the table provided.
findContext          :: IdentName -> It -> Maybe Context
findContext name it  =  objCtx <$> findIdent name it

-- | Recursively finds the context of an identifier in the table provided
findContext'          :: IdentName -> It -> Maybe Context
findContext' name it  =  objCtx <$> findIdent' name it


---- | Recursively finds the context of an identifier in the table provided
--findLocation          :: IdentName -> It -> (Reg, Int)
--findLocation name it  =  fromJust $ objLoc <$> findIdent name it

---- | Recursively finds the context of an identifier in the table provided
--findLocation'          :: IdentName -> It -> (Reg, Int)
--findLocation' name it  =  fromJust $ objLoc <$> findIdent' name it


-- | Does the identifier exist AND refer to a Function object?
isFunc          :: IdentName -> It -> Bool
isFunc name it  =  findContext name it == Just Function {}

-- | Recursive version of isFunc
isFunc'          :: IdentName -> It -> Bool
isFunc' name it  =  findContext' name it == Just Function {}

-- | Does the identifier name exist AND refer to a Variable object?
isVariable          :: IdentName -> It -> Bool
isVariable name it  =  findContext name it == Just Variable

-- | Recursive version of isVariable
isVariable'          :: IdentName -> It -> Bool
isVariable' name it  =  findContext' name it == Just Variable

-- | Does the identifier name exist AND refer to a Parameter object?
isParam          :: IdentName -> It -> Bool
isParam name it  =  findContext name it == Just Parameter

-- | Recursive version of isParam
isParam'          :: IdentName -> It -> Bool
isParam' name it  =  findContext' name it == Just Parameter

-- | Performs a retrieval operation on the table's dictionary
onDict                     :: ( Dictionary -> Maybe a ) -> It -> Maybe a
f `onDict` Empty           =  Nothing
f `onDict` ST    _ dict _  =  f dict

-- | Performs a retrieval operation on the table's enclosed table
onEncl                     :: ( It -> Maybe a ) -> It -> Maybe a
f `onEncl` Empty           =  Nothing
f `onEncl` ST    encl _ _  =  f encl

-- | Matches any context except a Function
nonFunction  :: [ Context ]
nonFunction  =  [ Variable , Parameter ]

