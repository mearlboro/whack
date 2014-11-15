module WaccSymbolTable
( emptyTable
, encloseIn
, addParams
, addFuncs
, addFunc
, addVariable
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
) where

import Data.Map            ( Map (..) 
                           , insert 
                           , empty 
                           , insertWith
                           , findWithDefault 
                           , lookup
                           , toList                            ) 
import Control.Applicative ( (<$>)                             )
import Data.Maybe          ( fromMaybe , fromJust , Maybe (..) )
import Data.Tuple          ( swap                              )
import Prelude hiding      ( lookup , empty                    )

import WaccDataTypes
import WaccShowInstances

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Creation :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | The global identifier table (global scope) that has no enclosing table
emptyTable  :: It 
emptyTable  =  ST Empty empty


-- | Create a new table enclosed by the given parent table
encloseIn         :: It -> It  
encloseIn parent  =  ST parent empty 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Insertion ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | Add a parameter list to the table
addParams           :: ParamList -> It -> It 
addParams plist it  =  foldr addParam it plist


-- | Add a single paramter to the table
addParam                        :: Param -> It -> It  
addParam ( Param ptype pname )  =  addObject pname ptype Parameter


-- | Add a list of functions to the table
addFuncs           :: [ Func ] -> It ->  It 
addFuncs funcs it  =  foldr addFunc it funcs


-- | Add a single function to the table 
addFunc                               :: Func -> It -> It 
addFunc f@( Func ftype fname _ _ _ )  =  addObject fname ftype ( Function f )
  

-- | Add a variable to the table
addVariable             :: IdentName -> Type -> It -> It 
addVariable name vtype  =  addObject name vtype Variable 


-- | Add an object to the table 
addObject                        :: IdentName -> Type -> Context -> It -> It 
addObject oname otype ctx table  =  
  case table of 
    Empty        -> ST Empty $ insertIn empty
    ST encl dict -> ST encl  $ insertIn dict 
  where
    insertIn = insertWith onClash oname ( otype , ctx )


-- | Handle case of re-declaration of a variable in the same scope
onClash          :: ( IdentObj -> IdentObj -> IdentObj )
onClash new old  =  error $ "Identifier Declared Twice In The Same Scope: " ++ 
                            show new ++ "->" ++ show old


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Retrieval ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- Look for an identifier in the current table only
findIdent          :: IdentName -> It -> Maybe IdentObj
findIdent name it  =  lookup name `onDict` it


-- Look for an identifier in the enclosed tables as well 
findIdent'          :: IdentName -> It -> Maybe IdentObj
findIdent' name it  =
  -- If the lookup fails in this table, try the enclosed table
  -- If the lookup succeeds wrap its result in a Just.
  maybe ( findIdent' name `onEncl` it ) Just ( findIdent name it )


-- | Given an identifier table, finds the enclosing function.
--   The only case findEnclFunc should fail is when the table given is the 
--   global scope or an empty table, otherwise it should always be able to 
--   find the enclosing function. -- TODO for this reason don't use maybe?
findEnclFunc                   :: It -> Maybe IdentObj
findEnclFunc   Empty           =  Nothing
findEnclFunc ( ST Empty _   )  =  Nothing -- Global scope
findEnclFunc ( ST encl dict )  =  
  case filter ( (==) Function {} . snd ) . map snd $ toList dict of 
    []    -> findEnclFunc encl
    [ x ] -> Just x
    _     -> error "Two Funcions In The Same Scope: This Should Never Happen"


-- | Find the type of an identifier in the table provided.
findType          :: IdentName -> It -> Maybe Type 
findType name it  =  fst <$> findIdent name it 


-- | Recursively find the type of an identifier in the table provided
findType'          :: IdentName -> It -> Maybe Type 
findType' name it  =  fst <$> findIdent' name it 


-- | Find the context of an identifier in the table provided.
findContext          :: IdentName -> It -> Maybe Context 
findContext name it  =  snd <$> findIdent name it 


-- | Recursively finds the context of an identifier in the table provided
findContext'          :: IdentName -> It -> Maybe Context 
findContext' name it  =  snd <$> findIdent' name it


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
onDict                   :: ( Dictionary -> Maybe a ) -> It -> Maybe a 
f `onDict` Empty         =  Nothing
f `onDict` ST    _ dict  =  f dict 


-- | Performs a retrieval operation on the table's enclosed table 
onEncl                   :: ( It -> Maybe a ) -> It -> Maybe a 
f `onEncl` Empty         =  Nothing
f `onEncl` ST    encl _  =  f encl




--isFuncCtx :: Context -> Bool 
--isFuncCtx ( Function _ ) = True 
--isFuncCtx            _   = False
{-
Confusingly the semantic analyser’s dictionary is termed the Symbol Table.
Note: the symbol table is not indexed on the symbols (tokens) of a program, 
rather it is indexed on the string name of identifier symbols (tokens). 
A better term would be to call it the Identifier Table or Identifier Dictionary.

class SymbolTable:
    
    SymbolTable encSymTable  # Link to enclosing symbol table
    Dictionary dict          # Maps names to objects
    
    def SymbolTable(SymbolTable st): # creates symbol table & link to enclosing table
        dict = Dictionary();         
        encSymTable = st;
    
    def add(name, obj):
        return dict.add(name, obj) # add name, obj to dictionary
    
    def lookupCurrLevelOnly(name): # return obj else None if name not in dict
        return dict.get(name) 
    
    def lookupCurrLevelAndEnclosingLevels(name): 
        S = self
        while S != None:
            obj = S.lookupCurrLevelOnly(name) # name found, return obj
            if obj != None: return obj        # name not found, move to enclosing ST
            S = S.encSymTable                 # name not found at any level, return
        return None
-}

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Failed Monad Attempt :::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

--instance Functor ( SymbolTable n ) where
--    _ `fmap` Empty        = Empty
--    f `fmap` ST encl dict = ST ( endl ) ( f `fmap` dict )

--instance Monad ( SymbolTable a ) where
--    return x = ST Empty $ singleton x undefined 

-- -- (>>=) :: ST a k -> ((a -> ST a j -> ST a j )
--    (>>=) ( ST encl dict ) f = \ident -> ST encl ( insert ident undefined dict ) 

--    fail = error "SymbolTable: symbol not found"


--infixr 4 <%>

--(<%>)               :: ( Dictionary -> Dictionary ) -> It -> It 
--_ <%> Empty         =  Empty
--f <%> ST encl dict  =  ST encl ( f dict )

--names = words "foo bar waz a b c hello world"

--contex = cycle [ Variable .. ]

--idents = zip ( repeat $ Just ( TypeBase IntBaseType ) ) contex 

--dict = zipWith3 insert names idents ( repeat empty )

--table = ST Empty dict 

--begin 
  
--  # [ foo :: int -> Func   ] is defined in the global scope
--  # 
--  # [ foo :: char -> Param ]
--  # [ bar :: bool -> Param ] are defined in the function scope 
--  # 
--  # Function body introduces a new inner scope
--  # [ bool 
--  # [  
--  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
--  int foo(char foo, bool bar) is 
--        foo = '?'       # Legal
--        bool foo = 3    # Illegal 

--        int result = call foo('!') # Illegal, foo is now a char
--  end 

--  # b :: bool -> Func  { global scope   }
--  # i :: int  -> Param { function scope }
--  bool b(int i) is 

--     # b :: char -> Variable { body scope }
--     # This is legal?
--     char b   = '?'

--     # Illegal since we have redefined b to be a 
--     bool r = call b(i) 
--  end 

--  string baz = "Hello"

--  pair(null, null) foo = null # Illegal


--end 


--globalScope :: { foo:int:FUNCION , bar:bool:FUNCION}

