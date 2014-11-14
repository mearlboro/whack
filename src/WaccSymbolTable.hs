module WaccSymbolTable
( emptyTable
, encloseIn
, addParams
, addFuncs
, addFunc
, findEnclFunc
, addVariable
, findIdent
, findIdent'
) where

import Data.Map       ( Map (..)
                      
                      , insert 
                      , empty 
                      , insertWith
                      , findWithDefault 
                      , lookup
                      , toList ) 

import Prelude hiding      ( lookup , empty )
import Control.Applicative ( (<$>)          )
import Data.Maybe ( fromMaybe , fromJust , Maybe (..) )
import Data.Tuple ( swap )

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
addParams  :: It -> ParamList -> It 
addParams  =  foldr addParam
  where
    addParam                       :: Param -> It -> It  
    addParam ( Param ptype name )  =  addObject name ptype Parameter


-- | Add a list of functions to the table
addFuncs  :: It -> [ Func ] -> It 
addFuncs  =  foldr addFunc


addFunc                              :: Func -> It -> It 
addFunc f@( Func ftype name _ _ _ )  =  addObject name ftype ( Function f )
  

-- | Add a variable to the table
addVariable                :: It -> IdentName -> Type -> It 
addVariable it name vtype  =  addObject name vtype Variable it 


-- | Add an object to the table 
addObject                       :: IdentName -> Type -> Context -> It -> It 
addObject name otype ctx table  =  
  case table of 
    Empty        -> ST Empty $ insertIn empty
    ST encl dict -> ST encl  $ insertIn dict 
  where
    insertIn = insertWith onClash name ( otype , ctx )


-- | Handle case of re-declaration of a variable already declared
onClash          :: ( IdentObj -> IdentObj -> IdentObj )
onClash new old  =  new
    --error $ "Identifier Declared Twice In The Same Scope: " ++ 
    --        show new ++ "->" ++ show old


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Retrieval ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- Look for an identifier in the current scope only
findIdent          :: IdentName -> It -> Maybe IdentObj
findIdent name it  =  lookup name `onDict` it


-- Look for an identifier in the enclosed scopes as well 
findIdent' :: IdentName -> It -> Maybe IdentObj
findIdent' name it 
  -- If the lookup fails in this table, try the enclosed table
  -- If the lookup succeeds wrap its result int a Just.
  = maybe ( findIdent' name `onEncl` it ) Just ( findIdent name it )



findEnclFunc                   :: It -> Maybe IdentObj
findEnclFunc   Empty           =  Nothing
findEnclFunc ( ST Empty _   )  =  Nothing -- Global scope
findEnclFunc ( ST encl dict )  =  
  case filter ( isFuncCtx . snd ) . map snd $ toList dict of 
    []  -> findEnclFunc encl
    [x] -> Just x
    _   -> error "Two Funcions In The Same Scope: This Should Never Happen"



isFuncCtx :: Context -> Bool 
isFuncCtx ( Function _ ) = True 
isFuncCtx            _   = False

findType          :: IdentName -> It -> Maybe Type 
findType name it  =  fst <$> findIdent name it 


findType'          :: IdentName -> It -> Maybe Type 
findType' name it  =  fst <$> findIdent' name it 


findContext          :: IdentName -> It -> Maybe Context 
findContext name it  =  snd <$> findIdent name it 


findContext'          :: IdentName -> It -> Maybe Context 
findContext' name it  =  snd <$> findIdent' name it



isFunc          :: IdentName -> It -> Bool 
isFunc name it  =  findContext name it == Just Function {} 


isFunc'          :: IdentName -> It -> Bool 
isFunc' name it  =  findContext' name it == Just Function {} 


isVariable          :: IdentName -> It -> Bool 
isVariable name it  =  findContext name it == Just Variable


isVariable'          :: IdentName -> It -> Bool 
isVariable' name it  =  findContext' name it == Just Variable


isParam          :: IdentName -> It -> Bool 
isParam name it  =  findContext name it == Just Parameter 


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

