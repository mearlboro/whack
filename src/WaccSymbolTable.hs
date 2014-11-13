module WaccSymbolTable
( SymbolTable (..)
, Scope
, newScope
, addParam
, addFunc
, addVariable
, findIdent
, findIdent'
) where

import Data.Map       ( Map (..) 
                      , insert 
                      , empty 
                      , insertWith
                      , findWithDefault 
                      , lookup )

import Prelude hiding ( lookup , empty )

import WaccDataTypes

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Symbol Table :::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | General purpose symbol table
data SymbolTable k a 
    = Empty 
    | ST ( SymbolTable k a ) ( Map k a )
    deriving ( Show )

-- | An identifier table is a symbol table that maps variable names to 
--   identifier objects. it may be empty or it may have: a dictionary (Map) 
--   that maps the names to the objects, as well as an enclosing identifier 
--   table. Identifier tables represent `scopes` in the program
type IdentTable = SymbolTable Name Identifier

-- | An identifier may appear in a program as a Variable name, Function name 
--   or Parameter name 
data Context = 
    Variable | Function | Parameter 
    deriving ( Eq , Show , Enum , Ord )

-- | An identifier has a type and a context
type Identifier = ( Type , Context )


-- | An Identifier name is just a string
type Name = [ Char ]

-- | An IdentTable is just a scope 
type Scope = IdentTable

-- | A Dictionary maps names to identifier objects
type Dictionary = Map Name Identifier


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Creation :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | Creates a new scope enclosed by the parenting scope
newScope         :: Scope -> Scope 
newScope parent  =  ST parent empty 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Insertion ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | Add a parameter to the table ( new variable in scope )
addParam                       :: Param -> Scope -> Scope 
addParam ( Param ptype name )  =  addObject name ptype Parameter


-- | Add a function to the table ( new funcion in global scope )
addFunc                          :: Func -> Scope -> Scope 
addFunc ( Func ftype name _ _ )  =  addObject name ftype Function 


-- | Add a variable to the table ( new variable in scope )
addVariable            :: Name -> Type -> Scope -> Scope 
addVariable var vtype  =  addObject var vtype Variable


-- | Add an object to the table 
addObject                       :: Name -> Type -> Context -> Scope -> Scope 
addObject name otype ctx table  =  case table of 
    Empty        -> ST Empty $ insert name ( otype , ctx ) empty
    ST encl dict -> ST encl  $ insert name ( otype , ctx ) dict  


-- | Handle case of redeclaration of a variable already declared
onInsert :: ( Identifier -> Identifier -> Identifier )
onInsert = undefined

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Retrieval ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- Look for an identifier in the current scope only
findIdent          :: Name -> Scope -> Maybe Identifier
findIdent name it  =  lookup name `onDict` it


-- Look for an identifier in the enclosed scopes as well 
findIdent' :: Name -> Scope -> Maybe Identifier
findIdent' name it 
  -- If the lookup fails in this table, try the enclosed table
  -- If the lookup succeeds wrap its result int a Just.
  = maybe ( findIdent' name `onEncl` it ) Just ( lookup name `onDict` it )


-- | Performs a retrieval operation on the table's dictionary
onDict                   :: ( Dictionary -> Maybe a ) -> Scope -> Maybe a 
f `onDict` Empty         =  Nothing
f `onDict` ST    _ dict  =  f dict 


-- | Performs a retrieval operation on the table's enclosed table 
onEncl                   :: ( Scope -> Maybe a ) -> Scope -> Maybe a 
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

