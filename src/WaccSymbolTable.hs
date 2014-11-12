module WaccSymbolTable
( SymbolTable (..)
, IdentTable
, It
, addParam
, addFunc
, addVariable
, findIdent
, findIdent'
) where

import Data.Map ( Map (..) , insert , empty , findWithDefault , lookup )
import Prelude hiding ( lookup , empty )

import WaccDataTypes

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Symbol Table :::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | General puropose symbol table
data SymbolTable k a = Empty | ST ( SymbolTable k a ) ( Map k a )

-- | Type synonim
type St k a = SymbolTable k a 

-- | An identifier may appear as a variable, function or parameter name 
data Context = Variable | Function | Parameter 

-- | An identifier has a type (Nothing if it is not known yet) and a context
type Identifier = ( Maybe Type , Context )

-- | Identifier name is just a string
type Name = [ Char ]

-- | An identifier table is a symbol table that maps variable names to 
--   identifier objects. it may be empty or it may have a dictionary mapping 
--   identifiers with their names as well as an enclosing identifier table.
--   The enclosing table represents the parten scope
type IdentTable = SymbolTable Name Identifier

-- | Type synonim 
type It = IdentTable

-- | Type synonim 
type Dictionary = Map Name Identifier


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Insertion ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | Add a parameter to the table 
addParam                       :: Param -> It -> It 
addParam ( Param ptype name )  =  addObject name ( Just ptype ) Parameter


-- | Add a function to the table 
addFunc                          :: Func -> It -> It 
addFunc ( Func ftype name _ _ )  =  addObject name ( Just ftype ) Function 


-- | Add a variable to the table
addVariable            :: Ident -> Maybe Type -> It -> It 
addVariable var vtype  =  addObject var vtype Variable


-- | Add an object to the table 
addObject                       :: String -> Maybe Type -> Context -> It -> It 
addObject name otype ctx table  =  case table of 
    Empty        -> Empty 
    ST encl dict -> ST encl $ insert name ( otype , ctx ) dict  


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Retrieval ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | Performs a retrieval operation on the table dictionary
onDict                   :: ( Dictionary -> Maybe a ) -> It -> Maybe a 
f `onDict` Empty         =  Nothing
f `onDict` ST    _ dict  =  f dict 


-- | Performs a retrieval operation on the table's enclosed table 
onEncl                   :: ( It -> Maybe a ) -> It -> Maybe a 
f `onEncl` Empty         =  Nothing
f `onEncl` ST    encl _  =  f encl


-- Look for an identifier in the current scope only
findIdent          :: String -> It -> Maybe Identifier
findIdent name it  =  lookup name `onDict` it


-- Look for an identifier in the enclosed scopes as well 
findIdent' :: String -> It -> Maybe Identifier
findIdent' name it 
  = maybe ( findIdent' name `onEncl` it ) Just ( lookup name `onDict` it )

    
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

