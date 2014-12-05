module Wacc.Data.DataTypes where

import qualified Data.Map as Map (Map)

-- qualified remove objLoc

-- ************************************************************************** --
-- **************************                  ****************************** --
-- **************************   WACC Grammar   ****************************** --
-- **************************    Data Types    ****************************** --
-- **************************                  ****************************** --
-- ************************************************************************** --


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | The following data types are deined and structured based on the language --
--   definition described in the WACC language specification.                 --
--   The lateral comments describe the Backus-Naur form syntax of WACC. -- -- --

data Program                                     -- <program> ::=
  = Program [ Func ] Stat                        -- 'begin' <func>* <stat> 'end'
  deriving ( Eq , Ord )

-- TODO rename into fType fName etc
data Func                                         -- <func> ::=
  = Func                                          -- <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end'
  { fType    :: Type
  , nameOf   :: IdentName
  , paramsOf :: ParamList
  , bodyOf   :: Stat
  , scopeOf  :: It
  } deriving ( Eq , Ord )

type ParamList = [ Param ]                       -- <param-list> ::= <param> (';' <param>)*

data Param                                       -- <param> ::=
  = Param                                        -- <type> <ident>
  { ptypeOf :: Type -- TODO
  , pnameOf :: IdentName
  } deriving ( Eq , Ord )

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Each statement is initialised with an empty identifier table. During the --
--   process of semantic analysis, this table It will be augmented with the   --
--   set of variables enclosed in the statement's scope. -- -- -- -- -- -- -- --
data Stat                                        -- <stat> ::=
  = SkipStat                                     -- 'skip'
  | FreeStat    Expr                          It -- 'free' <expr>
  | ReturnStat  Expr                          It -- 'return' <expr>
  | ExitStat    Expr                          It -- 'exit' <expr>
  | PrintStat   Expr                          It -- 'print' <expr>
  | PrintlnStat Expr                          It -- 'println' <expr>
  | ScopedStat  Stat                             -- 'begin' <stat> 'end'
  | ReadStat    AssignLhs                     It -- 'read' <assign-lhs>
  | WhileStat   Expr      Stat                It -- 'while' <expr> 'do' <stat> 'done'
  | SeqStat     Stat      Stat                   -- <stat> ';' <stat>
  | AssignStat  AssignLhs AssignRhs           It -- <assign-lhs> '=' <assign-rhs>
  | IfStat      Expr      Stat      Stat      It -- 'if' <expr> 'then' <stat> 'else' <stat> 'fi'
  | DeclareStat Type      IdentName AssignRhs It -- <type> <ident> '=' <assign-rhs>
  deriving ( Eq , Ord )

data AssignLhs                                   -- <assign-lhs> ::=
  = LhsIdent     IdentName                       -- <ident>
  | LhsPairElem  PairElem                        -- <pair-elem>
  | LhsArrayElem ArrayElem                       -- <array-elem>
  deriving ( Eq , Ord )

data AssignRhs                                   -- <assign-rghs> ::=
  = RhsExpr       Expr                           -- <expr>
  | RhsPairElem   PairElem                       -- <pair-elem>
  | RhsArrayLiter ArrayLiter                     -- <array-liter>
  | RhsNewPair    Expr       Expr                -- 'newpair' '(' <expr> ',' <expr> ')'
  | RhsCall       IdentName  ArgList             -- 'call' <ident> '(' <arg-list>? ')'
  deriving ( Eq , Ord )

type ArgList = [ Expr ]                          -- <arg-list> ::= <expr> (',' <expr> )

data PairElem                                    -- <pair-elem> ::=
  = Fst Expr                                     -- 'fst' <expr>
  | Snd Expr                                     -- 'snd' <expr>
  deriving ( Eq , Ord )

type ArrayElem = (IdentName, [ Expr ])           -- <array-elem> ::=

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | WACC's language specification defines a nested type system. In order to  --
--   make the type system stronger, the implementation in our compiler is     --
--   linear. Moreover, it features a NullType for the unallocated pair and an --
--   EmptyType for the wildcard type of the empty array. -- -- -- -- -- -- -- --
-- | For more details, see WaccParser. -- -- -- -- -- -- -- -- -- -- -- -- -- --
data Type                                        -- <type> ::=
  = IntType                                      -- 'int'
  | BoolType                                     -- 'bool'
  | CharType                                     -- 'char'
  | StringType                                   -- 'string'
  | PairType ( Maybe ( Type , Type ) )           -- <pair-type> ::= 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
  | ArrayType Type                               -- <array-type>
  | NullType                                     -- 'null'
  | EmptyType                                    -- <empty-array>
  deriving ( Eq , Ord )

data Expr                                        -- <expr> ::=
  = BoolLiterExpr     BoolLiter                  -- <bool-liter>
  | CharLiterExpr     CharLiter                  -- <char-liter>
  | IdentExpr         IdentName                  -- <ident>
  | UnaryOperExpr     UnaryOper Expr             -- <unary-oper> <expr>
  | ParenthesisedExpr Expr                       -- '(' <expr> ')'
  | IntLiterExpr      IntLiter                   -- <int-liter>
  | StrLiterExpr      StrLiter                   -- <str-liter>
  | PairLiterExpr                                -- <pair-liter>
  | ArrayElemExpr     ArrayElem                  -- <array-elem>
  | BinaryOperExpr    BinaryOper Expr Expr       -- <expr> <binary-oper> <expr>
  deriving ( Eq , Ord )

data UnaryOper                                   -- <unary-oper> ::=
  = NotUnOp                                      -- '!'
  | LenUnOp                                      -- 'len'
  | OrdUnOp                                      -- 'ord'
  | ChrUnOp                                      -- 'chr'
  | NegUnOp                                      -- '-'
  deriving ( Eq , Ord , Enum )

data BinaryOper                                  -- <binary-oper> ::=
  = AddBinOp                                     -- '+'
  | SubBinOp                                     -- '-'
  | MulBinOp                                     -- '*'
  | DivBinOp                                     -- '/'
  | ModBinOp                                     -- '%'
  | AndBinOp                                     -- '&&'
  | OrrBinOp                                     -- '||'
  | LsBinOp                                      -- '<'
  | GtBinOp                                      -- '>'
  | LEBinOp                                      -- '<='
  | GEBinOp                                      -- '>='
  | EqBinOp                                      -- '=='
  | NEBinOp                                      -- '!='
  deriving ( Eq , Ord , Enum )

type ArrayLiter = [ Expr ]                       -- <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']'

type IdentName  = [ Char ]                       -- <ident> ::= (' '|'a'-'z'|'A'-'Z')(' '|'a'-'z'|'A'-'Z'|'0'-'9')*

type IntLiter   = Int                            -- <int-sign>? <digit>+

type BoolLiter  = Bool                           -- <bool-liter> ::= 'true' | 'false'

type CharLiter  = Character                      -- <char-liter> ::= ''' <character> '''

type StrLiter   = [ Character ]                  -- <str-liter> ::= '"' <character>* '"'

type Character  = Char                           -- <character> ::= any-ASCII-character-except-'\'-'''-'"' | '\' <escaped-char>



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Grammar Type Utils :::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | Relaxed Eq class
class Eq a => Eq' a where
  (~==)    :: a -> a -> Bool
  (~/=)    :: a -> a -> Bool

  (~==)    =  (==)
  x ~/= y  =  not ( x ~== y )

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | A NullType is always a PairType and viceversa
--   A pair of any two types is always a PairType
--   An array of any type is always an ArrayType
--   A string is also an array type
instance Eq' Type where
  PairType  _ ~== PairType  _  =  True
  NullType    ~== PairType  _  =  True
  PairType  _ ~== NullType     =  True
  PairType  _ ~== _            =  False

  ArrayType _ ~== ArrayType _  =  True
  StringType  ~== ArrayType _  =  True
  ArrayType _ ~== StringType   =  True
  ArrayType _ ~== _            =  False

  t           ~== t'           =  t == t'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | A Function identifier always appears in the context of a Function,
--   regardless of the Func object it contains
instance Eq' Context where
  Function _  ~== Function _  =  True
  Function _  ~== _           =  False
  _           ~== Function _  =  False
  c           ~== c'          =  c == c'


instance Eq' PairElem where
  Fst _ ~== Fst _ = True 
  Snd _ ~== Snd _ = True 
  _     ~== _     = False 

-- For show instances, see GrammarShowInstances


-- ************************************************************************** --
-- **************************                  ****************************** --
-- **************************   Symbol Table   ****************************** --
-- **************************    Data  Type    ****************************** --
-- **************************                  ****************************** --
-- ************************************************************************** --

-- | General purpose symbol table
data SymbolTable k a
  = Empty
  | ST ( SymbolTable k a ) ( Map.Map k a )
  deriving ( Eq , Ord , Show )

-- | An identifier table is a symbol table that maps identifier names to
--   identifier objects. It may be empty or it may have: a dictionary (Map)
--   that maps the names to the objects, as well as an enclosing identifier
--   table. Identifier tables are usually linked with `scopes` in the program
--   and contain the map of identifier names to identifier objects.
type IdentTable = SymbolTable IdentName IdentObj

-- | An identifier may appear in a program as a Variable name, Function name
--   or Parameter name. In case of a funcion we save the actual Func object
data Context
  = Variable
  | Function Func
  | Parameter
  deriving ( Eq , Ord)

-- | An identifier object has a type and the context it appears in
data IdentObj 
  = IdentObj
  { objType :: Type 
  , objCtx  :: Context
  } deriving (Eq, Ord)

-- | Type synonym
type It = IdentTable

-- | A Dictionary maps names to identifier objects
type Dictionary = Map.Map IdentName IdentObj


