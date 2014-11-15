module WaccDataTypes where -- TODO rename into WaccTypes ?

import Data.Map ( Map (..) )

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 1. WACC Data Types :::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- TODO fit each comment into 80 chars

data Program                                     -- <program> ::=
  = Program [ Func ] Stat                        -- 'begin' <func>* <stat> 'end'   
  deriving ( Eq , Ord )   
 

data Func                                         -- <func> ::= 
  = Func                                          -- <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end'  
  { typeOf   :: Type 
  , nameOf   :: IdentName
  , paramsOf :: ParamList
  , bodyOf   :: Stat 
  , scopeOf  :: It 
  } deriving ( Eq , Ord )
 
type ParamList = [ Param ]                       -- <param-list> ::= <param> (';' <param>)*   
   
 
-- TODO make into a type synonym ?
-- type Param = ( Type , IdentName )
data Param                                       -- <param> ::=
  = Param                                        -- <type> <ident> 
  { ptypeOf :: Type                                
  , pnameOf :: IdentName
  } deriving ( Eq , Ord )  


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
 
-- TODO make into a type synonym
data ArrayElem                                   -- <array-elem> ::=
  = ArrayElem IdentName [ Expr ]                 -- <ident> '[' <expr> ']'
  deriving ( Eq , Ord )   
 
 
data Type                                        -- <type> ::=
  = IntType                                      -- 'int' 
  | BoolType                                     -- 'bool'
  | CharType                                     -- 'char'
  | StringType                                   -- 'string'
  | PairType ( Maybe ( Type, Type ) )            -- <pair-type> ::= 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'    
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

type IntLiter   = Integer                        -- TODO add BNF description

type BoolLiter  = Bool                           -- <bool-liter> ::= 'true' | 'false'   
  
type CharLiter  = Character                      -- <char-liter> ::= ''' <character> '''                  
  
type StrLiter   = [ Character ]                  -- <str-liter> ::= '"' <character>* '"'   
  
type Character  = Char                           -- <character> ::= any-ASCII-character-except-'\'-'''-'"' | '\' <escaped-char>   


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 2. Symbol Table ::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- | General purpose symbol table
data SymbolTable k a 
  = Empty 
  | ST ( SymbolTable k a ) ( Map k a ) 
  deriving ( Eq , Ord )


-- | An identifier table is a symbol table that maps identifier names to 
--   identifier objects. It may be empty or it may have: a dictionary (Map) 
--   that maps the names to the objects, as well as an enclosing identifier 
--   table. Identifier tables represent `scopes` in the program, but not quite.
--   They are indeed just identifier tables that contain the most 'updated'
--   map of identifier names to identifier objects.
type IdentTable = SymbolTable IdentName IdentObj


-- | An identifier may appear in a program as a Variable name, Function name 
--   or Parameter name. In case of a funcion we save the actual Func object
data Context 
  = Variable 
  | Function Func 
  | Parameter 
  deriving ( Eq , Ord )


-- | An identifier object has a type and the context it appears in
type IdentObj = ( Type , Context )

-- | Type synonym
type It = IdentTable

-- | A Dictionary maps names to identifier objects
type Dictionary = Map IdentName IdentObj

