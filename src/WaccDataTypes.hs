-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 1. Datatype Definitions ::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

module WaccDataTypes where


data Program                                    -- <program> ::=
  = Program [ Func ] Stat                       -- 'begin' <func>* <stat> 'end'   
  deriving ( Show , Eq )  
  
data Func                                       -- <func> ::= 
  = Func Type Ident ParamList Stat              -- <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end'  
  deriving ( Show , Eq )  
  
type ParamList = [ Param ]                      -- <param-list> ::= <param> (';' <param>)*   
  
data Param                                      -- <param> ::=
  = Param Type Ident                            -- <type> <ident>                                          
  deriving ( Show , Eq )  

data Stat                                       -- <stat> ::=
  = SkipStat                                    -- 'skip' 
  | FreeStat    Expr                            -- 'free' <expr>
  | ReturnStat  Expr                            -- 'return' <expr>
  | ExitStat    Expr                            -- 'exit' <expr>
  | PrintStat   Expr                            -- 'print' <expr>
  | PrintlnStat Expr                            -- 'println' <expr>
  | ScopedStat  Stat                            -- 'begin' <stat> 'end'
  | ReadStat    AssignLhs                       -- 'read' <assign-lhs>
  | WhileStat   Expr      Stat                  -- 'while' <expr> 'do' <stat> 'done'
  | SeqStat     Stat      Stat                  -- <stat> ';' <stat>
  | AssignStat  AssignLhs AssignRhs             -- <assign-lhs> '=' <assign-rhs>
  | IfStat      Expr      Stat      Stat        -- 'if' <expr> 'then' <stat> 'else' <stat> 'fi'
  | DeclareStat Type      Ident     AssignRhs   -- <type> <ident> '=' <assign-rhs>
  deriving ( Show , Eq )  
  
data AssignLhs                                  -- <assign-lhs> ::=
  = LhsIdent     Ident                          -- <ident>
  | LhsPairElem  PairElem                       -- <pair-elem>
  | LhsArrayElem ArrayElem                      -- <array-elem>
  deriving ( Show , Eq )                            
  
data AssignRhs                                  -- <assign-rghs> ::=
  = RhsExpr       Expr                          -- <expr>
  | RhsPairElem   PairElem                      -- <pair-elem>
  | RhsArrayLiter ArrayLiter                    -- <array-liter>
  | RhsNewPair    Expr       Expr               -- 'newpair' '(' <expr> ',' <expr> ')'
  | RhsCall       Ident      ArgList            -- 'call' <ident> '(' <arg-list>? ')'
  deriving ( Show , Eq )        
  
type ArgList = [ Expr ]                         -- <arg-list> ::= <expr> (',' <expr> )             
  
data PairElem                                   -- <pair-elem> ::=
  = Fst Expr                                    -- 'fst' <expr>
  | Snd Expr                                    -- 'snd' <expr>
  deriving ( Show , Eq )          
  
data Type                                       -- <type> ::=
  = IntType                                     -- 'int' 
  | BoolType                                    -- 'bool'
  | CharType                                    -- 'char'
  | StringType                                  -- 'string'
  | PairType ( Maybe ( Type, Type ) )           -- <pair-type> ::= 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'    
  | ArrayType Type                              -- <array-type>
  | NullType
  | EmptyType
  deriving ( Show , Eq )                        
  
data Expr                                       -- <expr> ::=
  = BoolLiterExpr     Bool                      -- <bool-liter>
  | CharLiterExpr     Char                      -- <char-liter>
  | IdentExpr         Ident                     -- <ident>
  | UnaryOperExpr     UnaryOper Expr            -- <unary-oper> <expr>
  | ParenthesisedExpr Expr                      -- '(' <expr> ')'
  | IntLiterExpr      IntLiter                  -- <int-liter>
  | StrLiterExpr      StrLiter                  -- <str-liter>
  | PairLiterExpr     PairLiter                 -- <pair-liter>
  | ArrayElemExpr     ArrayElem                 -- <array-elem>
  | BinaryOperExpr    BinaryOper Expr Expr      -- <expr> <binary-oper> <expr>
  deriving ( Show , Eq )   
  
data UnaryOper                                  -- <unary-oper> ::=
  = NotUnOp                                     -- '!'
  | LenUnOp                                     -- 'len'
  | OrdUnOp                                     -- 'ord'
  | ChrUnOp                                     -- 'chr'
  | NegUnOp                                     -- '-'
  deriving ( Show , Eq )  
  
data BinaryOper                                 -- <binary-oper> ::=
  = AddBinOp                                    -- '+'
  | SubBinOp                                    -- '-'
  | MulBinOp                                    -- '*'
  | DivBinOp                                    -- '/'
  | ModBinOp                                    -- '%'
  | AndBinOp                                    -- '&&'
  | OrrBinOp                                    -- '||'
  | LsBinOp                                     -- '<'
  | GtBinOp                                     -- '>'
  | LEBinOp                                     -- '<='
  | GEBinOp                                     -- '>='
  | EqBinOp                                     -- '=='
  | NEBinOp                                     -- '!='
  deriving ( Show , Eq )      
  
type Ident = [ Char ]                           -- <ident> ::= (' '|'a'-'z'|'A'-'Z')(' '|'a'-'z'|'A'-'Z'|'0'-'9')*  
  
data ArrayElem                                  -- <array-elem> ::=
  = ArrayElem Ident [ Expr ]                    -- <ident> '[' <expr> ']'
  deriving ( Show , Eq )  
  
type IntLiter  = Integer

type BoolLiter = Bool                           -- <bool-liter> ::= 'true' | 'false'   
  
type CharLiter = Character                      -- <char-liter> ::= ''' <character> '''                  
  
type StrLiter  = [ Character ]                  -- <str-liter> ::= '"' <character>* '"'   
  
type Character = Char                           -- <character> ::= any-ASCII-character-except-'\'-'''-'"' | '\' <escaped-char>   

data EscapedChar                                -- <escaped-char> ::=
  = Tab                                         -- 't' 
  | NewLine                                     -- 'n' 
  | NewPage                                     -- 'f'
  | Backspace                                   -- 'b'
  | Backslash                                   -- '\'
  | SingleQuote                                 -- '''
  | DoubleQuotes                                -- '"'
  | CarriageReturn                              -- 'r'
  | NullTerminator                              -- '0'
  deriving ( Show , Eq )  
  
type ArrayLiter = [ Expr ]                      -- <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']'                     
  
data PairLiter                                  -- <pair-liter> ::=
  = Null                                        -- 'null'
  deriving ( Show , Eq )  
  
