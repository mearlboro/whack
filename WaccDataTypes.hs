module WaccDataTypes where

-- <program>    ::= 'begin' <func>* <stat> 'end'
data Program 
  = Program     [Func] Stat
  deriving (Eq, Show)

-- <func>       ::= <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end'
data Func
  = Type        Ident ParamList Stat
  deriving (Eq, Show)

-- <param-list> ::= <param> (';' <param>)*
data ParamList
  = ParamList   [Param]
  deriving (Show)

-- <param>      ::= <type> <ident>
data Param 
  = Param       Type Ident
  deriving (Eq, Show)

-- <stat>
data Stat
  = NoOpStat
  | AssignStat  Type Ident AssignRhs
  | AssignStat  AssignLhs AssignRhs
  | ReadStat    AssignLhs
  | FreeStat    Expr
  | ReturnStat  Expr
  | ExitStat    Expr
  | PrintStat   Expr
  | PrintlnStat Expr 
  | IfStat      Expr Stat Stat
  | WhileStat   Expr Stat
  | ScopedStat  Stat
  | SeqStat     Stat Stat
  deriving (Eq, Show)

-- <assign-lhs>
data AssignLhs 
  = Ident
  | ArrayElem
  | PairElem
  deriving (Show)

-- <assign-rhs>
data AssignRhs
  = Expr
  | ArrayLiter
  | NewPair      Expr Expr
  | PairElem
  | Call         Func
  deriving (Show)

-- <arg-list>   ::= <expr> (',' <expr>)* 
data ArgList
  = ArgList     [Expr]
  deriving (Show)

-- <pair-elem>
data PairElem
  = Fst         Expr
  | Snd         Expr
  deriving (Show)

-- <type> 
data Type
  = BaseType
  | ArrayType
  | PairType
  deriving (Show)
=======
--Data types definitions for WACC compiler
type ValidDigit = Char

data Digit = Digit ValidDigit

data Sign  = PLUS
           | MINUS 

data BoolLiter  = TRUE
                | FALSE 

data CharLiter  = CharLiter Character

data StrLiter   = StrLiter [Character]

data Character  = ValidCharacter ValidChar
                | EscapeCharacter EscapedChar
 
data EscapedChar  = NUL
                  | BS
                  | TAB
                  | LF
                  | FF
                  | CR
                  | DoubleQ
                  | SingleQ
                  | BackSlash

data ArrayLiter = ArrayLiter [Expr]

data PairLiter  = NullPairLiter

data Comment    = Comment [Char]





