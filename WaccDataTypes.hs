
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Char ( isDigit , isLetter )


data Token                                      -- Terminal characters 'if' ',' ']' ... Terminals are lexical tokens
  = BEGIN    
  | END       
  | IS     
  | SKIP     
  | READ      
  | FREE 
  | RETURN 
  | EXIT 
  | PRINT 
  | PRINTLN
  | IF       
  | THEN      
  | ELSE 
  | FI
  | WHILE    
  | DO        
  | DONE
  | CALL
  | NEWPAIR 
  | FST 
  | SND
  | INT
  | BOOL 
  | CHAR 
  | STRING 
  | PAIR   
  | LEN    
  | ORD 
  | CHR 
  | NULL              -- 'null' pair
  | TRUE  
  | FALSE
  | HASHTAG           -- '#'
  | SEMICOLON         -- ';'
  | COMMA             -- ','
  | PARENTHESIS_LEFT  -- '('
  | PARENTHESIS_RIGHT -- ')'
  | BRACKET_LEFT      -- '['
  | BRACKET_RIGHT     -- '['
  | EXCLAMATION_MARK  -- '!' 
  | TIMES             -- '*'
  | UNDERSCORE        -- '_'
  | PERCENT           -- '%'
  | PLUS              -- '+'
  | DASH              -- '-'
  | EQUALS            -- '='
  | GREATER           -- '>'
  | GREATER_EQUAL     -- '>='        
  | LESS              -- '<'
  | LESS_EQUAL        -- '<='
  | DOUBLE_EQUALS     -- '=='
  | NOT_EQUAL         -- '!='
  | AND               -- '&&'
  | OR                -- '||'
  | NULL_TERMINATOR   -- '0'
  | BACKSPACE         -- 'b'
  | TAB               -- 't'
  | NEW_LINE          -- 'n'
  | NEW_PAGE          -- 'f'
  | CARRIAGE_RETURN   -- 'r'
  | BACKSLASH         -- '\'
  | SLASH             -- '/'
  | SINGLE_QUOTE      -- '''
  | DOUBLE_QUOTES     -- '"'
  -- | IDENT ValidIdent -- maybe needed later
  -- | NUMBER Integer

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
  = TypeBase  BaseType                          -- <base-type>
  | TypePair  PairType                          -- <pair-type>
  | TypeArray ArrayType                         -- <array-type>
  deriving ( Show , Eq )                        
  
data BaseType                                   -- <base-type> ::=
  = IntBaseType                                 -- 'int' 
  | BoolBaseType                                -- 'bool'
  | CharBaseType                                -- 'char'
  | StringBaseType                              -- 'string'
  deriving ( Show , Eq )

data ArrayType                                  -- <array-type> ::=
  = ArrayType Type                              -- <type> '[' ']'  
  deriving ( Show , Eq )                 

type PairType = ( PairElemType , PairElemType ) -- <pair-type> ::= 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'   

data PairElemType                               -- <pair-elem-type> ::=
  = PairPairElemType                            -- 'pair'
  | BasePairElemType  BaseType                  -- <base-type>
  | ArrayPairElemType ArrayType                 -- <array-type>
  deriving ( Show , Eq )   
  
data Expr                                       -- <expr> ::=
  = BoolLiterExpr     Bool                      -- <bool-liter>
  | CharLiterExpr     Char                      -- <char-liter>
  | IdentExpr         Ident                     -- <ident>
  | UnaryOperExpr     Expr                      -- <unary-oper> <expr>
  | ParenthesisedExpr Expr                      -- '(' <expr> ')'
  | IntLiterExpr      IntLiter                  -- <int-liter>
  | StrLiterExpr      StrLiter                  -- <str-liter>
  | PairLiterExpr     PairLiter                 -- <pair-liter>
  | ArrayElemExpr     ArrayElem                 -- <array-elem>
  | BinaryOperExpr    Expr      Expr            -- <expr> <binary-oper> <expr>
  deriving ( Show , Eq )   
  
data UnaryOper                                  -- <unary-oper> ::=
  = NotUnOp                                     -- '!'
  | LenUnOp                                     -- 'len'
  | OrdUnOp                                     -- 'ord'
  | ChrUnOp                                     -- 'chr'
  | NegUnOp                                     -- '-'
  deriving ( Show , Eq )  
  
data BinaryOper                                 -- <binary-oper> ::=
  = OrBinOp                                     -- '||'
  | DivBinOp                                    -- '/'
  | ModBinOp                                    -- '%'
  | AddBinOp                                    -- '+'
  | SubBinOp                                    -- '-'
  | AndBinOp                                    -- '&&'
  | LessBinOp                                   -- '<'
  | TimesBinOp                                  -- '*'
  | LessEqBinOp                                 -- '<='
  | EqualsBinOp                                 -- '=='
  | GreaterBinOp                                -- '>'
  | NotEqualsBinOp                              -- '!='
  | GreaterEqBinOp                              -- '>='
  deriving ( Show , Eq )      
  
type Ident = [ Char ]                           -- <ident> ::= (' '|'a'-'z'|'A'-'Z')(' '|'a'-'z'|'A'-'Z'|'0'-'9')*  
  
data ArrayElem                                  -- <array-elem> ::=
  = ArrayElem Ident Expr                        -- <ident> '[' <expr> ']'
  deriving ( Show , Eq )  
  
data IntLiter                                   -- <int-liter> ::=
  = IntLiter ( Maybe IntSign ) Integer {-*-}    -- <int-sign>? <digit>+ 
  deriving ( Show , Eq )  
  
{- removed Digit, use Integer -}                -- <digit> ::= ('0'-'9')  
  
data IntSign                                    -- <int-sign> ::=
  = Plus                                        -- '+'
  | Minus                                       -- '-'    
  deriving ( Show , Eq )  
  
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
  
type Comment = [ Char ]                         -- <comment> ::= '#' (any-character-except-EOL)* (EOL)

      


