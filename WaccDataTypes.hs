module WaccDataTypes 
((..))

where

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

 

reservedWords :: [ String ]
reservedWords 
  = [ "begin" , "end", "is", "skip", "read", "free", "return", "exit", "print", "println"
    , "if", "then", "else", "fi", "while", "do", "done", "call", "newpair", "fst", "snd"
    , "int", "bool", "char", "string", "pair", "len", "ord", "chr", "null", "true", "false" ]

reservedOperators :: [ String ]
reservedOperators
  = [ "+",  "-", "*",  "/",  "=",  "==", "!=", "<", ">", "<="
    , ">=", "&&", "||", "%", "len", "ord", "chr", "!"] 

languageDef -- TODO write type signature
  = emptyDef 
  { Token.commentLine     = "#"
  , Token.identStart      = letter   <|> char '_'
  , Token.identLetter     = alphaNum <|> char '_'
  , Token.reservedNames   = reservedWords
  , Token.reservedOpNames = reservedOperators 
  }

lexer = Token.makeTokenParser languageDef -- TODO write type signature

identifier = Token.identifier lexer -- parses an identifier -- TODO write type signature
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis: parens p takes care of the parenthesis and uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
     
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Abstract Data Tree Formation :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <program> PARSER 


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <func> PARSER 

-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: <func> PARSER 



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Expression PARSER 

-- :: Arithetic expression :: --
arithExpr :: Parser Expr
arithExpr = buildExpressionParser arithOpers arithTerm
 
-- :: Boolean expression :: --
boolExpr :: Parser Expr
boolExpr = buildExpressionParser boolOpers boolTerm

-- :: Arithmetic operators :: --
arithOpers 
  = [ [ Prefix ( reservedOp "!"   >> return NotUnOp        )           ]
    , [ Prefix ( reservedOp "len" >> return LenUpOp        )           ]
    , [ Prefix ( reservedOp "ord" >> return OrdUpOp        )           ]
    , [ Prefix ( reservedOp "chr" >> return ChrUnOp        )           ]
    , [ Prefix ( reservedOp "-"   >> return NegUnOp        ) AssocLeft ] 
    , [ Infix  ( reservedOp "+"   >> return PlusBinOp      ) AssocLeft ] 
    , [ Infix  ( reservedOp "*"   >> return TimesBinOp     ) AssocLeft ] 
    , [ Infix  ( reservedOp "/"   >> return DivBinOp       ) AssocLeft ] 
    , [ Infix  ( reservedOp "%"   >> return ModBinOp       ) AssocLeft ] 
    , [ Infix  ( reservedOp "<"   >> return LessBinOp      ) AssocLeft ]
    , [ Infix  ( reservedOp "<="  >> return LessEqBinOp    ) AssocLeft ] 
    , [ Infix  ( reservedOp ">"   >> return GreaterBinOp   ) AssocLeft ] 
    , [ Infix  ( reservedOp ">="  >> return GreaterEqBinOp ) AssocLeft ] ]

-- :: Bolean operators :: --
bOperators 
  = [ [ Infix  ( reservedOp "=="  >> return EqualsBinOp    ) AssocLeft ]
    , [ Infix  ( reservedOp "!="  >> return NotEqualsBinOp ) AssocLeft ]
    , [ Infix  ( reservedOp "&&"  >> return AndBinOp       ) AssocLeft ]
    , [ Infix  ( reservedOp "||"  >> return OrBinOp        ) AssocLeft ] ]
  
-- :: Arithmetic term :: --
arithTerm :: Parser Expr
arithTerm = parens arithExpr
         <|> 

-- :: Boolean term :: --
boolTerm = undefined




pExpr :: Parser Expr
pExpr = fail "Implement!"


waccParser :: Parser Program
waccParser = whiteSpace >> pProgram


pProgram :: Parser Program -- 'begin' <func>* <stat> 'end'
pProgram = do 
  reserved "begin"
  funcs <- many pFunc
  stat  <- pStat
  reserved "end"
  return $ Program funcs stat


pFunc :: Parser Func -- <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end' 
pFunc = do 
  typez <- pType   
  ident <- identifier
  pList <- parens pParamList
  reserved "is"
  stat  <- pStatement
  reserved "end"
  return $ Func typez ident pList stat


pType :: Parser Type
pType = fail "Implement!"


pParamList :: Parser ParamList
pParamList = fail "Implement!"


pStat :: Parser Stat
pStat = fail "Implement!"


pStatement :: Parser Stat
pStatement = fail "Implement!"




-- To parse values of type `a` we need a `Parser a`. 
charParser :: Parser Char
-- Parsec comes with `anyChar` that parses any character...
charParser = anyChar


regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


--This function will run the parser, but additionally fail if it doesn't
--consume all the input.
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse ( p <* eof ) ""

--This function will apply the parser, then also return any left over
--input which wasn't parsed.
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ( (,) <$> p <*> leftOver ) ""
  where leftOver = manyTill anyToken eof


{-

type Parser = Parsec String ()

type Parsec s u = ParsecT s u Identity


`ParsecT s u m a` is a parser with stream type `s`, user state type `u`,
underlying monad `m` and return type `a`.


-- The `char` parser parses a specific character which you supply:
char     :: Char -> Parser Char

upper    :: Parser Char
lower    :: Parser Char
alphaNum :: Parser Char
letter   :: Parser Char
digit    :: Parser Char
hexDigit :: Parser Char
octDigit :: Parser Char

oneOf    :: [Char] -> Parser Char
noneOf   :: [Char] -> Parser Char

spaces :: Parser ()

space   :: Parser Char
newline :: Parser Char
tab     :: Parser Char


satisfy :: (Char -> Bool) -> Parser Char


many1 :: Parser a -> Parser [a]
It applies the parser given one or more times, returning the result.

many parses zero or more items rather than one or more
-}

-- Parsec has `regularParse`, which take a Parser, a String to parse and
-- either returns what it parsed or an error if it didn't match the stuff to parse
--myParser :: Parser a -> String -> Either ParseError a
--myParser = regularParse

-- To parse an integer we need to parse the digits and the read the result as an int

integerParser :: Parser Integer
integerParser = do 
    n <- many1 digit
    return ( read n )

integerParser' :: Parser Integer
integerParser' = many1 digit >>= \n -> return $ read n 


variableNameParser :: Parser String
variableNameParser = do
    first <- firstCharParser
    rest  <- many restOfNameParser
    return ( first:rest )  
  where
    firstCharParser :: Parser Char
    firstCharParser = satisfy ( \c -> c == '_' || isLetter c )
    restOfNameParser :: Parser Char 
    restOfNameParser = satisfy ( \c -> isDigit c || isLetter c || c == '_' )
   


