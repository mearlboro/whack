-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 2. WACC Language Definition ::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

module Wacc.WaccLanguageDef
( waccIdentifier
, waccReserved
, waccReservedOp
, waccCharLiter
, waccStrLiter
, waccInteger
, waccNatural
, waccWhiteSpace
, waccParens
, waccBrackets
, waccSemicolon
, waccComma
, waccColon
, waccSemiSep
, waccSemiSep1
, waccCommaSep
, waccCommaSep1
, waccOperators
, waccLexeme
) where

import Wacc.WaccDataTypes

import Control.Monad.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language ( emptyDef , haskellStyle )
import qualified Text.ParserCombinators.Parsec.Token as Token

--------------------------------------------------------------------------------
reservedWords :: [ String ]
reservedWords
  = [  "begin",     "end",   "is", "skip", "read", "free", "return",  "exit"
    ,  "print", "println",   "if", "then", "else",   "fi",  "while",    "do"
    ,   "done", "newpair", "call",  "fst",  "snd",  "int",   "bool",  "char"
    , "string",    "pair",  "len",  "ord",  "chr", "null",   "true", "false" ]


reservedOps :: [ String ]
reservedOps
  = [ "+" , "-",   "*",   "/",   "=", "=="
    , "!=", "<",   ">",  "<=",  ">=", "&&"
    , "||", "%", "len", "ord", "chr",  "!" ]


languageDef :: Token.LanguageDef sf
languageDef
  = emptyDef
  { Token.commentLine     = "#"
  , Token.nestedComments  = False
  , Token.identStart      = letter   <|> char '_'
  , Token.identLetter     = alphaNum <|> char '_'
  , Token.reservedNames   = reservedWords
  , Token.reservedOpNames = reservedOps
  , Token.caseSensitive   = True }


lexer :: Token.GenTokenParser String st Identity
lexer = Token.makeTokenParser languageDef


waccIdentifier = Token.identifier    lexer
waccReserved   = Token.reserved      lexer
waccReservedOp = Token.reservedOp    lexer
waccCharLiter  = Token.charLiteral   lexer
waccStrLiter   = Token.stringLiteral lexer
waccInteger    = Token.integer       lexer
waccNatural    = Token.natural       lexer
waccWhiteSpace = Token.whiteSpace    lexer
waccParens     = Token.parens        lexer
waccBrackets   = Token.brackets      lexer
waccSemicolon  = Token.semi          lexer
waccComma      = Token.comma         lexer
waccColon      = Token.colon         lexer
waccSemiSep    = Token.semiSep       lexer
waccSemiSep1   = Token.semiSep1      lexer
waccCommaSep   = Token.commaSep      lexer
waccCommaSep1  = Token.commaSep1     lexer
waccLexeme     = Token.lexeme        lexer


waccOperators :: [[Operator Char st Expr]]
waccOperators
  -- |Unary operators have the same precedence, which is 0.
 = [ [ Prefix ( waccOperators' "!"   $ UnaryOperExpr  NotUnOp  )
   ,   Prefix ( waccOperators' "len" $ UnaryOperExpr  LenUnOp  )
   ,   Prefix ( waccOperators' "ord" $ UnaryOperExpr  OrdUnOp  )
   ,   Prefix ( waccOperators' "chr" $ UnaryOperExpr  ChrUnOp  )
   ,   Prefix ( waccOperators' "-"   $ UnaryOperExpr  NegUnOp  )           ]

  -- |Arithmetic operators *, /, % have precedence 1.
   , [ Infix  ( waccOperators' "*"   $ BinaryOperExpr MulBinOp ) AssocLeft
   ,   Infix  ( waccOperators' "/"   $ BinaryOperExpr DivBinOp ) AssocLeft
   ,   Infix  ( waccOperators' "%"   $ BinaryOperExpr ModBinOp ) AssocLeft ]

  -- |Arithmetic operators +, - have precedence 2.
   , [ Infix  ( waccOperators' "+"   $ BinaryOperExpr AddBinOp ) AssocLeft
   ,   Infix  ( waccOperators' "-"   $ BinaryOperExpr SubBinOp ) AssocLeft ]

  -- |Relational operators have precedence 3.
   , [ Infix  ( waccOperators' "<"   $ BinaryOperExpr LsBinOp  ) AssocLeft
   ,   Infix  ( waccOperators' ">"   $ BinaryOperExpr GtBinOp  ) AssocLeft
   ,   Infix  ( waccOperators' "<="  $ BinaryOperExpr LEBinOp  ) AssocLeft
   ,   Infix  ( waccOperators' ">="  $ BinaryOperExpr GEBinOp  ) AssocLeft ]

  -- |Equality and inequality operators have precedence 4.
   , [ Infix  ( waccOperators' "=="  $ BinaryOperExpr EqBinOp  ) AssocLeft
   ,   Infix  ( waccOperators' "!="  $ BinaryOperExpr NEBinOp  ) AssocLeft ]

  -- |Logical and operator has precedence 5.
   , [ Infix  ( waccOperators' "&&"  $ BinaryOperExpr AndBinOp ) AssocLeft ]

  -- |Logical or operator has precedence 6.
   , [ Infix  ( waccOperators' "||"  $ BinaryOperExpr OrrBinOp ) AssocLeft ] ]

    where

        waccOperators' op expr = waccReservedOp op >> return ( expr )

