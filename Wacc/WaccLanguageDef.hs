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

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language ( emptyDef , haskellStyle )
import qualified Text.ParserCombinators.Parsec.Token as Token


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

-- TODO write type signature
languageDef
  = emptyDef
  { Token.commentLine     = "#"
  , Token.nestedComments  = False
  , Token.identStart      = letter   <|> char '_'
  , Token.identLetter     = alphaNum <|> char '_'
  , Token.reservedNames   = reservedWords
  , Token.reservedOpNames = reservedOps
  --, Token.opStart         = Token.opStart  haskellStyle
  --, Token.opLetter        = Token.opLetter haskellStyle
  , Token.caseSensitive   = True }

-- TODO write type signature
lexer = Token.makeTokenParser languageDef


-- TODO write type signatures
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

-- TODO write type signature
waccOperators
  = [ [ Prefix ( waccOperators' "!"    $ UnaryOperExpr  NotUnOp  )           ]
    , [ Prefix ( waccOperators' "len " $ UnaryOperExpr  LenUnOp  )           ]
    , [ Prefix ( waccOperators' "ord " $ UnaryOperExpr  OrdUnOp  )           ]
    , [ Prefix ( waccOperators' "chr " $ UnaryOperExpr  ChrUnOp  )           ]
    , [ Prefix ( waccOperators' "-"    $ UnaryOperExpr  NegUnOp  )           ]
    , [ Infix  ( waccOperators' "+"    $ BinaryOperExpr AddBinOp ) AssocLeft ]
    , [ Infix  ( waccOperators' "-"    $ BinaryOperExpr SubBinOp ) AssocLeft ]
    , [ Infix  ( waccOperators' "*"    $ BinaryOperExpr MulBinOp ) AssocLeft ]
    , [ Infix  ( waccOperators' "/"    $ BinaryOperExpr DivBinOp ) AssocLeft ]
    , [ Infix  ( waccOperators' "%"    $ BinaryOperExpr ModBinOp ) AssocLeft ]
    , [ Infix  ( waccOperators' "<"    $ BinaryOperExpr LsBinOp  ) AssocLeft ]
    , [ Infix  ( waccOperators' ">"    $ BinaryOperExpr GtBinOp  ) AssocLeft ]
    , [ Infix  ( waccOperators' "<="   $ BinaryOperExpr LEBinOp  ) AssocLeft ]
    , [ Infix  ( waccOperators' ">="   $ BinaryOperExpr GEBinOp  ) AssocLeft ]
    , [ Infix  ( waccOperators' "=="   $ BinaryOperExpr EqBinOp  ) AssocLeft ]
    , [ Infix  ( waccOperators' "!="   $ BinaryOperExpr NEBinOp  ) AssocLeft ]
    , [ Infix  ( waccOperators' "&&"   $ BinaryOperExpr AndBinOp ) AssocLeft ]
    , [ Infix  ( waccOperators' "||"   $ BinaryOperExpr OrrBinOp ) AssocLeft ] ]

    where

        waccOperators' op expr = waccReservedOp op >> return ( expr )


  
