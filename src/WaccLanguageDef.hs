module WaccLanguageDef 
( waccIdentifier
, waccReserved  
, waccReservedOp
, waccParens    
, waccInteger   
, waccSemi      
, waccWhiteSpace
, waccOperators
) where

import WaccDataTypes 

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language ( emptyDef )
import qualified Text.ParserCombinators.Parsec.Token as Token


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: WACC Language Definition :::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

reservedWords :: [ String ]
reservedWords 
  = [ "begin", "end", "is", "skip", "read", "free", "return", "exit", "print"
    , "println", "if", "then", "else", "fi", "while", "do", "done", "call"
    , "newpair", "fst", "snd", "int", "bool", "char", "string", "pair", "len"
    , "ord", "chr", "null", "true", "false" ]

reservedOps :: [ String ]
reservedOps
  = [ "+",  "-", "*",  "/",  "=",  "=="
    , "!=", "<", ">", "<=", ">=", "&&"
    , "||", "%", "len", "ord", "chr", "!"] 

-- TODO write type signature
languageDef 
  = emptyDef 
  { Token.commentLine     = "#"
  , Token.identStart      = letter   <|> char '_'
  , Token.identLetter     = alphaNum <|> char '_'
  , Token.reservedNames   = reservedWords
  , Token.reservedOpNames = reservedOps }

-- TODO write type signature
lexer = Token.makeTokenParser languageDef 


-- TODO write type signatures
waccIdentifier = Token.identifier lexer -- parses an identifier 
waccReserved   = Token.reserved   lexer -- parses a reserved name
waccReservedOp = Token.reservedOp lexer -- parses an operator
waccParens     = Token.parens     lexer -- parses surrounding parenthesis: parens p takes care of the parenthesis and uses p to parse what's inside them
waccInteger    = Token.integer    lexer -- parses an integer
waccSemi       = Token.semi       lexer -- parses a semicolon
waccWhiteSpace = Token.whiteSpace lexer -- parses whitespace
    
     
-- TODO write type signature
waccOperators 
  = [ [ Prefix ( waccOperators' "!"   $ UnaryOperExpr  NotUnOp        )           ] 
    , [ Prefix ( waccOperators' "len" $ UnaryOperExpr  LenUnOp        )           ]
    , [ Prefix ( waccOperators' "ord" $ UnaryOperExpr  OrdUnOp        )           ]
    , [ Prefix ( waccOperators' "chr" $ UnaryOperExpr  ChrUnOp        )           ]
    , [ Prefix ( waccOperators' "-"   $ UnaryOperExpr  NegUnOp        )           ]
    , [ Infix  ( waccOperators' "+"   $ BinaryOperExpr AddBinOp       ) AssocLeft ] 
    , [ Infix  ( waccOperators' "-"   $ BinaryOperExpr SubBinOp       ) AssocLeft ]
    , [ Infix  ( waccOperators' "*"   $ BinaryOperExpr MulBinOp       ) AssocLeft ] 
    , [ Infix  ( waccOperators' "/"   $ BinaryOperExpr DivBinOp       ) AssocLeft ] 
    , [ Infix  ( waccOperators' "%"   $ BinaryOperExpr ModBinOp       ) AssocLeft ] 
    , [ Infix  ( waccOperators' "<"   $ BinaryOperExpr LsBinOp        ) AssocLeft ]
    , [ Infix  ( waccOperators' ">"   $ BinaryOperExpr GtBinOp        ) AssocLeft ] 
    , [ Infix  ( waccOperators' "<="  $ BinaryOperExpr LEBinOp        ) AssocLeft ] 
    , [ Infix  ( waccOperators' ">="  $ BinaryOperExpr GEBinOp        ) AssocLeft ]
    , [ Infix  ( waccOperators' "=="  $ BinaryOperExpr EqBinOp        ) AssocLeft ]
    , [ Infix  ( waccOperators' "!="  $ BinaryOperExpr NEBinOp        ) AssocLeft ]
    , [ Infix  ( waccOperators' "&&"  $ BinaryOperExpr AndBinOp       ) AssocLeft ]
    , [ Infix  ( waccOperators' "||"  $ BinaryOperExpr OrrBinOp       ) AssocLeft ] ]

waccOperators' op expr = waccReservedOp op >> return ( expr )
  