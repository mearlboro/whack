module Wacc.Data.ShowInstances where

import Wacc.Data.DataTypes

import Data.List  ( intersperse )
import Data.Map   ( toList      )
import Data.Maybe ( fromJust    )


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Grammar Types Show Instances :::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Show program
instance Show Program where
  show ( Program funcs main )
    =  "begin\n\n"
    ++ showMany funcs "\n\n"
    ++ ( if length funcs == 0 then "" else "\n\n" )
    ++ show' "  " main
    ++ "\n\nend"

-- | Show function
instance Show Func where
  show ( Func ftype name plist body it )
    =  "  " ++ show ftype
    ++ " "  ++ name
    ++ "("  ++ showMany plist "," ++ ") is    "
    ++ showTable it      ++ "\n"
    ++ show' "    " body ++ "\n  end"

-- | Show parameters
instance Show Param where
  show ( Param t i ) = show t ++ " " ++ i

-- | Show context
instance Show Context where
  show Variable       = "•"
  show ( Function _ ) = "ƒ"
  show Parameter      = "¶"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Show statement
showMany       :: Show a => [ a ] -> [ Char ] -> [ Char ]
showMany xs i  =  concat . intersperse i . map show $ xs

show' :: [ Char ] -> Stat -> [ Char ]
show' indent stat = case stat of
  SkipStat         -> indent ++ "skip"
  FreeStat    e it -> indent ++ "free "    ++ show e ++ "  " ++ showTable it
  ReturnStat  e it -> indent ++ "return "  ++ show e ++ "  " ++ showTable it
  ExitStat    e it -> indent ++ "exit "    ++ show e ++ "  " ++ showTable it
  PrintStat   e it -> indent ++ "print "   ++ show e ++ "  " ++ showTable it
  PrintlnStat e it -> indent ++ "println " ++ show e ++ "  " ++ showTable it

  ScopedStat  s    -> indent
                   ++ "begin\n" ++ show' ( indent ++ "  " ) s ++ "\n" ++ indent
                   ++ "end"

  ReadStat    lhs it -> indent
                     ++ "read " ++ show lhs ++ "  " ++ showTable it

  WhileStat   e s it -> indent
                     ++ "while " ++ show e
                     ++ " do  "  ++ showTable it   ++ "\n"
                     ++ show' ( indent ++ "  " ) s ++ "\n" ++ indent
                     ++ "done"

  SeqStat     s s'   -> show' indent s ++ " ;\n" ++
                        show' indent s'

  AssignStat  lhs rhs it -> indent
                         ++ show lhs ++ " = "
                         ++ show rhs ++ "  " ++ showTable it

  IfStat      e s s' it  -> indent
                         ++ "if "     ++ show e
                         ++ " then  " ++ showTable it   ++ "\n"
                         ++ show' ( indent ++ "  " ) s  ++ "\n"             ++ indent
                         ++ "else\n"  ++ show' ( indent ++ "  ") s' ++ "\n" ++ indent
                         ++ "fi"

  DeclareStat t i lhs it -> indent ++ show t   ++ " "  ++ i
                         ++ " = "  ++ show lhs ++ "  " ++ showTable it

-- | Assignments
instance Show AssignLhs where
  show lhs = case lhs of
    LhsIdent     ident -> ident
    LhsPairElem  pElem -> show pElem
    LhsArrayElem aElem -> show aElem

instance Show AssignRhs where
  show rhs = case rhs of
    RhsExpr       e     -> show e
    RhsPairElem   pe    -> show pe
    RhsArrayLiter exprs -> "[" ++ showMany exprs "," ++ "]"
    RhsNewPair    e e'  -> "newpair(" ++ show e ++ ", " ++ show e' ++ ")"
    RhsCall       i al  -> "call " ++ i ++ "(" ++ showMany al "," ++ ")"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Types
instance Show Type where
  show t = case t of
    IntType                      -> "int"
    BoolType                     -> "bool"
    CharType                     -> "char"
    StringType                   -> "string"
    PairType Nothing             -> "pair"
    PairType ( Just ( t , t' ) ) -> "pair(" ++ show t ++ ", " ++ show t' ++ ")"
    ArrayType t                  -> show t ++ "[]"
    NullType                     -> "null"
    EmptyType                    -> "EMPTY_TYPE"

-- | Pair elements
instance Show PairElem where
  show pElem = case pElem of
    Fst e -> "fst " ++ show e
    Snd e -> "snd " ++ show e

-- | Array elements
instance Show ArrayElem where
  show ( ArrayElem ident exprs ) =
    ident ++ "[" ++ showMany exprs "][" ++ "]"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Expressions
instance Show Expr where
  show expr = case expr of
    BoolLiterExpr     b       -> show b
    CharLiterExpr     c       -> show c
    IdentExpr         v       -> v
    UnaryOperExpr     op e    -> show op ++ " " ++ show e
    ParenthesisedExpr e       -> "(" ++ show e ++ ")"
    IntLiterExpr      i       -> show i
    StrLiterExpr      s       -> show s
    PairLiterExpr             -> "null"
    ArrayElemExpr     ae      -> show ae
    BinaryOperExpr    op e e' -> show e ++ " " ++ show op ++ " " ++ show e'

-- | Operators
instance Show UnaryOper where
  show op = fromJust $ lookup op dict
    where
      dict = zip [ NotUnOp .. ] $ words "! len ord chr -"

instance Show BinaryOper where
  show op = fromJust $ lookup op dict
    where
      dict = zip [ AddBinOp .. ] $ words "+ - * / % && || < > <= >= == !="




-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
showTable table = ""





