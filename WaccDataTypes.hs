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





