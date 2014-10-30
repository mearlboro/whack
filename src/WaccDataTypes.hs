
type RWord      = [ Char ] -- Reserved word
type RChar      = Char     -- Reserved char
type ValidIdent = [ Char ]
type 

--   <base-type>  ::= 'int'
data BaseType       = BTint | BTbool | BTchar | BTstring

--   <array-type> ::=            <type>  [    ']'
data ArrayType      = ArrayType  Type 

--   <pair-type>  ::=            pair ( pair-elem-type> , <pair-elem-type> )
data PairType       = PairType   PairElemType PairElemType 


data PairElemType   =  BasePairElemType BaseType 
                    | ArrayPairElemType ArrayType
                    |  PairPairElemType

data Expr           =   IntLiterExpr IntLiter
                    |  BoolLiterExpr BoolLiter
                    |  CharLiterExpr CharLiter
                    |   StrLiterExpr StrLiter
                    |      IdentExpr Ident
                    |  ArrayElemExpr ArrayElem
                    |  UnaryOperExpr Expr
                    | BinaryOperExpr Expr Expr

data UnaryOper  = UOnot {- ! -} | UOnegate | UOlen | UOord | UOchr
 
data BinaryOper = BOtimes    | BOdiv | BOmod  | BOadd | BOsub
                | BOgreater  | BOgeq | BOless | BOleq | BOequal
                | BOneq      | BOand | BOor   

data Ident      = Ident ValidIdent

data ArrayElem = ArrayElem Ident Expr

data IntLiter  = SignedIntLiter IntSign [ Digit ] 
               | IntLiter [ Digit ] 



