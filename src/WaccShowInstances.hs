module WaccShowInstances where

import WaccDataTypes

import Data.List  ( intersperse )
import Data.Maybe ( fromJust    )
import Data.Map   ( toList      )


showMany       :: Show a => [ a ] -> [ Char ] -> [ Char ]
showMany xs i  =  concat . intersperse i . map show $ xs


instance Show Program where
  show ( Program funcs main ) = 
    "begin\n\n"                     ++ 
      showMany funcs "\n\n" ++ "\n\n" ++ 
      show' "" main             ++ 
    "\n\nend"


instance Show Func where
  show ( Func ftype name plist body it ) = 
    show ftype ++ " " ++ name ++ "(" ++ showMany plist "," ++ ") is\t\t" ++ 
      showTable it ++ "\n" ++ show' "\t" body ++ "\nend" 


instance Show Param where
  show ( Param t i ) = show t ++ " " ++ i

instance Show Context where
  show Variable       = "•"
  show ( Function _ ) = "ƒ"
  show Parameter      = "¶"

show' :: [ Char ] -> Stat -> [ Char ]
show' indent stat = case stat of 
  SkipStat            -> indent ++ "skip"                            
  FreeStat    e it      -> indent ++ "free "     ++ show e ++ "\t" ++ showTable it                       
  ReturnStat  e it      -> indent ++ "return "   ++ show e ++ "\t" ++ showTable it                       
  ExitStat    e it      -> indent ++ "exit "     ++ show e ++ "\t" ++ showTable it                 
  PrintStat   e it      -> indent ++ "print "    ++ show e ++ "\t" ++ showTable it                   
  PrintlnStat e it      -> indent ++ "println "  ++ show e ++ "\t" ++ showTable it

  ScopedStat  s       -> indent ++ "begin\n"   ++ 
                            show' ( indent ++ "\t" ) s ++ "\n" ++
                         indent ++ "end"   

  ReadStat    lhs it   -> indent ++ "read " ++ show lhs  ++ "\t" ++ showTable it

  WhileStat   e s it   -> indent ++ "while " ++ show e ++ " do\t" ++ showTable it ++ "\n" ++
                            show' ( indent ++ "\t" ) s ++ "\n" ++
                          indent ++ "done"  

  SeqStat     s s'    -> show' indent s ++ " ;\n" ++ 
                         show' indent s'   

  AssignStat  lhs rhs it -> indent ++ show lhs ++ " = " ++ show rhs ++ "\t" ++ showTable it 

  IfStat      e s s' it  -> indent ++ "if " ++ show e ++ " then\t" ++ showTable it ++ "\n" ++
                            show' ( indent ++ "\t" ) s ++ "\n" ++
                         indent ++ "else\n" ++ 
                            show' ( indent ++ "\t") s' ++ "\n" ++
                         indent ++ "fi"

  DeclareStat t i lhs it -> indent ++ show t ++ " " ++ i ++ " = " ++ show lhs ++ "\t" ++ showTable it
 

instance Show AssignLhs where
  show lhs = case lhs of 
    LhsIdent     ident -> ident    
    LhsPairElem  pelem -> show pelem 
    LhsArrayElem aelem -> show aelem 

instance Show AssignRhs where
  show rhs = case rhs of 
    RhsExpr       e -> show e               
    RhsPairElem   pe -> show pe            
    RhsArrayLiter exprs -> "[" ++ showMany exprs "," ++ "]"      
    RhsNewPair    e e' -> "newpair(" ++ show e ++ ", " ++ show e' ++ ")"    
    RhsCall       i al -> "call " ++ i ++ "(" ++ showMany al "," ++ ")"  
 
instance Show PairElem where
  show pelem = case pelem of 
    Fst e -> "fst " ++ show e
    Snd e -> "snd " ++ show e


instance Show Type where
  show t = case t of 
    IntType                -> "int"                         
    BoolType               -> "bool"                         
    CharType               -> "char"                           
    StringType             -> "string"                        
    PairType Nothing       -> "pair" 
    PairType ( Just ( t , t' ) ) -> "pair(" ++ show t ++ ", " ++ show t' ++ ")"
    ArrayType t            -> show t ++ "[]"                      
    NullType               -> "null"                        
    EmptyType              -> "EMPTY_TYPE"                        

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

instance Show UnaryOper where
  show op = fromJust $ lookup op dict 
    where
      dict = zip [ NotUnOp .. ] $ words "! len ord chr -" 


instance Show BinaryOper where
  show op = fromJust $ lookup op dict 
    where 
      dict = zip [ AddBinOp .. ] $ words "+ - * / % && || < > <= >= == !="


instance Show ArrayElem where 
  show ( ArrayElem ident exprs ) = 
    ident ++ "[" ++ showMany exprs "][" ++ "]"

--instance Show PairLiter where show _ = "null"




showTable table = 
  case table of 
    Empty      -> "ø"
    --ST Empty m -> "(" ++ show' m ++ ")"
    ST st    m -> showTable st ++ "|" ++ show' m 
  where
    show' :: Dictionary -> [ Char ]
    show' m = concat . intersperse " " . map show'' . toList $ m
      where 
        show'' ( name , ( ttype , ctx ) ) =
          show ctx ++ name -- ++ ":" ++ show ttype 




 
