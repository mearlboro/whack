{-# LANGUAGE FlexibleInstances #-}

module Wacc.CodeGeneration.TransCommon where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.Data.DataTypes
import Wacc.Data.SymbolTable (findType', findIdent')

import Data.Maybe (fromJust, isJust)
import Data.List  (intersperse)


-- ************************************************************************** --
-- **************************                         *********************** --
-- **************************        Helpers          *********************** --
-- **************************                         *********************** -- 
-- ************************************************************************** --

strVar :: Rn -> Rd -> Int -> Int -> Instr 
strVar rd rn size off 
  | size == 1 = if off == 0 then STRB'Reg rd rn else STRB'Off rd rn off
  | otherwise = if off == 0 then STR'Reg rd rn else STR'Off rd rn off

ldrVar :: Rn -> Rd -> Int -> Int -> Instr 
ldrVar rd rn size off 
  | size == 1 = if off == 0 then LDRSB'Reg rd rn else LDRSB'Off rd rn off
  | otherwise = if off == 0 then LDR'Reg rd rn else LDR'Off rd rn off

strArg :: Rn -> Rn -> Int -> Int -> Instr 
strArg rd rn size off 
  | size == 1 = STRB'Arg rd rn off 
  | otherwise = STR'Arg rd rn off 


-- ************************************************************************** --
-- **************************                         *********************** --
-- **************************       Common Utils      *********************** --
-- **************************                         *********************** -- 
-- ************************************************************************** --

sizeOf       :: HasType t => t -> It -> Bytes
sizeOf t it  =  case typeOf t it of 
  IntType     -> 4                                 
  BoolType    -> 1                             
  CharType    -> 1                             
  StringType  -> 4                              
  PairType  _ -> 4  
  ArrayType _ -> 4                        
  NullType    -> 4                         
  EmptyType   -> 0

class HasType a where
  typeOf :: a -> It -> Type 

instance HasType Type where
  typeOf = const 

instance HasType AssignRhs where
  typeOf (RhsExpr       e         ) it = typeOf e it             
  typeOf (RhsPairElem   pairE     ) it = typeOf pairE it         
  typeOf (RhsArrayLiter []        ) _  = EmptyType         
  typeOf (RhsArrayLiter elems     ) it = ArrayType $ typeOf (head elems) it         
  typeOf (RhsCall       id    _   ) it = typeOf (IdentExpr id) it        
  typeOf (RhsNewPair    fstE  sndE) it = PairType (Just (fstT, sndT))   
    where
      fstT = typeOf fstE it 
      sndT = typeOf sndE it 

instance HasType AssignLhs where
  typeOf (LhsIdent     id    ) = typeOf (IdentExpr id)  
  typeOf (LhsPairElem  pairE ) = typeOf pairE 
  typeOf (LhsArrayElem arrayE) = typeOf arrayE 

instance HasType Expr where
  typeOf (BoolLiterExpr     _           ) _  = BoolType    
  typeOf (CharLiterExpr     _           ) _  = CharType      
  typeOf (IdentExpr         id          ) it = fromJust $ findType' id it
  typeOf (UnaryOperExpr     NotUnOp  _  ) _  = BoolType
  typeOf (UnaryOperExpr     LenUnOp  _  ) _  = IntType
  typeOf (UnaryOperExpr     OrdUnOp  _  ) _  = IntType
  typeOf (UnaryOperExpr     ChrUnOp  _  ) _  = CharType
  typeOf (UnaryOperExpr     NegUnOp  _  ) _  = IntType
  typeOf (ParenthesisedExpr e           ) it = typeOf e it        
  typeOf (IntLiterExpr      _           ) _  = IntType   
  typeOf (StrLiterExpr      _           ) _  = StringType
  typeOf (PairLiterExpr                 ) _  = NullType       
  typeOf (ArrayElemExpr     arrayE      ) it = typeOf arrayE it 
  typeOf (BinaryOperExpr    AddBinOp _ _) _  = IntType
  typeOf (BinaryOperExpr    SubBinOp _ _) _  = IntType
  typeOf (BinaryOperExpr    MulBinOp _ _) _  = IntType
  typeOf (BinaryOperExpr    DivBinOp _ _) _  = IntType
  typeOf (BinaryOperExpr    ModBinOp _ _) _  = IntType
  typeOf (BinaryOperExpr    AndBinOp _ _) _  = BoolType
  typeOf (BinaryOperExpr    OrrBinOp _ _) _  = BoolType
  typeOf (BinaryOperExpr    LsBinOp  _ _) _  = BoolType
  typeOf (BinaryOperExpr    GtBinOp  _ _) _  = BoolType
  typeOf (BinaryOperExpr    LEBinOp  _ _) _  = BoolType
  typeOf (BinaryOperExpr    GEBinOp  _ _) _  = BoolType
  typeOf (BinaryOperExpr    EqBinOp  _ _) _  = BoolType
  typeOf (BinaryOperExpr    NEBinOp  _ _) _  = BoolType   

instance HasType PairElem where
  typeOf (Fst e) it = let PairType (Just (fstT, _)) = typeOf e it in fstT
  typeOf (Snd e) it = let PairType (Just (_, sndT)) = typeOf e it in sndT

instance HasType ArrayElem where
  typeOf (id, es) it = iterate elemT arrayT !! length es
    where
      arrayT = typeOf (IdentExpr id) it

      elemT (ArrayType t) = t
      elemT            t  = t 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Does the statement introduce variables in its scope?
getBytesNeeded                            :: Stat -> Int -- In Bytes
getBytesNeeded (SeqStat s s'           )  =  getBytesNeeded s + getBytesNeeded s'                   
getBytesNeeded (DeclareStat vtype _ _ _)  =  sizeOf vtype undefined
getBytesNeeded _                          =  0 




-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************       Prettification       *********************** -- 
-- ***********************                            *********************** --
-- ************************************************************************** -- 

-- TODO Do Not Rename 
makePretty :: ( ArmState, [ Instr ] ) -- computed by transProgram
           ->   String                -- printable compiled program
makePretty (s, instrs) 
    =  "\t" ++ show ( INDIR Data )  ++ "\n"
    ++ concatMap putDataLabel ( reverse ( dataLabels s ) )
    ++ "\t" ++ show ( INDIR Text )  ++ "\n"                  
    ++ "\t" ++ show ( INDIR ( Global ( "main" ) ) ) ++ "\n"
    ++ ( concat $ intersperse "\n\t" $ map show instrs ) ++ "\n"
    ++ concatMap putPredefLabel ( predefLabels s )
      where
        putDataLabel ( DataLabel l str ) 
          =  "\t" ++ l ++ ":"
          ++ "\n\t\t.word "    ++ show length' 
          ++ "\n\t\t.ascii \"" ++ str  ++ "\"\n"
            where
                length' = length str - ((length . filter (\x -> x == '\\')) str)

        putPredefLabel ( PredefLabel l instrs )
          = ( concat $ intersperse "\n\t" $ map show instrs ) ++ "\n"


-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************           Labels           *********************** -- 
-- ***********************                            *********************** --
-- ************************************************************************** -- 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the so-called predefLabels for print. They consist
--   of lists of instructions generated similarly to main and the other functions,
--   but are defined as a sub-type of Label for logic convenience, and for easing 
--   up their use in a BL instruction.

intPrintPredef ls 
  = ([ intLbl ], [ PredefLabel name instrs ])
    where
      intLbl = newDataLabel "%d\\0" ls

      name   =  "p_print_int:"                         
      instrs =  ( [ DEFINE $ JumpLabel name ] 
             ++ [ PUSH [ LR ] ]
             ++ [ MOV'Reg R1 R0 ]
             ++ [ LDR'Lbl R0 intLbl ]
             ++ [ ADD R0 R0 $ Op2'ImmVal 4 ] 
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )


boolPrintPredef ls 
  = ([ trueLbl, falseLbl ], [ PredefLabel name instrs ])
    where
      falseLbl = newDataLabel "false\\0" ls
      trueLbl  = newDataLabel "true\\0"  (falseLbl:ls)

      name   =  "p_print_bool:"                         
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ CMP R0 $ Op2'ImmVal 0 ]
             ++ [ LDRNE'Lbl R0 trueLbl  ]
             ++ [ LDREQ'Lbl R0 falseLbl ]
             ++ [ ADD R0 R0 $ Op2'ImmVal 4  ]
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )

strPrintPredef ls
  = ([ strLbl ], [ PredefLabel name instrs ])
    where
      strLbl = newDataLabel "%.*s\\0" ls

      name   = "p_print_string:"
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ LDR'Reg R1 R0 ]
             ++ [ ADD R2 R0 $ Op2'ImmVal 4 ]
             ++ [ LDR'Lbl R0 strLbl ]
             ++ [ ADD R0 R0$ Op2'ImmVal 4 ]
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )

refPrintPredef ls
  = ([ refLbl ], [ PredefLabel name instrs ])
    where
        refLbl = newDataLabel "%p\\0" ls
        
        name   = "p_print_reference:"
        instrs =  ( [ DEFINE $ JumpLabel name ]
               ++ [ PUSH [ LR ] ]
               ++ [ MOV'Reg R1 R0 ]
               ++ [ LDR'Lbl R0 refLbl ]  
               ++ [ ADD R0 R0 $ Op2'ImmVal 4 ]
               ++ [ BL $ JumpLabel "printf" ]
               ++ [ MOV'Reg R0 R0 ]
               ++ [ BL $ JumpLabel "fflush" ]
               ++ [ POP [ PC ] ] )


printlnPredef ls
  = ([ printlnLbl ], [ PredefLabel name instrs ])
    where
      printlnLbl = newDataLabel "\\0" ls 

      name   = "p_print_ln:"
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ LDR'Lbl R0 printlnLbl ]
             ++ [ ADD R0 R0 $ Op2'ImmVal 4 ]
             ++ [ BL $ JumpLabel "puts" ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL $ JumpLabel "fflush" ]
             ++ [ POP [ PC ] ] )
 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the predefLabels for read functions.
intReadPredef ls
  = ([ intLbl ], [ PredefLabel name instrs ])
    where 
      intLbl = newDataLabel "%d\\0" ls
    
      name   = "p_read_int:"
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ MOV'Reg R1 R0 ] 
             ++ [ LDR'Lbl R0 intLbl ]
             ++ [ ADD R0  R0 $ Op2'ImmVal 4 ]
             ++ [ BL $ JumpLabel "scanf" ]
             ++ [ POP [ PC ] ] )


charReadPredef ls
  = ([ charLbl ], [ PredefLabel name instrs ])
    where 
      charLbl = newDataLabel "%c\\0" ls

      name   = "p_read_char:"
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ MOV'Reg R1 R0 ] 
             ++ [ LDR'Lbl R0 charLbl ]
             ++ [ ADD R0  R0 $ Op2'ImmVal 4 ]
             ++ [ BL $ JumpLabel "scanf" ]
             ++ [ POP [ PC ] ] )


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the predefLabels for free functions.

-- Free array
freeArrPredef ls
  = ([ freeLbl ], [ PredefLabel name instrs ])
    where
      freeLbl = newDataLabel (  "NullReferenceError: dereference a null " 
                             ++ "reference.\\0\n"                            )
                             ls
      name    = "p_free_array:"
      instrs  =  ( [ DEFINE $ JumpLabel name ]
              ++ [ PUSH [ LR ] ]
              ++ [ CMP R0 $ Op2'ImmVal 0 ]
              ++ [ LDREQ'Lbl R0 freeLbl ]
              ++ [ BEQ $ JumpLabel "p_throw_runtime_error" ]
              ++ [ BL  $ JumpLabel "free" ]
              ++ [ POP [ PC ] ] )

-- Free pair 
freePairPredef ls
  = ([ freeLbl ], [ PredefLabel name instrs ])
    where
      freeLbl = newDataLabel (  "NullReferenceError: dereference a null "
                             ++ "reference.\\n"                          )
                             ls
      name    = "p_free_pair:"
      instrs  =  ( [ DEFINE $ JumpLabel name ]
              ++ [ PUSH [ LR ] ]
              ++ [ CMP R0 $ Op2'ImmVal 0 ]
              ++ [ LDREQ'Lbl R0 freeLbl ]
              ++ [ BEQ $ JumpLabel "p_throw_runtime_error" ]
              ++ [ PUSH [ R0 ] ]
              ++ [ LDR'Reg R0 R0 ]
              ++ [ BL  $ JumpLabel "free" ]
              ++ [ LDR'Reg R0 R0 ]
              ++ [ ldrVar R0 R0 4 4]
              ++ [ BL  $ JumpLabel "free" ]
              ++ [ POP [ R0 ] ] 
              ++ [ BL  $ JumpLabel "free" ]
              ++ [ POP [ PC ] ] )


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the so-called predefLabels for errors. These funcs
--   will print an error message and exit the program if necessary.

-- Integer overflow error, generates function label and data label for the message
ovfErrPredef ls
  = ([ ovfLbl ], [ PredefLabel name instrs ])
    where
      -- Creates a data label for printing an overflow error -- TODO \n \0 label issue
      ovfLbl =  newDataLabel (  "OverflowError: the result is too small/large "
                             ++ "to store in a 4-byte signed-integer.\\n\\0"  )
                             ls
      name   =  "p_throw_overflow_error:"
      -- The set of instructions calls runtime error which exits the program
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ LDR'Lbl R0 ovfLbl ]
             ++ [ BL ( JumpLabel "p_throw_runtime_error" ) ] )


-- Divide by zero check, generates function label and data label for the message
divZeroCheckPredef ls
  = ([ divLbl ], [ PredefLabel name instrs ])
    where
      -- Creates a data label for printing an overflow error -- TODO \n \0 label issue
      divLbl =  newDataLabel "DivideByZeroError: divide or modulo by zero.\\n\\0"
                             ls
      name   = "p_check_divide_by_zero:"
      -- The set of instructions calls runtime error which exits the program
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ CMP R1 $ Op2'ImmVal 0 ]
             ++ [ LDREQ'Lbl R0 divLbl ] 
             ++ [ BLEQ $ JumpLabel "p_throw_runtime_error" ] 
             ++ [ POP [ PC ] ] )


-- Array bounds check, generates function label and data label
arrBoundsCheckPredef ls
  = ([ outIndLbl, negIndLbl ], [ PredefLabel name instrs ])
    where
      -- Creates a data label for printing an overflow error -- TODO \n \0 label issue
      negIndLbl =  newDataLabel "ArrayIndexOutOfBoundsError: negative index\\n\\0"
                                ls
      outIndLbl =  newDataLabel "ArrayIndexOutOfBoundsError: index too large\\n\\0"
                                (negIndLbl:ls)

      name   = "p_check_array_bounds:"
      -- The set of instructions calls runtime error which exits the program
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ CMP R0 $ Op2'ImmVal 0 ]
             ++ [ LDRLT'Lbl R0 negIndLbl ]
             ++ [ BLLT $ JumpLabel "p_throw_runtime_error" ]
             ++ [ LDR'Reg R1 R1 ]
             ++ [ LDRCS'Lbl R0 outIndLbl ]
             ++ [ BLCS $ JumpLabel "p_throw_runtime_error" ] 
             ++ [ POP [ PC ] ] )

-- Null pointer check, generates function and data label
nullPtrCheckPredef ls
  = ([ nullLbl ], [ PredefLabel name instrs ]) 
    where
      nullLbl = newDataLabel (  "NullReferenceError: dereference a null "
                             ++ "reference.\\n\\0"                      )
                             ls
      name    = "p_check_null_pointer:"
      instrs  =  ( [ DEFINE $ JumpLabel name ] 
              ++ [ PUSH [ LR ] ]
              ++ [ CMP R0 $ Op2'ImmVal 0 ]
              ++ [ LDREQ'Lbl R0 nullLbl ]
              ++ [ BLEQ $ JumpLabel "p_throw_runtime_error" ] 
              ++ [ POP [ PC ] ] )

-- Runtime error
runtErrPredef
  = [ PredefLabel name instrs ]
    where
      name   = "p_throw_runtime_error:"
      instrs =  ( [ DEFINE $ JumpLabel name ] 
             ++ [ BL $ JumpLabel "p_print_string" ] 
             ++ [ MOV R0 $ Op2'ImmVal (-1) ]
             ++ [ BL ( JumpLabel "exit" )  ] )



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Create a new data label with a given string, using the current number of labels 
--   to name it.
newDataLabel str ls 
  = DataLabel lName str
    where 
      lName = "msg_" ++ ( show $ length ls ) 

-- | For naming "runtime" labels
nextLabel :: Int -> Label
nextLabel i = JumpLabel $ "L" ++ show i
 

-- | Gets the label name, to be used for duplicate checks
labelName :: Label -> LabelName
labelName (JumpLabel   name  ) = name
labelName (PredefLabel name _) = name
labelName (DataLabel   name _) = name

-- | Checks if a predef label was added or not in the current program
containsLabel name ls
  = or . map (== name) $ map labelName ls



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Updates the state with the read labels and functions 
stateAddRead s name
  = if not $ containsLabel (name ++ ":") ps 
      then 
        case name of
            "p_read_int"  -> stateAddRead' intReadPredef  
            "p_read_char" -> stateAddRead' charReadPredef
      else s 

    where
      stateAddRead' function = let (l, p) = function ls
                               in s { dataLabels = l ++ ls, predefLabels = ps ++ p } 
                            
      ls           = dataLabels     s
      ps           = predefLabels   s

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Updates the state with the print labels and functions
stateAddPrint s name
  = if not $ containsLabel (name ++ ":") ps
      then 
        case name of 
          "p_print_int"       -> stateAddPrint' intPrintPredef  
          "p_print_bool"      -> stateAddPrint' boolPrintPredef 
          "p_print_string"    -> stateAddPrint' strPrintPredef  
          "p_print_reference" -> stateAddPrint' refPrintPredef  
          "p_print_ln"        -> stateAddPrint' printlnPredef   
          _                   -> s
      else s 

    where
      stateAddPrint' function = let (l, p) = function ls 
                                in  s { dataLabels = l ++ ls, predefLabels = ps ++ p }

      ls           = dataLabels     s
      ps           = predefLabels   s


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Updates the state with the predefined functions that perform checks, and might
-- throw runtime errors. 
stateAddCheckNullPtr s
  = stateAddError s "p_check_null_pointer:" nullPtrCheckPredef 

stateAddIntOverflowError s 
  = stateAddError s "p_throw_overflow_error:" ovfErrPredef 
        
stateAddDivZeroError s
  = stateAddError s "p_check_divide_by_zero:" divZeroCheckPredef
      
stateAddArrayBounds s
  = stateAddError s "p_check_array_bounds:" arrBoundsCheckPredef 

stateAddFreeArr s
  = stateAddError s "p_free_array:" freeArrPredef

stateAddFreePair s
  = stateAddError s "p_free_pair:" freePairPredef

-- Helper function to add string print and runtime error
stateAddError s name function
  = s { dataLabels = ls', predefLabels = ps' }
    where
      (ls', ps')
        = if not $ containsLabel name ps
            then 
                let (l, p)   = if not $ containsLabel "p_print_string:" ps 
                                 then strPrintPredef ls
                                 else ([], [])          
                in
                let (l', p') = function (l ++ ls)  
                in
                let  p''     = if not $ containsLabel "p_throw_runtime_error:" ps 
                                 then runtErrPredef
                                 else []
                in
                (l' ++ l ++ ls, ps ++ p ++ p' ++ p'')
            else (ls,   ps)

      ls           = dataLabels     s
      ps           = predefLabels   s

