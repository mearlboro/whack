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

-- ************************************************************************** --

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
    =  show ( INDIR Data )  ++ "\n"
    ++ concatMap putDataLabel ( dataLabels s )
    ++ show ( INDIR Text )  ++ "\n"                  
    ++ show ( INDIR ( Global ( "main" ) ) ) ++ "\n"
    ++ ( concat $ intersperse "\n" $ map show instrs ) ++ "\n"
    ++ concatMap putPredefLabel ( predefLabels s )
      where
        putDataLabel ( DataLabel l str ) 
          =  l
          ++ "\n\t.word " ++ show ( length str + 1 )
          ++ "\n\t.ascii \"" ++ str  ++ "\\0\"\n"

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

intPrintPredef dataLabel 
  = [ PredefLabel name instrs ]
    where
      name   =  "p_print_int"                         
      instrs =  ( [ DEFINE $ JumpLabel name ] 
             ++ [ PUSH [ LR ] ]
             ++ [ MOV'Reg R1 R0 ]
             ++ [ LDR'Lbl R0 dataLabel ]
             ++ [ ADD R0 R0 $ Op2'ImmVal 4 ] 
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )


boolPrintPredef dataLabel1 dataLabel2 
  = [ PredefLabel name instrs ]
    where
      name   =  "p_print_bool"                         
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ CMP R0 $ Op2'ImmVal 0 ]
             ++ [ LDRNE'Lbl R0 dataLabel1 ]
             ++ [ LDRNQ'Lbl R0 dataLabel2 ]
             ++ [ ADD R0 R0 $ Op2'ImmVal 4  ]  -- How do we know it's 4?
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )


strPrintPredef dataLabel
  = [ PredefLabel name instrs ]
    where
      name   = "p_print_string"
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ LDR'Reg R1 R0 ]
             ++ [ ADD R2 R0 $ Op2'ImmVal 4 ]
             ++ [ LDR'Lbl R0 dataLabel ]
             ++ [ ADD R0 R0$ Op2'ImmVal 4 ]
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the so-called predefLabels for errors. These funcs
--   will print an error message and exit the program if necessary.

stateAddIntOverflowError s
  = s { dataLabels = ls', predefLabels = ps' }
    where
      (ls', ps')
        = if not $ containsLabel "p_throw_overflow_error" ps
            then
              let l        = newDataLabel   "%.*s" ls in
              let p        = strPrintPredef l         in
              let (l', p') = ovfErrPredef   (l:ls)    in
              let p''      = runtErrPredef            in
              (l': l: ls, ps ++ p ++ p' ++ p'')
            else
              (ls,   ps)

      ls           = dataLabels     s
      ps           = predefLabels   s


-- Integer overflow error 
ovfErrPredef ls
  = (ovfLbl, [ PredefLabel name instrs ])
    where
      -- Creates a data label for printing an overflow error -- TODO \n \0 label issue
      ovfLbl =  newDataLabel ( "OverflowError: the result is too small/large" ++ 
                               " to store in a 4-byte signed-integer."         )
                ls
      name   =  "p_throw_overflow_error"
      -- The set of instructions calls runtime error which exits the program
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ LDR'Lbl R0 ovfLbl ]
             ++ [ BL ( JumpLabel "p_throw_runtime_error" ) ] )


-- Runtime error
runtErrPredef
  = [ PredefLabel name instrs ]
    where
      name   = "p_throw_runtime_error"
      instrs =  ( [ DEFINE $ JumpLabel name ] 
             ++ [ BL $ JumpLabel "p_print_string" ] 
             ++ [ MOV R0 $ Op2'ImmVal (-1) ]
             ++ [ BL ( JumpLabel "exit" )  ] )

-- | Create a new data label and return the list with the label added.
newDataLabel str ls 
  = DataLabel lName str
    where 
      lName = "msg_" ++ ( show $ length ls )

labelName :: Label -> LabelName
labelName (JumpLabel   name  ) = name
labelName (PredefLabel name _) = name
labelName (DataLabel   name _) = name

-- 
containsLabel name ls
  = or . map (==name) $ map labelName ls


-- TODO move somewhere else
nextLabel :: Int -> Label
nextLabel i = JumpLabel $ "L" ++ show i
 