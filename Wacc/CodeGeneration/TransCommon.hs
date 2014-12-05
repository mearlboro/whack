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

-- | Does the statement introduce variables in its scope?
getBytesNeeded                            :: Stat -> Int -- In Bytes
getBytesNeeded (SeqStat s s'           )  =  getBytesNeeded s + getBytesNeeded s'                   
getBytesNeeded (DeclareStat vtype _ _ _)  =  sizeOf vtype
getBytesNeeded _                          =  0 

-- | Does the statement introduce variables in its scope?
getBytesNeeded'    :: Stat -> Int -- In Bytes
getBytesNeeded' s  =  snd $ getBytesNeeded'' s (False, 0)
  where
    getBytesNeeded'' (SeqStat s s') (stop, acc)  =  if stop' then (undefined, acc') else (stop'', acc'')
        where
          (stop' , acc' ) = getBytesNeeded'' s  (stop, acc) 
          (stop'', acc'') = getBytesNeeded'' s' (stop', acc')

    getBytesNeeded'' (DeclareStat vtype _ _ _) (_, acc)  =  (False, acc + sizeOf vtype)
    getBytesNeeded'' (ScopedStat  _          ) (_, acc)  =  (True, acc)    
    getBytesNeeded'' (WhileStat   _ _ _      ) (_, acc)  =  (True, acc)        
    getBytesNeeded'' (IfStat      _ _ _ _    ) (_, acc)  =  (True, acc)       
    getBytesNeeded'' (_                      ) (_, acc)  =  (False, acc)             


sizeOfExpr e it = sizeOf (typeOfExpr e it)

sizeOf                :: Type -> Int  -- In bytes 
sizeOf IntType        =  4                                 
sizeOf BoolType       =  1                             
sizeOf CharType       =  1                             
sizeOf StringType     =  4 -- Addresss                              
sizeOf (PairType  _)  =  4 -- Address   
sizeOf (ArrayType _)  =  4 -- Address                         
sizeOf NullType       =  4 -- ?                                
sizeOf EmptyType      =  0 -- ?


typeOfExpr                                        :: Expr -> It -> Type    
typeOfExpr ( BoolLiterExpr     _            ) _   =  BoolType    
typeOfExpr ( CharLiterExpr     _            ) _   =  CharType      
typeOfExpr ( IdentExpr         id           ) it  =  fromJust $ findType' id it
typeOfExpr ( UnaryOperExpr     NotUnOp  _   ) _   =  BoolType
typeOfExpr ( UnaryOperExpr     LenUnOp  _   ) _   =  IntType
typeOfExpr ( UnaryOperExpr     OrdUnOp  _   ) _   =  IntType
typeOfExpr ( UnaryOperExpr     ChrUnOp  _   ) _   =  CharType
typeOfExpr ( UnaryOperExpr     NegUnOp  _   ) _   =  IntType
typeOfExpr ( ParenthesisedExpr e            ) it  =  typeOfExpr e it        
typeOfExpr ( IntLiterExpr      _            ) _   =  IntType   
typeOfExpr ( StrLiterExpr      _            ) _   =  StringType
typeOfExpr ( PairLiterExpr                  ) _   =  NullType       
typeOfExpr ( ArrayElemExpr     arrelem      ) it  =  typeOfArrElem arrelem it     
typeOfExpr ( BinaryOperExpr    AddBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    SubBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    MulBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    DivBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    ModBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    AndBinOp _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    OrrBinOp _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    LsBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    GtBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    LEBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    GEBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    EqBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    NEBinOp  _ _ ) _   =  BoolType   

typeOfArrElem _ _ = EmptyType -- TODO

-- Get the type of a pair element. We try to check the containing expression
-- and it must be of type PairType. If it fails there's no type to return.
-- Otherwise we can safely pattermatch on PairType and retrieve the pair
-- element type
getPairElemType           :: PairElem -> It -> Maybe Type
getPairElemType pElem it  =
  case pElem of

    Fst expr -> getPairElemType' expr True  
    Snd expr -> getPairElemType' expr False

  where

    getPairElemType' expr first = 
      case getPairElemType'' expr of
        Just (PairType (Just (ftype, stype))) -> Just $ if first then ftype else stype
        _                                     -> Nothing

    getPairElemType'' (IdentExpr       ident  ) = findType' ident it  
    getPairElemType'' (ArrayElemExpr   arrelem) = getArrayElemType arrelem it 
    getPairElemType''                        _  = Nothing 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Get the type of an array element.
getArrayElemType                               :: ArrayElem -> It -> Maybe Type
getArrayElemType ( ident , exprs ) it  =
    if isValidArray then arrElemType else Nothing
  where
    arrayObj                  =  findIdent' ident it
    IdentObj arrayType arrayCtx    =  fromJust arrayObj
    isValidArray              =  isJust arrayObj            &&
                                 arrayType ~== ArrayType {} &&
                                 arrayCtx ~/= Function {}
    arrElemType               =  Just $
                                   if   arrayType == StringType
                                   then CharType
                                   else deepen ( length exprs + 1 ) arrayType

    deepen 0   t              =  t
    deepen n ( ArrayType t )  =  deepen ( n - 1 ) t
    deepen _   t              =  t


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
    ++ concatMap putDataLabel ( reverse ( dataLabels s ) )
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


boolPrintPredef dataLabel2 dataLabel1 
  = [ PredefLabel name instrs ]
    where
      name   =  "p_print_bool"                         
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ CMP R0 $ Op2'ImmVal 0 ]
             ++ [ LDRNE'Lbl R0 dataLabel1 ]
             ++ [ LDRNQ'Lbl R0 dataLabel2 ]
             ++ [ ADD R0 R0 $ Op2'ImmVal 4  ]
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


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the predefLabels for read functions.
intReadPredef dataLabel
  = [ PredefLabel name instrs ]
    where 
      name   = "p_read_int"
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ MOV'Reg R1 R0 ] 
             ++ [ LDR'Lbl R0 dataLabel ]
             ++ [ ADD R0  R0 $ Op2'ImmVal 4 ]
             ++ [ BL $ JumpLabel "scanf" ]
             ++ [ POP [ PC ] ] )


charReadPredef dataLabel
  = [ PredefLabel name instrs ]
    where 
      name   = "p_read_char"
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ MOV'Reg R1 R0 ] 
             ++ [ LDR'Lbl R0 dataLabel ]
             ++ [ ADD R0  R0 $ Op2'ImmVal 4 ]
             ++ [ BL $ JumpLabel "scanf" ]
             ++ [ POP [ PC ] ] )


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the predefLabels for free functions.

-- Free array
freeArrErrPredef ls
  = (freeLbl, [ PredefLabel name instrs ])
    where
      freeLbl = newDataLabel (  "NullReferenceError: dereference a null " 
                             ++ "reference."                            )
                             ls
      name    = "p_free_array"
      instrs  =  ( [ DEFINE $ JumpLabel name ]
              ++ [ PUSH [ LR ] ]
              ++ [ CMP R0 $ Op2'ImmVal 0 ]
              ++ [ LDREQ'Lbl R0 freeLbl ]
              ++ [ BEQ $ JumpLabel "p_throw_runtime_error" ]
              ++ [ BL  $ JumpLabel "free" ]
              ++ [ POP [ PC ] ] )

-- Free pair 
freePairErrPredef ls
  = (freeLbl, [ PredefLabel name instrs ])
    where
      freeLbl = newDataLabel (  "NullReferenceError: dereference a null "
                             ++ "reference."                            )
                             ls
      name    = "p_free_pair"
      instrs  =  ( [ DEFINE $ JumpLabel name ]
              ++ [ PUSH [ LR ] ]
              ++ [ CMP R0 $ Op2'ImmVal 0 ]
              ++ [ LDREQ'Lbl R0 freeLbl ]
              ++ [ BEQ $ JumpLabel "p_throw_runtime_error" ]
              ++ [ PUSH [ R0 ] ]
              ++ [ LDR'Reg R0 R0 ]
              ++ [ BL  $ JumpLabel "free" ]
              ++ [ LDR'Reg R0 R0 ]
              ++ [ LDR'Reg R0 R0 ] -- TODO: [r0, #4] ??
              ++ [ BL  $ JumpLabel "free" ]
              ++ [ POP [ R0 ] ] 
              ++ [ BL  $ JumpLabel "free" ]
              ++ [ POP [ PC ] ] )


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the so-called predefLabels for errors. These funcs
--   will print an error message and exit the program if necessary.

-- Integer overflow error, generates function label and data label for the message
ovfErrPredef ls
  = (ovfLbl, [ PredefLabel name instrs ])
    where
      -- Creates a data label for printing an overflow error -- TODO \n \0 label issue
      ovfLbl =  newDataLabel (  "OverflowError: the result is too small/large "
                             ++ "to store in a 4-byte signed-integer."        )
                             ls
      name   =  "p_throw_overflow_error"
      -- The set of instructions calls runtime error which exits the program
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ LDR'Lbl R0 ovfLbl ]
             ++ [ BL ( JumpLabel "p_throw_runtime_error" ) ] )


-- Divide by zero check, generates function label and data label for the message
divZeroCheckPredef ls
  = (divLbl, [ PredefLabel name instrs ])
    where
      -- Creates a data label for printing an overflow error -- TODO \n \0 label issue
      divLbl =  newDataLabel "DivideByZeroError: divide or modulo by zero."
                             ls
      name   = "p_check_divide_by_zero"
      -- The set of instructions calls runtime error which exits the program
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ CMP R1 $ Op2'ImmVal 0 ]
             ++ [ LDREQ'Lbl R0 divLbl ] 
             ++ [ BLEQ $ JumpLabel "p_throw_runtime_error" ] 
             ++ [ POP [ PC ] ] )


-- Array bounds check, generates function label and data label for the message
arrBoundsCheckPredef ls
  = (negIndLbl:[outIndLbl], [ PredefLabel name instrs ])
    where
      -- Creates a data label for printing an overflow error -- TODO \n \0 label issue
      negIndLbl =  newDataLabel "ArrayIndexOutOfBoundsError: negative index"
                                ls
      outIndLbl =  newDataLabel "ArrayIndexOutOfBoundsError: index too large"
                                (negIndLbl:ls)

      name   = "p_check_array_bounds"
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

nullPtrCheckPredef ls
  = (nullLbl, [ PredefLabel name instrs ]) 
    where
      nullLbl = newDataLabel (  "NullReferenceError: dereference a null "
                             ++ "reference."                            )
                             ls
      name    = "p_check_null_pointer"
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
      name   = "p_throw_runtime_error"
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
  = or . map (==name) $ map labelName ls


