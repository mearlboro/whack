module Wacc.CodeGeneration.WaccCodeGen where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.Data.DataTypes
import Wacc.Data.SymbolTable

import qualified Data.Map as Map
import Data.Array
import Data.List ( intersperse )
import Data.Maybe
import Data.Char
-- import Data.Tuple.Select ( sel3 )

import Debug.Trace

-- ************************************************************************** --
-- **************************                         *********************** --
-- **************************     Code generation     *********************** --
-- **************************                         *********************** -- 
-- ************************************************************************** --


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
transProgram :: Program        -- | The program Augmented AST
             -> ( ArmState  ,  -- | Program state, to extract the string labels
                  [ Instr ] )  -- | The instructions the program translates to
transProgram (Program funcs body) 
    = (s, instrs)
      where
        instrs 
          =  [ DEFINE ( JumpLabel "main:" ) ]  -- Define main function label
          ++ [ PUSH [ LR ]                  ]  -- Pushes the current return address onto the stack
          ++ instrs'                           -- 
          ++ [ LDR R0 0                     ]  -- 
          ++ [ POP  [ PC ]                  ]  --
          ++ [ INDIR Ltorg                  ] 
        (s, instrs') = transStat body state0 
        state0       = ArmState { stackMap      = Map.empty
                                , stackOffset   = 0 
                                , availableRegs = [R4 .. R10]
                                , numJumpLabels = 0
                                , dataLabels    = [] 
                                , predefLabels  = []
                                }

-- TODO rename lol
makePretty :: ( ArmState, [ Instr ] ) -- computed by transProgram
           ->   String                -- printable compiled program
makePretty (s, instrs) 
    =  show ( INDIR Data )  ++ "\n"
    ++ concatMap putDataLabel ( dataLabels s )
    ++ show ( INDIR Text )  ++ "\n"                  
    ++ show ( INDIR ( Global ( JumpLabel "main" ) ) ) ++ "\n"
    ++ ( concat $ intersperse "\n" $ map show instrs )

      where
        putDataLabel ( DataLabel l str ) 
          =  l
          ++ "\n\t.word " ++ show ( length str + 1 )
          ++ "\n\t.ascii \"" ++ str  ++ "\\0\"\n"


nextLabel :: Int -> Label
nextLabel i = JumpLabel $ "L" ++ show i




-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | The following transform functions will get the AST of a statement, expression,
--   or function and an ARM state containing information about labels and registers
--   It will return a set of instructions instead of the given piece of program and
--   an updated state wrapped in a tuple. 
--
-- trans____ :: *AST*      -- | Func, Stat, Expr to be compiled
--           -> ARMState   -- | Describing the state before transforming
--           -> ( ArmState  , -- | The instructions compiled out of the input
--                [ Instr ] ) -- | Describing the new state

-- properties of trans : normal, deterministic, total

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     Function Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- | 
transFunc :: Func      -> ArmState
          -> ( ArmState, [ Instr ] )
              
transFunc (Func ftype fname args body it) s 
  = (s', functInstrs)
    where
      functInstrs       
        =  [ DEFINE ( JumpLabel fname ) ]  -- Define label with unique function name 
        ++ [ PUSH [ LR ]                ]  -- Pushes current return address onto stack                       
        ++ bodyInstrs                      -- The instructions from the func body
        ++ [ POP  [ PC ]                ]  -- Restore program counter from the stack
      (s', bodyInstrs) = transStat body s



-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Statement Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transStat :: Stat -> ArmState -> ( ArmState, [ Instr ] )

transStat SkipStat s
  = (s, [])

transStat (FreeStat e it) s = error "FreeStat"

transStat (ExitStat e _) s 
  = (s', exitInstrs)
    where
      (s', exprInstrs) = transExpr e s
      exitInstrs       = exprInstrs ++ [ BL ( JumpLabel "exit" ) ]

transStat (ReturnStat e _) s = error "ReturnStat" 


transStat (PrintStat e it) s
  = ( s'', instrs')
    where
        -- The new state just updates the data labels
        s''          = s' { dataLabels = ls'' }

        -- First gets the instructions for the expr, then adds the print
        instrs'      = instrs ++ [ MOV'Reg R0 dst ] ++ label
        (s', instrs) = transExpr e s 
        (dst:_)      = availableRegs s'
        -- The print label/function called depends on the type
        label        = case typeOfExpr e it of
                          BoolType   -> [ BL $ JumpLabel "p_print_bool"   ]
                          CharType   -> [ BL $ JumpLabel "putchar"        ]
                          IntType    -> [ BL $ JumpLabel "p_print_int"    ]
                          StringType -> [ BL $ JumpLabel "p_print_string" ]  

        -- Updates the data labels with the strings to be printed
        ls'  = dataLabels s'
        ls'' = case typeOfExpr e it of
                  BoolType -> boolDataLabels ls'
                  _        -> ls'

        -- Generates the proper data labels for each type of the print param
        boolDataLabels ls = let (l,  ls' ) = newDataLabel "true"  ls  in
                            let (l', ls'') = newDataLabel "false" ls' in
                            ls''



transStat (PrintlnStat e it) s  = error "PrintlnStat"

transStat (ScopedStat stat) s
  = transStat stat s

transStat (ReadStat lhs it) s = error "TODO" 

transStat (WhileStat cond body it) s 
  = (s'', whileInstrs)
    where
      label0             =  nextLabel i 
      label1             =  nextLabel ( i + 1 )
      i                  =  numJumpLabels s
      (s'', condInstrs)  =  transExpr cond s'                
      (s' , bodyInstr )  =  transStat body sBody
      sBody              =  s { numJumpLabels = i + 2 }
      whileInstrs        =  [ B label0      ]          
                         ++ [ DEFINE label1 ]         
                         ++ bodyInstr                 
                         ++ [ DEFINE label0 ]         
                         ++ condInstrs                 
                         ++ [ CMP dst $ Op2'ImmVal 0 ] 
                         ++ [ BEQ label1    ]
      (dst:_)            =  availableRegs s 
transStat (SeqStat stat stat') s
  = (s'', stat1Instr ++ stat2Instr)
    where
      (s' , stat1Instr) = transStat stat  s 
      (s'', stat2Instr) = transStat stat' s'

transStat (DeclareStat vtype vname rhs it) s = error "DeclareStat" 

transStat (AssignStat lhs rhs it) s =  error "AssignStat" 

transStat (IfStat cond sthen selse it) s = error "IfStat" 

transRHS (RhsExpr e) s = transExpr e s


-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************   Expression Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- |
transExpr :: Expr -> ArmState -> ( ArmState, [ Instr ] )


-- | Put the value of boolean @b@ into the first avaialble register @dst@
transExpr (BoolLiterExpr b) s
  = (s, [ MOV dst (Op2'ImmVal $ if b then 1 else 0) ])
    where
        (dst:_) = availableRegs s

-- | Put the corresponding integer value of @c@ into the destination reg @dst@
transExpr (CharLiterExpr c) s
  = (s, [ MOV dst (Op2'ImmVal $ ord c) ])  -- todO use LDR
    where
        (dst:_) = availableRegs s

-- | Lookup what register variable @id@ is in, and copy its content in @dst@
transExpr (IdentExpr id) s
  = (s, [ MOV dst (Op2'Reg src) ]) -- TODO LOL
    where
      src = R0 --findReg id m 
      (dst:_) = availableRegs s


-- | Evaluates the expression and places it in the destination regster @dst@,
--   will perform  the unary operation on that reg 
transExpr (UnaryOperExpr op e) s
  = (s'', exprInstr ++ unopInstr)
    where
      (s' , exprInstr) = transExpr e  s 
      (s'', unopInstr) = transUnOp op s'

-- |
transExpr (ParenthesisedExpr e) s 
  = transExpr e s 

-- |
transExpr (IntLiterExpr i) s
  = (s, [ LDR dst i ])
    where 
      (dst:_) = availableRegs s

-- |
transExpr (StrLiterExpr str) s
  = (s, [ LDR'Lbl dst l ])
    where
      (dst:_)  = availableRegs s
      s'       = s { dataLabels = ls' }
      (l, ls') = newDataLabel str $ dataLabels s

-- |
transExpr PairLiterExpr s = error "PairLiterExpr" 

-- | TODO make ArrayElem a type synonym PLSSSSSSS
transExpr (ArrayElemExpr (ArrayElem ident exprs)) s = error "ArrayElemExpr"  

-- |
transExpr (BinaryOperExpr op e e') s = error "BinaryOperExpr"  
 

--------------------------------------------------------------------------------
-- | Create a new data label and return the list with the label added.
newDataLabel str ls 
  = (l, ls ++ [l]) 
    where 
      l     = DataLabel lName str
      lName = "msg_" ++ ( show $ length ls )


--------------------------------------------------------------------------------
-- | Generate instructions for a unary operator
transUnOp :: UnaryOper -> ArmState -> ( ArmState, [ Instr ] )
transUnOp NotUnOp s 
  = (s, unopInstrs)
    where
      unopInstrs =  [ EOR     dst dst $ Op2'ImmVal 1 ]
                 ++ [ MOV'Reg R0  dst ]
      (dst:_)    =  availableRegs s
    

transUnOp LenUnOp s = error "LenUnop"
transUnOp OrdUnOp s = error "OrdUnop"
transUnOp ChrUnOp s = error "ChrUnop"
transUnOp NegUnOp s = error "NegUnop"


----------------------------------------------------------------------------------




-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- TYPE OF EXPRESSION
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
sizeOf                :: Type -> Int  -- In bytes 
sizeOf IntType        =  4                                 
sizeOf BoolType       =  1                             
sizeOf CharType       =  1                             
sizeOf StringType     =  4 -- Addresss                              
sizeOf (PairType  _)  =  4 -- Address   
sizeOf (ArrayType _)  =  4 -- Address                         
sizeOf NullType       =  0 -- ?                                
sizeOf EmptyType      =  0 -- ?
 
typeOfExpr                                        :: Expr -> It -> Type    
typeOfExpr ( BoolLiterExpr     _            ) _   =  BoolType    
typeOfExpr ( CharLiterExpr     _            ) _   =  CharType      
typeOfExpr ( IdentExpr         id           ) it  =  fromJust (findType' id it)       
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
 
typeOfArrElem :: ArrayElem -> It -> Type
typeOfArrElem _ _ = NullType -- lazy to make it compile, irrelephant atm
--typeOfArrElem (id, es) it   = deepen (length es + 1) $ fromJust (findType' id it) 
--  where
--    deepen 0  t             =  t
--    deepen n (ArrayType t)  =  deepen (n-1) t
--    deepen _  t             =  t


sizeOfType :: Type -> Int  -- In bytes 
sizeOfType IntType       = 4                                 
sizeOfType BoolType      = 1                             
sizeOfType CharType      = 1                             
sizeOfType StringType    = 4  -- Addresss                              
sizeOfType (PairType  _) = 4 -- Address   
sizeOfType (ArrayType _) = 4 -- Address                         
sizeOfType NullType      = 0 -- ?                                
sizeOfType EmptyType     = 0 -- ?


