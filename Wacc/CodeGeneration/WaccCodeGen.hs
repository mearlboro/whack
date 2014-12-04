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
-- Prerequisites                                                                

type RegMap = Map.Map IdentName Reg

findReg       :: IdentName -> RegMap -> Reg 
findReg m id  =  fromJust $ Map.lookup m id 

type AvailRegs = [ Register ]

type ArmState = (RegMap, [Label], Int, AvailRegs) -- Int is number of labels used

type ExitCode = Int

nextLabel :: ExitCode -> Label
nextLabel i = JumpLabel $ "L" ++ show i



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
transProgram :: Program        -- | The program Augmented AST
             -> ( [ Instr ] ,  -- | Program state, to extract the string labels
                  ArmState  )  -- | The instructions the program translates to
transProgram (Program funcs body) 
    = (instrs, s)
      where
        instrs 
          =  [ DEFINE ( JumpLabel "main:" ) ]  -- Define main function label
          ++ [ PUSH [ LR ]                  ]  -- Pushes the current return address onto the stack
          ++ instrs'                           -- 
          ++ [ LDR R0 0                     ]  -- 
          ++ [ POP  [ PC ]                  ]  --
          ++ [ INDIR Ltorg                  ] 
        (instrs', s) = transStat body state0 
        state0       = (Map.empty, [], 0, [R4 .. R10])


-- TODO rename lol
makePretty :: ( [ Instr ], ArmState ) -- computed by transProgram
           ->   String                -- printable compiled program
makePretty (instrs, s@(_, ls, _, _)) 
    =  show ( INDIR Data )  ++ "\n"
    ++ concatMap putDataLabel ls
    ++ show ( INDIR Text )  ++ "\n"                  
    ++ show ( INDIR ( Global ( JumpLabel "main" ) ) ) ++ "\n"
    ++ ( concat $ intersperse "\n" $ map show instrs )

      where
        putDataLabel ( DataLabel l str ) 
          =  l
          ++ "\n\t.word " ++ show ( length str + 1 )
          ++ "\n\t.ascii \"" ++ str  ++ "\\0\"\n"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | The following transform functions will get the AST of a statement, expression,
--   or function and an ARM state containing information about labels and registers
--   It will return a set of instructions instead of the given piece of program and
--   an updated state wrapped in a tuple. 
--
-- trans____ :: *AST*      -- | Func, Stat, Expr to be compiled
--           -> ArmState   -- | Describing the state before transforming
--           -> ( [ Instr ], -- | The instructions compiled out of the input
--                ArmState ) -- | Describing the new state

-- properties of trans : normal, deterministic, total

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     Function Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- | 
transFunc :: Func      -> ArmState
          -> ( [ Instr ], ArmState )
              
transFunc (Func ftype fname args body it) s 
  = (functInstrs, s')
    where
      functInstrs       
        =  [ DEFINE ( JumpLabel fname ) ]  -- Define label with unique function name 
        ++ [ PUSH [ LR ]                ]  -- Pushes current return address onto stack                       
        ++ bodyInstrs                      -- The instructions from the func body
        ++ [ POP  [ PC ]                ]  -- Restore program counter from the stack
      (bodyInstrs, s') = transStat body s



-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Statement Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transStat :: Stat      -> ArmState
          -> ( [ Instr ], ArmState )

transStat SkipStat s
  = ([], s)

transStat (FreeStat e it) s = error "FreeStat"

transStat (ExitStat e _) s 
  = (exitInstrs, s')
    where
      (exprInstrs, s') = transExpr e s
      exitInstrs       = exprInstrs ++ [ BL ( JumpLabel "exit" ) ]

transStat (ReturnStat e _) s = error "ReturnStat" 

transStat (PrintStat e it) s@(m, ls, i, rs@(dst:_))
  = ( instrs', (m, ls'''', i, rs))
    where
        (instrs, (_, ls', _, _)) = transExpr e s 
        instrs' = instrs ++ [ MOV'Reg R0 dst ] ++ label
        label   = case typeOfExpr e it of
                   BoolType   -> [ BL $ JumpLabel "p_print_bool"   ]
                   CharType   -> [ BL $ JumpLabel "putchar"        ]
                   IntType    -> [ BL $ JumpLabel "p_print_int"    ]
                   StringType -> [ BL $ JumpLabel "p_print_string" ]  

        ls''''  = if typeOfExpr e it == BoolType
                      then 
                          let (l,  ls'' ) = newDataLabel "true"  ls'  in
                          let (l', ls''') = newDataLabel "false" ls'' in
                          ls'''
                      else ls'


transStat (PrintlnStat e it) s  = error "PrintlnStat"

transStat (ScopedStat stat) s
  = transStat stat s

transStat (ReadStat lhs it) s = error "TODO" 

transStat (WhileStat cond body it) s@(m, ls, i, rs@(dst:_)) 
  = (whileInstrs, s'')
    where
      label0             = nextLabel i 
      label1             = nextLabel (i + 1)
      (condInstrs, s'')  = transExpr cond s'                
      (bodyInstr,  s' )  = transStat body (m, ls, i + 2, rs)

      whileInstrs        =  [ B label0      ]          
                         ++ [ DEFINE label1 ]         
                         ++ bodyInstr                 
                         ++ [ DEFINE label0 ]         
                         ++ condInstrs                 
                         ++ [ CMP dst $ Op2'ImmVal 0 ] 
                         ++ [ BEQ label1    ]

transStat (SeqStat stat stat') s
  = (stat0Instr ++ stat1Instr, s'')
    where
      (stat0Instr, s' ) = transStat stat  s 
      (stat1Instr, s'') = transStat stat' s'

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
transExpr :: Expr       -> ArmState
          -> ( [ Instr ],  ArmState )


-- | Put the value of boolean @b@ into the first avaialble register @dst@
transExpr (BoolLiterExpr b) s@(_, _, _, (dst:_)) 
  = ( [ MOV dst (Op2'ImmVal $ if b then 1 else 0) ], s )


-- | Put the corresponding integer value of @c@ into the destination reg @dst@
transExpr (CharLiterExpr c) s@(_, _, _, (dst:_))
  = ( [ MOV dst (Op2'ImmVal $ ord c) ], s )  -- todO use LDR


-- | Lookup what register variable @id@ is in, and copy its content in @dst@ reg
transExpr (IdentExpr id) s@(m, _, _, (dst:_))
  = ( [ MOV dst (Op2'Reg src) ], s ) -- TODO LOL
    where
      src = findReg id m 


-- | Evaluates the expression and places it in the dest reg(dst) , performs the unary operation on that reg 
transExpr (UnaryOperExpr op e) s@(m, _, _, (dst:_))
  = (trace "unaryop" (exprInstr ++ unopInstr, s''))
    where
      ( exprInstr, s' ) = transExpr e  s 
      ( unopInstr, s'') = transUnOp op s'

-- |
transExpr (ParenthesisedExpr e) s 
  = transExpr e s 

-- |
transExpr (IntLiterExpr i) s@(_, _, _, rs@(dst:_)) = 
   ( [ LDR dst i ], s )

-- |
transExpr (StrLiterExpr str) s@(m, ls, i, rs@(dst:_))
  = ( [ LDR'Lbl dst l ], (m, ls', i, rs) )
    where
        (l, ls') = newDataLabel str ls

-- |
transExpr PairLiterExpr s@(m, ls, i, rs@(dst:_)) = error "PairLiterExpr" 

-- | TODO make ArrayElem a type synonym PLSSSSSSS
transExpr (ArrayElemExpr (ArrayElem ident exprs)) s@(m, ls, i, rs@(dst:_)) = error "ArrayElemExpr"  

-- |
transExpr (BinaryOperExpr op e e') s@(m, ls, i, rs@(dst:_)) = error "BinaryOperExpr"  
 

--------------------------------------------------------------------------------
newDataLabel str ls 
  = (l, ls ++ [l]) 
    where 
      l     = DataLabel lName str
      lName = "msg_" ++ ( show $ length ls )

-- | Generate instructions for a unary operator
transUnOp :: UnaryOper -> ArmState
          -> ( [ Instr ], ArmState )
transUnOp NotUnOp s@(m, ls, i, rs@(dst:_)) = (unopInstrs, s)
  where
    unopInstrs =  [ EOR R4 R4 $ Op2'ImmVal 1 ]
               ++ [ MOV'Reg R0 R4 ]
    

transUnOp LenUnOp s@(m, ls, i, rs@(dst:_)) = error "LenUnop"
transUnOp OrdUnOp s@(m, ls, i, rs@(dst:_)) = error "OrdUnop"
transUnOp ChrUnOp s@(m, ls, i, rs@(dst:_)) = error "ChrUnop"
transUnOp NegUnOp s@(m, ls, i, rs@(dst:_)) = error "NegUnop"


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


