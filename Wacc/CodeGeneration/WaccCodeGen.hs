module Wacc.CodeGeneration.WaccCodeGen where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.Data.DataTypes

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
    =  show ( INDIR Text )  ++ "\n"                  
    ++ ( concat $ intersperse "\n" $ map show ls  ) 
    ++ show ( INDIR ( Global ( JumpLabel "main" ) ) ) ++ "\n"
    ++ ( concat $ intersperse "\n" $ map show instrs )



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

-- |
transStat SkipStat s
  = ([], s)

-- |
transStat (FreeStat e it) s = error "FreeStat"

---- | 
transStat (ExitStat e _) s 
  = (exitInstrs, s')
    where
      (exprInstrs, s') = transExpr e s
      exitInstrs       = exprInstrs ++ [ BL ( JumpLabel "exit" ) ]

-- | 
transStat (ReturnStat e _) s = error "ReturnStat" 
-- = ((m, l', rs), instr)
--  where
--    (exprInstr, l') = transExpr e rs m l 
--    instr           = [ BL "exit" ]


transStat (PrintStat e it) s@(m, ls, i, rs) = error "PrintStat" 
--transStat (PrintStat e it) s@(m, ls, i, rs) 
--  = (instrs, (m, printStrLabel: ls', i, rs))
--    where
--
--      typez <- 
--      (exprInstrs, s'@(_, ls', _, _)) = transExpr e s  -- Will add extra labels if string or whatevs
--  
--      -- first define a SringLabel containing the information to be printed
--      printStrLabel = StringLabel ( "msg_" ++ cl ) "print_text" -- TODO: how does text even get here??????
--      cl = show $ length ls
--
--      instrs =  exprInstrs 
--             ++ [ DEFINE $ JumpLabel $ "p_print_" ++ "type" ] -- TODO: get type??
--             ++ [ PUSH [ LR ] ]
--             ++ printSetupInstrs 
--             ++ [ BL ( JumpLabel "printf" ) ]
--             ++ [ MOV R0 $ Op2'ImmVal 0 ]
--             ++ [ BL ( JumpLabel "fflush" ) ]
--             ++ [ POP [ PC ]  ] 
--      printSetupInstrs = []

transStat (PrintlnStat e it) s  = error "PrintlnStat"
-- |                
--transStat (PrintlnStat e it) s@(m, ls, rs)  =  ((m, l', rs), instr)
--  where
--    (exprInstr, l') = transExpr e rs m l 
--    instr           = exprInstr ++ [{- Special instructions to print? -}]


-- |                 
transStat (ScopedStat stat) s
  = transStat stat s


---- |                               
transStat (ReadStat lhs it) s = error "TODO" 


-- |                    
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

-- |             
transStat (SeqStat stat stat') s
  = (stat0Instr ++ stat1Instr, s'')
    where
      (stat0Instr, s' ) = transStat stat  s 
      (stat1Instr, s'') = transStat stat' s'

transStat (DeclareStat vtype vname rhs it) s = error "DeclareStat" 

-- |        
-- transStat (DeclareStat vtype vname rhs it) s 
--   = (declInstr, s')
--     where
--       STR' t = if sizeOfType t == 1 then STRB'Reg else STR'Reg
-- 
--       size                            = sizeOfType vtype
--       (rhsInstr, s'@(_, _, _, dst:_)) = transRHS rhs s -- TODO: que/
--       declInstr                       =  [ SUB SP SP $ Op2'ImmVal size ] -- Reserve space on the stack
--                                       ++ rhsInstr
--                                       ++ [ (STR' vtype) SP dst ]
--                                       ++ [ ADD SP SP $ Op2'ImmVal size ]

-- |                  
transStat (AssignStat lhs rhs it) s =  error "AssignStat" 

-- |              
transStat (IfStat cond sthen selse it) s = error "IfStat" 

---- |
--transLHS :: AssignLhs -> [ Instr ] 
--transLHS (LhsIdent id) = error "TODO"              
--transLHS (LhsPairElem pelem) = error "TODO"                 
--transLHS (LhsArrayElem (ArrayElem id exprs)) = error "TODO" 

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
transExpr (StrLiterExpr str) s@(m, ls, i, rs@(dst:_)) = error "NEED A DIRECTIVE FIELD IN ARM STATE" 

-- |
transExpr PairLiterExpr s@(m, ls, i, rs@(dst:_)) = error "PairLiterExpr" 

-- | TODO make ArrayElem a type synonym PLSSSSSSS
transExpr (ArrayElemExpr (ArrayElem ident exprs)) s@(m, ls, i, rs@(dst:_)) = error "ArrayElemExpr"  

-- |
transExpr (BinaryOperExpr op e e') s@(m, ls, i, rs@(dst:_)) = error "BinaryOperExpr"  
 


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


--extractVar                       :: Expr -> Store -> Variable 
--extractVar (IdentExpr ident) s   =  lookupStore s ident
--extractVar _                 _   =  error "extractIdent: Expecting IdentExpr"



-- sizeof :: Expr -> Word -- How many consecutive addresses does it take 
sizeof (BoolLiterExpr     _    ) = 1            
sizeof (CharLiterExpr     _    ) = 1            
sizeof (IdentExpr         name ) = error "WHHHHHHHAT"
sizeof (UnaryOperExpr _ _      ) = 1
sizeof (ParenthesisedExpr e    ) = sizeof e               
sizeof (IntLiterExpr      _    ) = 1               
sizeof (StrLiterExpr      str  ) = length str              
sizeof (PairLiterExpr          ) = 0  -- ?                
sizeof (ArrayElemExpr (ArrayElem arrName exprs)) = error "TODO"           
sizeof (BinaryOperExpr  _ e1 e2) = error "TODO"  

sizeOfType :: Type -> Int  -- In bytes 
sizeOfType IntType       = 4                                 
sizeOfType BoolType      = 1                             
sizeOfType CharType      = 1                             
sizeOfType StringType    = 4  -- Addresss                              
sizeOfType (PairType  _) = 4 -- Address   
sizeOfType (ArrayType _) = 4 -- Address                         
sizeOfType NullType      = 0 -- ?                                
sizeOfType EmptyType     = 0 -- ?


