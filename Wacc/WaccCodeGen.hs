module Wacc.WaccCodeGen where

import Wacc.ARM11Instructions
import Wacc.WaccDataTypes
import qualified Data.Map as Map
import Data.Array
import Data.Maybe
import Data.Char


--------------------------------------------------------------------------------
--  REGISTER MAP  --------------------------------------------------------------
--------------------------------------------------------------------------------

type RegMap = Map.Map IdentName Reg

findReg       :: IdentName -> RegMap -> Reg 
findReg m id  =  fromJust $ Map.lookup m id 

type LabelNum = Int -- [ Label ]

nextLabel    :: LabelNum -> Label 
nextLabel l  =  "label" ++ show l

type AvailRegs = [ Register ]

type ArmState = (RegMap, LabelNum, AvailRegs)

--------------------------------------------------------------------------------
--  PROGRAM  -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | 
transProgram :: Program -> [ Instr ]
transProgram (Program funcs body) =

    [ DEFINE "main:"           ] ++ -- Define main function label
    [ PUSH [ LR ]              ] ++ -- Pushes the current return address onto the stack
    snd (transStat body state)   ++ -- 
    [ LDR R0 0                 ] ++ -- 
    [ POP  [ PC ]              ]    -- 
  where
    state = (Map.empty, 0, [R4 .. R10])

--------------------------------------------------------------------------------
--  FUNC  ----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | 
transFunc :: Func -> ArmState -> (ArmState, [ Instr ])
transFunc (Func ftype fname args body it) arm = (arm', functInstr)
  where
    (arm', bodyInstr) = transStat body arm
    functInstr        = [ DEFINE fname ] ++ -- Define label with function name which we know is unique
                        [ PUSH [ LR ]  ] ++ -- Pushes the current return address onto the stack
                        bodyInstr        ++ 
                        [ POP  [ PC ]  ]    -- Restore program counter from the stack
  
  


--------------------------------------------------------------------------------
--  STAT  ----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | 
transStat 
  :: Stat 
  -> ArmState 
  -> (ArmState, [ Instr ])

-- |
transStat SkipStat s@(m, l, rs)  =  (s, [])

-- |
--transStat (FreeStat e it) s@(m, l, rs)s = error "TODO"
--  where
--    is = transExpr e rs s ++  [{- Special instructions to free expression? -}]

---- | 
transStat (ExitStat e _) s@(m, l, rs)  =  (s', exitInstr)
  where
    (s', exprInstr) = transExpr e s
    exitInstr       = exprInstr ++ [ BL "exit" ]

-- | 
--transStat (ReturnStat e _) s@(m, l, rs)  =  ((m, l', rs), instr)
--  where
--    (exprInstr, l') = transExpr e rs m l 
--    instr           = [ BL "exit" ]


---- | 
--transStat (PrintStat e _) s@(m, l, rs)  =  ((m, l', rs), instr)
--  where
--    (exprInstr, l') = transExpr e rs m l 
--    instr           = exprInstr ++ [{- Special instructions to print? -}]


---- |                
--transStat (PrintlnStat e it) s@(m, l, rs)  =  ((m, l', rs), instr)
--  where
--    (exprInstr, l') = transExpr e rs m l 
--    instr           = exprInstr ++ [{- Special instructions to print? -}]


-- |                 
transStat (ScopedStat stat) s  =  transStat stat s  


---- |                               
--transStat (ReadStat lhs it) h s rs = error "TODO" 


-- |                    
transStat (WhileStat cond body it) s@(m, l, rs@(dst:_))  = (s'', whileInstr)
  where
    label0           = nextLabel l 
    label1           = nextLabel (l+1)
    (s'', condInstr) = transExpr cond s'
    (s',  bodyInstr) = transStat body (m, l+2, rs)
    whileInstr       = [ B label0       ] ++ 
                       [ DEFINE label1  ] ++
                       bodyInstr ++
                       [ DEFINE label0  ] ++
                       condInstr ++
                       [ CMP dst $ Op2'ImmVal 0 ] ++
                       [ BEQ label1 ] 


-- |             
transStat (SeqStat stat stat') s = 
    (s'', stat0Instr ++ stat1Instr)
  where
    (s',  stat0Instr) = transStat stat  s
    (s'', stat1Instr) = transStat stat' s'


-- |        
transStat (DeclareStat vtype vname rhs it) s = (s', declInstr)
  where
    instrSTR :: Type -> (Rd -> Int -> Instr)
    instrSTR t = if sizeOfType t == 1 then STRB else STR

    size                         = sizeOfType vtype
    (s'@(_, _, dst:_), rhsInstr) = transRHS s 
    declInstr                    = [ SUB SP SP $ Op2'ImmVal size ] ++ -- Reserve space on the stack
                                   rhsInstr ++
                                   [ instrSTR vtype SP dst ] ++ 
                                   [ ADD SP SP $ Op2'ImmVal size ]

    -- Check type of varialbe. If primitive save its value into register

---- |                  
--transStat (AssignStat lhs rhs it) h s rs =  error "TODO" 

---- |              
--transStat (IfStat cond sthen selse it) h s rs = error "TODO" 

---- |
--transLHS :: AssignLhs -> [ Instr ] 
--transLHS (LhsIdent id) = error "TODO"              
--transLHS (LhsPairElem pelem) = error "TODO"                 
--transLHS (LhsArrayElem (ArrayElem id exprs)) = error "TODO" 

transRHS = error "TODO"

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************   Expression Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- |
transExpr 
  :: Expr                  -- *                
  -> ArmState              -- *  
  -> (ArmState, [ Instr ]) -- *               


-- | Put the value of boolean @b@ into the first avaialble register @dst@
transExpr (BoolLiterExpr b) s@(_, _, (dst:_))  = 
  (s, [ MOV dst (Op2'ImmVal $ if b then 1 else 0) ])


-- | Put the corresponding integer value of @c@ into the destination reg @dst@
transExpr (CharLiterExpr c) s@(_, _, (dst:_))  = 
  (s, [ MOV dst (Op2'ImmVal $ ord c) ])  -- todO use LDR


-- | Lookup what register variable @id@ is in, and copy its content in @dst@ reg
transExpr (IdentExpr id) s@(m, _, (dst:_)) = 
    (s, [ MOV dst (Op2'Reg src) ]) -- TODO LOL
  where
    src = findReg id m 


-- | Evaluates the expression and places it in the dest reg(dst) , performs the unary operation on that reg 
transExpr (UnaryOperExpr op e) s@(m, _, (dst:_)) = 
    (s'', exprInstr ++ unopInstr)
  where
    (s',  exprInstr)  =  transExpr e s
    (s'', unopInstr)  =  transUnOp op s'

-- |
transExpr (ParenthesisedExpr e) s = transExpr e s

-- |
transExpr (IntLiterExpr i) s@(m, l, rs@(dst:_)) = error "TODO"  

-- |
transExpr (StrLiterExpr str) s@(m, l, rs@(dst:_)) = error "TODO"  

-- |
transExpr PairLiterExpr s@(m, l, rs@(dst:_)) = error "TODO" 

-- | TODO make ArrayElem a type synonym PLSSSSSSS
transExpr (ArrayElemExpr (ArrayElem ident exprs)) s@(m, l, rs@(dst:_)) = error "TODO"  

-- |
transExpr (BinaryOperExpr op e e') s@(m, l, rs@(dst:_)) = error "TODO"  
 

-- | Generate instructions for a unary operator
transUnOp :: UnaryOper -> ArmState -> (ArmState, [Instr])
transUnOp NotUnOp s@(m, l, rs@(dst:_)) = ((m, l+1, rs), unopInstr)
  where
    label      =  nextLabel l 
    unopInstr  =  [ CBZ dst label ]          ++
                  [ MOV dst $ Op2'ImmVal 0 ] ++
                  [ DEFINE label ]           ++
                  [ MOV dst $ Op2'ImmVal 1 ]


transUnOp LenUnOp s@(m, l, rs@(dst:_)) = error "TODO"
transUnOp OrdUnOp s@(m, l, rs@(dst:_)) = error "TODO"
transUnOp ChrUnOp s@(m, l, rs@(dst:_)) = error "TODO"
transUnOp NegUnOp s@(m, l, rs@(dst:_)) = error "TODO"



----------------------------------------------------------------------------------

---- | Converts integer constant @n@ so that it can be used as operand2 #<imm8m>
--asImm8m :: Int -> Int 
--asImm8m n = error "TODO"

----------------------------------------------------------------------------------


--extractVar                       :: Expr -> Store -> Variable 
--extractVar (IdentExpr ident) s   =  lookupStore s ident
--extractVar _                 _   =  error "extractIdent: Expecting IdentExpr"



--sizeof :: Expr -> Word -- How many consecutive addresses does it take 
--sizeof (BoolLiterExpr     _    ) = 1            
--sizeof (CharLiterExpr     _    ) = 1            
--sizeof (IdentExpr         name ) = error "WHHHHHHHAT"
--sizeof (UnaryOperExpr _ _      ) = 1
--sizeof (ParenthesisedExpr e    ) = sizeof e               
--sizeof (IntLiterExpr           ) = 1               
--sizeof (StrLiterExpr      str  ) = length str              
--sizeof (PairLiterExpr          ) = 0  -- ?                
--sizeof (ArrayElemExpr (ArrayElem arrName exprs)) = error "TODO"           
--sizeof (BinaryOperExpr  _ e1 e2) = error "TODO"  

sizeOfType              :: Type -> Int  -- In bytes 
sizeOfType IntType      = 4                                 
sizeOfType BoolType     = 1                             
sizeOfType CharType     = 1                             
sizeOfType StringType   = 4 -- Addresss                              
sizeOfType (PairType  _) = 4 -- Address   
sizeOfType (ArrayType _) = 4 -- Address                         
sizeOfType NullType     = 0 -- ?                                
sizeOfType EmptyType    = 0 -- ?                             




