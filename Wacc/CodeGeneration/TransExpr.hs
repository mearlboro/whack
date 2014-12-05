module Wacc.CodeGeneration.TransExpr
( transExpr
) where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.CodeGeneration.TransCommon

import Wacc.Data.DataTypes

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************   Expression Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- 
transExpr :: Assembler (Expr, It)
transExpr = error "MERGE"

---- 
--transExpr (IntLiterExpr i) s
--  = (s, [ LDR dst i ])
--    where 
--      (dst:_) = availableRegs s

---- | Put the value of boolean @b@ into the first avaialble register @dst@
--transExpr (BoolLiterExpr b) s
--  = (s, [ MOV dst (Op2'ImmVal $ if b then 1 else 0) ])
--    where
--        (dst:_) = availableRegs s

---- | Put the string into a dataLabel and the label into the first register @dst@
--transExpr (StrLiterExpr str) s
--  = (s', [ LDR'Lbl dst l ])
--    where
--      (dst:_)  = availableRegs s
--      s'       = s { dataLabels = ls' }
--      (l, ls') = newDataLabel str $ dataLabels s


---- | Put the char @c@ into the destination reg @dst@
--transExpr (CharLiterExpr c) s
--  = (s, [ MOV dst (Op2'ImmChr c) ])
--    where
--      (dst:_) = availableRegs s

---- TODO! TEST!
---- | Lookup what register variable @id@ is in, and copy its content in @dst@
--transExpr (IdentExpr id) s = (s, pushDst) -- TODO LOL
--    where
--      (dst:_) = availableRegs s
      
--      hello = Map.lookup id (stackMap s)
--      (src, off) = fromJust $ Map.lookup id (stackMap s)

--      pushDst = [ LDR'Off dst src off ] -- [sp, #<off>] where TODO check 0 case

--transExpr (UnaryOperExpr NegUnOp (IntLiterExpr i)) s 
--  = transExpr (IntLiterExpr (-i)) s

---- | Evaluates the expression and places it in the destination regster @dst@,
----   will perform  the unary operation on that reg 
--transExpr (UnaryOperExpr op e) s
--  = (s'', exprInstr ++ unopInstr)
--    where
--      (s' , exprInstr) = transExpr e  s 
--      (s'', unopInstr) = transUnOp op s'

---- |
--transExpr (ParenthesisedExpr e) s 
--  = transExpr e s 

---- |
--transExpr (PairLiterExpr) s = (s, [ LDR dst size ])
--  where
--    (dst:_) = availableRegs s 
--    size = 0 -- sizeOf NullType



---- | TODO make ArrayElem a type synonym PLSSSSSSS
--transExpr (ArrayElemExpr (ident, exprs)) s = error "ArrayElemExpr"  

---- |
--transExpr (BinaryOperExpr op e e') s = error "BinaryOperExpr"  
 

----------------------------------------------------------------------------------
---- | Create a new data label and return the list with the label added.
--newDataLabel str ls 
--  = (l, ls ++ [l]) 
--    where 
--      l     = DataLabel lName str
--      lName = "msg_" ++ ( show $ length ls )


----------------------------------------------------------------------------------
---- | Generate instructions for a unary operator
--transUnOp :: UnaryOper -> ArmState -> ( ArmState, [ Instr ] )
--transUnOp NotUnOp s 
--  = (s, unopInstrs)
--    where
--      unopInstrs =  [ EOR     dst dst $ Op2'ImmVal 1 ]
--                 ++ [ MOV'Reg R0  dst ]
--      (dst:_)    =  availableRegs s
    
--transUnOp LenUnOp s
--  = (s, unopInstrs)
--    where 
--      unopInstrs =  [ LDR'Lbl dst l ]  -- stores in dst the adrress of a string
--                 ++ [ LDR'Reg dst dst ]-- puts into dst the length of the addr,
--                                        -- meaning the legth of the string
--                  -- REDO comment
--      (l:_)      =  dataLabels    s
--      (dst:_)    =  availableRegs s

---- | Ints and chars are treated the same by ARM, so there is not need to do
---- anything out of the ordinary regarding Ord and Chr  
--transUnOp OrdUnOp s = (s, [])

--transUnOp ChrUnOp s = (s, []) 

--transUnOp NegUnOp s 
--  = (s, negUnOpInstrs)
--    where 
--      negUnOpInstrs =  [ RSBS dst dst $ Op2'ImmVal 0]  -- reverse subtract | dst := 0 - dst
--                    ++ [ BLVS $ l ]     -- jumps to pThrow if overflow
--                                             -- |_Change this
--      (l:_)      =  dataLabels    s
--      (dst:_)    =  availableRegs s

