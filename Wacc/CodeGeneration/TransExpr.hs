module Wacc.CodeGeneration.TransExpr
( transExpr
, transPairElemExpr
, transArrayLitExpr
) where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.CodeGeneration.TransCommon

import Wacc.Data.DataTypes

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************   Expression Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- |
transExpr :: Assembler Expr

-- |
transExpr s (IntLiterExpr i)
  = (s, [ LDR dst i ])
    where 
      (dst:_) = freeRegs s

-- | Put the value of boolean @b@ into the first avaialble register @dst@
transExpr s (BoolLiterExpr b)
  = (s, [ MOV dst (Op2'ImmVal $ if b then 1 else 0) ])
    where
        (dst:_) = freeRegs s

-- | Put the string into a dataLabel and the label into the first register @dst@
transExpr s (StrLiterExpr str)
  = (s', [ LDR'Lbl dst l ])
    where
      (dst:_) = freeRegs s
      s'      = s { dataLabels = l:ls }
      ls      = dataLabels s
      l       = newDataLabel str $ dataLabels s


-- | Put the char @c@ into the destination reg @dst@
transExpr s (CharLiterExpr c)
  = (s, [ MOV dst (Op2'ImmChr c) ])
    where
      (dst:_) = freeRegs s

-- |
transExpr s PairLiterExpr 
  = (s, [ LDR R0 8] ++ [ BL $ JumpLabel "malloc" ])


-- | TODO make ArrayElem a type synonym PLSSSSSSS
transExpr s (ArrayElemExpr (ident, exprs)) = error "ArrayElemExpr"  

-- TODO! TEST!
-- | Lookup what register variable @id@ is in, and copy its content in @dst@
transExpr s (IdentExpr id) = (s, pushDst) -- TODO LOL
    where
      (dst:_) = freeRegs s
      (src, off) = lookupLoc s id
      pushDst = [ LDR'Off dst src off ] -- [sp, #<off>] where 

-- |
transExpr s (ParenthesisedExpr e) 
  = transExpr s e 

-- | Evaluates the expression and places it in the destination regster @dst@,
--   will perform  the unary operation on that reg 
transExpr s (UnaryOperExpr op e)
  = (s'', exprInstr ++ unopInstr)
    where
      (s' , exprInstr) = transExpr s  e  
      (s'', unopInstr) = transUnOp s' op 


-- |
transExpr s (BinaryOperExpr op e e')
  = (s''', chikiInstr ++ chakaInstr ++ binopInstr)
    where
      (s'  , chikiInstr) = transExpr  s   e  
      (_   , chakaInstr) = transExpr  s'' e' 
      (s''', binopInstr) = transBinOp s'  op 
      s''    = s' { freeRegs = rs }
      (r:rs) = freeRegs s'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Generate instructions for a unary operator
transUnOp :: Assembler UnaryOper 
transUnOp s NotUnOp 
  = (s, unopInstrs)
    where
      unopInstrs =  [ EOR     dst dst $ Op2'ImmVal 1 ]
                 ++ [ MOV'Reg R0  dst ]
      (dst:_)    =  freeRegs s
    
transUnOp s LenUnOp
  = (s, unopInstrs)
    where 
      unopInstrs =  [ LDR'Lbl dst l ]  -- stores in dst the adrress of a string
                 ++ [ LDR'Reg dst dst ]-- puts into dst the length of the addr,
                                        -- meaning the legth of the string
                  -- REDO comment
      (l:_)      =  dataLabels s
      (dst:_)    =  freeRegs s

-- | Ints and chars are treated the same by ARM, so there is not need to do
-- anything out of the ordinary regarding Ord and Chr  
transUnOp s OrdUnOp = (s, [])

transUnOp s ChrUnOp = (s, []) 

transUnOp s NegUnOp 
  = (s', negUnOpInstrs)
    where 
      s'            = stateAddIntOverflowError s
      negUnOpInstrs =  [ RSBS dst dst $ Op2'ImmVal 0]  -- reverse subtract | dst := 0 - dst
                    ++ [ BLVS ( JumpLabel "p_throw_overflow_error") ]

      (dst:_)    =  freeRegs s



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Generate instructions for a unary operator
transBinOp :: Assembler BinaryOper 
transBinOp s AddBinOp 
  = ( s', instrs )
    where
      s'       =  stateAddIntOverflowError s 

      instrs   =  ( [ ADDS r r r' ] 
               ++ [ BLVS ( JumpLabel "p_throw_overflow_error") ] )
      (r:r':_) =  freeRegs s

transBinOp s SubBinOp 
  = ( s', instrs )
    where
      s'       =  stateAddIntOverflowError s 

      instrs   =  ( [ SUBS r r r' ] 
               ++ [ BLVS ( JumpLabel "p_throw_overflow_error") ] )
      (r:r':_) =  freeRegs s

transBinOp s MulBinOp 
  = ( s', instrs )
    where
      s'       =  stateAddIntOverflowError s 

      instrs   =  ( [ SMULL r r' r r' ]
               ++ [ CMP r' $ Op2'ASR'Sh r 31 ]
               ++ [ BLNE ( JumpLabel "p_throw_overflow_error") ] )
      (r:r':_) =  freeRegs s

transBinOp s DivBinOp 
  = ( s', instrs )
    where
      s'     =  stateAddDivZeroError s 

      instrs =  ( [ MOV'Reg R0 r ]
               ++ [ MOV'Reg R1 r'] 
               ++ [ BL  ( JumpLabel "p_check_divide_by_zero" ) ]
               ++ [ BL  ( JumpLabel " __aeabi_idiv" ) ]
               ++ [ MOV'Reg r R0 ] )
      (r:r':_) =  freeRegs s



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Adds the overflow error data and predef labels as needed for all integers
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


stateAddDivZeroError s
  = s { dataLabels = ls', predefLabels = ps' }
    where
      (ls', ps')
        = if not $ containsLabel "p_check_divide_by_zero" ps 
            then
              let l        = newDataLabel   "%.*s" ls in  
              let p        = strPrintPredef l         in 
              let (l', p') = divZeroErrPredef   (l:ls)    in
              let p''      = runtErrPredef            in 
              (l': l: ls, ps ++ p ++ p' ++ p'')
            else
              (ls,   ps)

      ls           = dataLabels     s
      ps           = predefLabels   s

-- ************************************************************************** --
-- ****************                                         ***************** --
-- ****************   Complicated Expression Translation    ***************** --
-- ****************                                         ***************** -- 
-- ************************************************************************** --

-- TODO oemge refecter ples
type WhichOfTheTwo = Int 
transPairElemExpr :: Assembler (Expr, It, WhichOfTheTwo) 
transPairElemExpr s (expr, it, wott) = (s' { freeRegs = rs }, instr)
  where 
    rs@(dst:nxt:regs) = freeRegs s 
    (s', exprInstr) = transExpr s { freeRegs = nxt:regs } expr 
    size = (sizeOf (typeOfExpr expr it))
    instr 
      =  exprInstr 
      ++ [ LDR R0 size             ] 
      ++ [ BL (JumpLabel "malloc") ]
      ++ [ strVar nxt R0 size 0    ]
      ++ [ strVar R0 dst 4 (wott * 4) ]
  
-- TODO oemge refecter ples
-- Translates an array 
transArrayLitExpr :: Assembler ([ Expr ], It)
transArrayLitExpr arm (es, it) = transArray' es 0 (arm, [])
      where
        transArray' :: [ Expr ] -> Int -> (ArmState, [Instr]) -> (ArmState, [ Instr ])
        transArray' [] _ result            = result
        transArray' (e:es) index (arm, is) = (arm'', is ++ is'')
          where
            s@(arm', _is') = transArrayElem arm (e, it, index) 
            (arm'', is'') = transArray' es (index+1) s

-- TODO oemge refecter ples
--Translates an array elem 
transArrayElem :: Assembler (Expr, It, Int) 
transArrayElem arm (e, it, index) = (arm' { freeRegs= r } , exprInstr ++ storeInstr)
    where
        
        r@(dst:nxt:regs) = freeRegs arm 

        -- Translate the expression 
        (arm', exprInstr) = transExpr arm{freeRegs = nxt:regs} e
        -- At what index in the heap the elem is
        -- +4 Beause the first 4 bytes are taken up by the length
        offset = (index * (sizeOf (typeOfExpr e it))) + 4 -- MAGIC NUMBER NOOO 
        storeInstr = if typeOfExpr e it == BoolType  -- TODO func for CHAR 
                        then [STRB'Off nxt dst offset]
                        else [STR'Off  nxt dst offset]  


