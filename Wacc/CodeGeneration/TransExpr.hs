module Wacc.CodeGeneration.TransExpr
( transExpr
, transPairElemExpr
, transArrayLitExpr
) where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.CodeGeneration.TransCommon

import Wacc.Data.DataTypes

-- git checkout master --> git reset --hard <branch_name>

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


-- | 
transExpr s (ArrayElemExpr (id, exprs))
  = (s'', instrs)
    
      where 
        -- Adds the array bounds labels to the state
        s''            = stateAddArrayBounds s
        
        -- Updates the registers in the state
        s'             = s { freeRegs = nxt:regs }
        (dst:nxt:regs) = freeRegs s

        (src, off) = lookupLoc s id  
        -- The set of instructions 
        instrs =  [ ADD dst src $ Op2'ImmVal off ] 
               ++ concat ( map (transExpr') exprs )
               ++ [ LDR'Reg dst dst ] 

        -- Translates each
        transExpr' e = snd ( transExpr s' e )
                      ++ [ LDR'Reg dst dst ]
                      ++ [ MOV'Reg R0 nxt ]
                      ++ [ MOV'Reg R1 dst ]
                      ++ [ BL $ JumpLabel "p_check_array_bounds" ]
                      ++ [ ADD dst dst $ Op2'ImmVal 4 ]
                      ++ [ ADD dst dst $ Op2'LSL'Sh nxt 2 ]


-- | Lookup what register variable @id@ is in, and copy its content in @dst@
transExpr s (IdentExpr id) 
  = (s, pushDst) -- TODO LOL
    where
      (dst:_) = freeRegs s
      (src, off) = lookupLoc s id
      pushDst = [ LDR'Off dst src off ] -- [sp, #<off>] where 

-- |
transExpr s (ParenthesisedExpr e) 
  = transExpr s e 

-- 
transExpr s (UnaryOperExpr NegUnOp (IntLiterExpr i)) 
  = transExpr s (IntLiterExpr (-i))

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
      unopInstrs =  [ LDR'Lbl dst l ]   -- stores in dst the adrress of a string
                 ++ [ LDR'Reg dst dst ] -- puts into dst the length of the addr,
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
      s'            =  stateAddIntOverflowError s
      negUnOpInstrs =  [ RSBS dst dst $ Op2'ImmVal 0]
                    ++ [ BLVS ( JumpLabel "p_throw_overflow_error") ]

      (dst:_)    =  freeRegs s



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Generate instructions for a binary operator
transBinOp :: Assembler BinaryOper 

-- | Arithmetic operators
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
      s'       =  stateAddDivZeroError s 

      instrs   =  ( [ MOV'Reg R0 r ]
                 ++ [ MOV'Reg R1 r'] 
                 ++ [ BL  ( JumpLabel "p_check_divide_by_zero" ) ]
                 ++ [ BL  ( JumpLabel " __aeabi_idiv" ) ]
                 ++ [ MOV'Reg r R0 ] )
      (r:r':_) =  freeRegs s

transBinOp s ModBinOp 
  = ( s', instrs )
    where
      s'       =  stateAddDivZeroError s 

      instrs   =  ( [ MOV'Reg R0 r ]
                 ++ [ MOV'Reg R1 r'] 
                 ++ [ BL  ( JumpLabel "p_check_divide_by_zero" ) ]
                 ++ [ BL  ( JumpLabel " __aeabi_idivmod" ) ]
                 ++ [ MOV'Reg r R0 ] )
      (r:r':_) =  freeRegs s


-- | Logic operators
transBinOp s AndBinOp
  = ( s, instrs )
    where
      instrs   = ( [ AND'Reg r r r' ] )
      (r:r':_) = freeRegs s

transBinOp s OrrBinOp
  = ( s, instrs )
    where
      instrs   = ( [ ORR'Reg r r r' ] )
      (r:r':_) = freeRegs s

-- | Relational operators
transBinOp s LsBinOp  -- less than <
  = ( s, instrs )
    where
      instrs   = ( [ MOVLT r $ Op2'ImmVal 1 ]
               ++  [ MOVGE r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s GtBinOp  -- greater than >
  = ( s, instrs )
    where
      instrs   = ( [ MOVGT r $ Op2'ImmVal 1 ]
               ++  [ MOVLE r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s LEBinOp -- less equal <=
  = ( s, instrs )
    where
      instrs   = ( [ MOVLE r $ Op2'ImmVal 1 ]
               ++  [ MOVGT r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s GEBinOp -- greater equal >=
  = ( s, instrs )
    where
      instrs   = ( [ MOVGE r $ Op2'ImmVal 1 ]
               ++  [ MOVLT r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s EqBinOp -- equal ==
  = ( s, instrs )
    where
      instrs   = ( [ MOVEQ r $ Op2'ImmVal 1 ]
               ++  [ MOVNE r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s NEBinOp -- not equal !=
  = ( s, instrs )
    where
      instrs   = ( [ MOVNE r $ Op2'ImmVal 1 ]
               ++  [ MOVEQ r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s



-- ************************************************************************** --
-- ****************                                         ***************** --
-- ****************   Complicated Expression Translation    ***************** --
-- ****************                                         ***************** -- 
-- ************************************************************************** --

-- TODO oemge refecter ples
type WhichOfTheTwo = Int 
transPairElemExpr :: Assembler (Expr, It, WhichOfTheTwo) 
transPairElemExpr s (expr, it, wott) 
  = (s'' { freeRegs = rs }, instr)
    where 
      rs@(dst:nxt:regs) = freeRegs s 
      (s', exprInstr) = transExpr s { freeRegs = nxt:regs } expr 
      size = sizeOf expr it
      instr 
        =  exprInstr 
        ++ [ LDR R0 size             ] 
        ++ [ BL (JumpLabel "malloc") ]
        ++ [ strVar nxt R0 size 0    ]
        ++ [ strVar R0 dst 4 (wott * 4) ]

      s'' = stateAddCheckNullPtr s'

-- TODO oemge refecter ples
-- Translates an array 
transArrayLitExpr :: Assembler ([ Expr ], It)
transArrayLitExpr arm (es, it) = 
  transArray' es 0 (arm, [])
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
transArrayElem arm (e, it, index) = 
  (arm' { freeRegs= r } , exprInstr ++ storeInstr)
    where
      r@(dst:nxt:regs) = freeRegs arm 

      -- Translate the expression 
      (arm', exprInstr) = transExpr arm{freeRegs = nxt:regs} e
      -- At what index in the heap the elem is
      -- +4 Beause the first 4 bytes are taken up by the length
      offset = (index * (sizeOf e it)) + 4 -- MAGIC NUMBER NOOO 
      storeInstr = if typeOf e it == BoolType  -- TODO func for CHAR 
                      then [STRB'Off nxt dst offset]
                      else [STR'Off  nxt dst offset]  


