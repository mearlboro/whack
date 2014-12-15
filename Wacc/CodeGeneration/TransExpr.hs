module Wacc.CodeGeneration.TransExpr
( transExpr
, transPairElemExpr
, transArrayLitExpr
) where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.CodeGeneration.TransCommon

import Wacc.Data.DataTypes

import Data.Char (ord)

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************   Expression Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- |
transExpr :: Assembler (Expr, It)

-- |
transExpr s (IntLiterExpr i, _)
  = (s, [ LDR dst i ])
    where 
      (dst:_) = freeRegs s

-- | Put the value of boolean @b@ into the first avaialble register @dst@
transExpr s (BoolLiterExpr b, _)
  = (s, [ MOV dst (Op2'ImmVal $ if b then 1 else 0) ])
    where
        (dst:_) = freeRegs s

-- | Put the string into a dataLabel and the label into the first register @dst@
transExpr s (StrLiterExpr str, _)
  = (s', [ LDR'Lbl dst l ])
    where
      (dst:_) = freeRegs s
      s'      = s { dataLabels = l:ls }
      ls      = dataLabels s
      l       = newDataLabel str $ dataLabels s


-- | Put the char @c@ into the destination reg @dst@
transExpr s (CharLiterExpr c, _)
  = (s, [ MOV dst $ Op2'ImmVal (ord c) ]) --(s, [ MOV dst (Op2'ImmChr c) ]) 
    where
      (dst:_) = freeRegs s

-- |
transExpr s (PairLiterExpr, _) = (s, [ LDR dst 0 ])
  where
    (dst:_) = freeRegs s -- ++ [ BL $ JumpLabel "malloc" ])


-- | TODO make ArrayElem a type synonym PLSSSSSSS
--transExpr s (ArrayElemExpr (id, exprs)), _)
--  =    (s', addI ++ concat ( map (transExpr') exprs )
--         ++ [ LDR'Reg dst dst ] )
--          -- ++ [ STR'Reg dst SP ] )
--    where 
--      (dst:nxt:regs) = freeRegs s
--      (src, off) = lookupLoc s id  
--      addI =  [ ADD dst src $ Op2'ImmVal off ] 


--      s' = s { freeRegs = nxt:regs }
---- (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
--      (s'', elemIs) = mapAccumL (trasExpr') s' exprs 

--      transExpr' arm expr = (arm', 
--        where
--          (arm', exprI) = transExpr arm expr 
--                        ++ [ LDR'Reg dst dst ]
--                        ++ [ MOV'Reg R0 nxt ]
--                        ++ [ MOV'Reg R1 dst ]
--                        ++ [ BL $ JumpLabel "p_check_array_bounds" ]
--                        ++ [ ADD dst dst $ Op2'ImmVal 4 ]
--                        ++ [ ADD dst dst $ Op2'LSL'Sh nxt 2 ]

transExpr s (ArrayElemExpr (id, exprs), it) = (s'', addI ++ elemsI ++ [ ldrVar dst dst size 0 ])
  where
    (dst:nxt:regs) = freeRegs s
    (src, off)     = lookupLoc s id 
    addI =  [ ADD dst src $ Op2'ImmVal off ] 

    size = sizeOf (id, exprs) it 
    (s', elemsI) = transDims s exprs 

    s'' = stateAddArrayBounds s'

    transDims s []     = (s,           [])
    transDims s (e:es) = (s'', eI' ++ esI)
      where

        finalAddI  =  ADD dst dst (if size == 1 then Op2'Reg nxt else Op2'LSL'Sh nxt 2)  

        isLast     = null es 
        (s',  eI)  = transExpr s { freeRegs = nxt:regs }  (e, it) 
        (s'', esI) = transDims s' es 
        eI'        = 
          eI ++ 
          [ LDR'Reg dst dst 
          , MOV'Reg R0 nxt -- Args for p_check_array_bounds
          , MOV'Reg R1 dst 
          , BL (JumpLabel "p_check_array_bounds") 
          , ADD dst dst $ Op2'ImmVal 4 ] ++
          if isLast then [ finalAddI ] else [ ADD dst dst $ Op2'LSL'Sh nxt 2 ]


-- | Lookup what register variable @id@ is in, and copy its content in @dst@
transExpr s (IdentExpr id, it) 
  = (s, pushDst) -- TODO LOL
    where
      (dst:_) = freeRegs s
      (src, off) = lookupLoc s id
      size = sizeOf (IdentExpr id) it 
      pushDst = [ ldrVar dst src size off ] --LDR'Off dst src off ] -- [sp, #<off>] where 

-- |
transExpr s (ParenthesisedExpr e, it) 
  = transExpr s (e, it) 

-- 
transExpr s (UnaryOperExpr NegUnOp (IntLiterExpr i), it) 
  = transExpr s (IntLiterExpr (-i), it)

-- | Evaluates the expression and places it in the destination regster @dst@,
--   will perform  the unary operation on that reg 
transExpr s (UnaryOperExpr op e, it)
  = (s'', exprInstr ++ unopInstr)
    where
      (s' , exprInstr) = transExpr s  (e,  it)  
      (s'', unopInstr) = transUnOp s' (op, e) 


-- |
transExpr s (BinaryOperExpr op e e', it)
  = (s''', chikiInstr ++ chakaInstr ++ binopInstr)
    where
      (s'  , chikiInstr) = transExpr  s   (e,  it) 
      (_   , chakaInstr) = transExpr  s'' (e', it) 
      (s''', binopInstr) = transBinOp s'  op 
      s''    = s' { freeRegs = rs }
      (r:rs) = freeRegs s'


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Generate instructions for a unary operator
transUnOp :: Assembler (UnaryOper, Expr) 
transUnOp s (NotUnOp, _)
  = (s, unopInstrs)
    where

      unopInstrs =  [ EOR     dst dst $ Op2'ImmVal 1 ]
                  -- ++ [ MOV'Reg R0  dst ]
      (dst:_)    =  freeRegs s
    
transUnOp s (LenUnOp, e)
  = (s, [ LDR'Reg dst dst ])
    where 
       -- = case typeOf  of 
            --StringType   -> 
            --ArrayType {} -> 
      unopInstrs =  [ LDR'Lbl dst l ]   -- stores in dst the adrress of a string
                 -- ++ [ LDR'Reg dst dst ] -- puts into dst the length of the addr,
                                        -- meaning the legth of the string
                  -- REDO comment
      (l:_)      =  dataLabels s
      (dst:_)    =  freeRegs s

-- | Ints and chars are treated the same by ARM, so there is not need to do
-- anything out of the ordinary regarding Ord and Chr  
transUnOp s (OrdUnOp, _) = (s, [])

transUnOp s (ChrUnOp, _) = (s, []) 

transUnOp s (NegUnOp, _) 
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
                 ++ [ MOV'Reg r R1 ] )
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
      instrs   = ( [ CMP   r $ Op2'Reg r'   ]
      	       ++  [ MOVLT r $ Op2'ImmVal 1 ]
               ++  [ MOVGE r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s GtBinOp  -- greater than >
  = ( s, instrs )
    where
      instrs   = ( [ CMP   r $ Op2'Reg r'   ]
      	       ++  [ MOVGT r $ Op2'ImmVal 1 ]
               ++  [ MOVLE r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s LEBinOp -- less equal <=
  = ( s, instrs )
    where
      instrs   = ( [ CMP   r $ Op2'Reg r'   ]
      	       ++  [ MOVLE r $ Op2'ImmVal 1 ]
               ++  [ MOVGT r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s GEBinOp -- greater equal >=
  = ( s, instrs )
    where
      instrs   = ( [ CMP   r $ Op2'Reg r'   ]
      	        ++ [ MOVGE r $ Op2'ImmVal 1 ]
               ++  [ MOVLT r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s EqBinOp -- equal ==
  = ( s, instrs )
    where
      instrs   = ( [ CMP   r $ Op2'Reg r'   ]
      	       ++  [ MOVEQ r $ Op2'ImmVal 1 ]
               ++  [ MOVNE r $ Op2'ImmVal 0 ] )
      (r:r':_) = freeRegs s

transBinOp s NEBinOp -- not equal !=
  = ( s, instrs )
    where
      instrs   = ( [ CMP   r $ Op2'Reg r'   ]
      	       ++  [ MOVNE r $ Op2'ImmVal 1 ]
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
      (s', exprInstr) = transExpr s { freeRegs = nxt:regs } (expr, it) 
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
  (arm' { freeRegs = r } , exprInstr ++ storeInstr)
    where
      r@(dst:nxt:regs) = freeRegs arm 

      -- Translate the expression 
      (arm', exprInstr) = transExpr arm{freeRegs = nxt:regs} (e, it)
      -- At what index in the heap the elem is
      -- +4 Beause the first 4 bytes are taken up by the length
      offset = (index * (sizeOf e it)) + 4 -- MAGIC NUMBER NOOO 
      storeInstr = if typeOf e it == BoolType  -- TODO func for CHAR 
                      then [STRB'Off nxt dst offset]
                      else [STR'Off  nxt dst offset]  


