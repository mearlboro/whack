module Wacc.CodeGeneration.TransStat 
( transStat
, transScoped
) where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.CodeGeneration.TransExpr
import Wacc.CodeGeneration.TransCommon

import Wacc.Data.DataTypes

import qualified Data.Map   as Map (Map, insert, lookup, map)
import           Data.Maybe        (fromJust)
import           Data.List         (mapAccumL, mapAccumR) 
import           Data.Tuple        (swap)
import           Debug.Trace

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Statement Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transStat :: Assembler Stat
--------------------------------------------------------------------------------
transStat s SkipStat = (s, [])
--------------------------------------------------------------------------------
transStat s (FreeStat e it) = (s'', freeI)
  where
    (dst:_)     = freeRegs s  
    (s', exprI) = transExpr s (e, it) 

    -- Obtain predefined label depending on expr type and update the labels 
    (s'', label) = case typeOf e it of 
      ArrayType {} -> (stateAddFreeArr  s', "p_free_array")
      PairType  {} -> (stateAddFreePair s', "p_free_pair" )
    
    freeI = exprI ++ [ MOV'Reg R0 dst, BL (JumpLabel label) ]
--------------------------------------------------------------------------------
transStat s (ExitStat e it) = (s', exitI)
  where
    (dst:_)     = freeRegs s   
    (s', exprI) = transExpr s (e, it)

    exitI = exprI ++ [ MOV'Reg R0 dst, BL (JumpLabel "exit") ] 
--------------------------------------------------------------------------------
transStat s (ReturnStat e it) = (s'', returnI)
  where
    (dst:_)     = freeRegs s   
    (s', exprI) = transExpr s (e, it)

    -- We need to keep track of when we have reached a return statement
    s'' = s' { hasReturned = True }

    returnI 
      =  exprI     
      -- Move result of expression from dst into return register R0                      
      ++ [ MOV'Reg R0 dst ] 
      -- Add to stack pointer as many bytes as have been allocated so far           
      ++ (addVar SP SP $ memoryUsed s'') 
      ++ [ POP [ PC ] ]
--------------------------------------------------------------------------------       
transStat s (PrintStat e it) = ( s'', printI)
  where
    (dst:_)     = freeRegs s
    (s', exprI) = transExpr s (e, it)  

    label = case typeOf e it of
      IntType            -> "p_print_int"    
      BoolType           -> "p_print_bool"   
      CharType           -> "putchar"        
      StringType         -> "p_print_string"   
      ArrayType CharType -> "p_print_string"
      _                  -> "p_print_reference" 

    -- The new state just updates the data labels
    s'' = stateAddPrint s' label 

    printI = exprI ++ [ MOV'Reg R0 dst, BL (JumpLabel label) ]
--------------------------------------------------------------------------------
transStat s (PrintlnStat e it) = (s'', printlnI)
  where
    label = "p_print_ln"

    -- The new state will get the print and println labels
    s'' = stateAddPrint s' label

    -- Println is a print, so do print first
    (s', printI) = transStat s (PrintStat e it) 
    printlnI     = printI ++ [ BL (JumpLabel label) ]
--------------------------------------------------------------------------------
transStat s (ScopedStat stat) = transScoped s stat
--------------------------------------------------------------------------------
transStat s (ReadStat lhs it) = (s'', readI)
  where
    (dst:_)    = freeRegs s
    (s', lhsI) = transLhs s (lhs, it) 

    readL = case typeOf lhs it of 
      IntType  -> "p_read_int"
      CharType -> "p_read_char"

    s'' = stateAddRead s' readL
  
    readI = lhsI ++ [ MOV'Reg R0 dst, BL (JumpLabel readL) ]
--------------------------------------------------------------------------------
transStat s (WhileStat cond body it) = (s''', whileI)
  where
    (dst:_) = freeRegs s

    currL = numJumpLabels s
    condL = "while_cond_" ++ show  currL 
    bodyL = "while_body_" ++ show (currL + 1)

    s' = s { numJumpLabels = currL + 2 } -- Two labels have been used

    (s'',  condI) = transExpr   s' (cond, it) 
    (s''', bodyI) = transScoped s'' body

    whileI =  
      [ B (JumpLabel condL)            
      , DEFINE (JumpLabel $ bodyL ++ ":")        
      ] ++ bodyI ++              
      [ DEFINE (JumpLabel $ condL ++ ":")        
      ] ++ condI ++            
      [ CMP dst $ Op2'ImmVal 1   
      , BEQ (JumpLabel bodyL) ] 
--------------------------------------------------------------------------------
transStat s (SeqStat fst snd) = (s''', seqI)
  where 
    availRegs    = freeRegs s                   -- Save available registers
    (s',  fstI)  = transStat s fst              -- Translate first statement
    s''          = s' { freeRegs = availRegs }  -- Restore saved registers
    (s''', sndI) = transStat s'' snd            -- Translate second satement
    seqI         = fstI ++ sndI
--------------------------------------------------------------------------------
transStat s (DeclareStat vtype vname rhs it) = (s''', declareI)
  where
    (dst:_)    = freeRegs s
    (s', rhsI) = transRhs s (rhs, it) 

    size   = sizeOf vtype it       -- How many bytes does this variable take up?
    offset = stackOffset s' - size -- Work out the stack offset for this var 

    s''  = s' { stackOffset = offset }      -- Update the stack offset for the next declaration
    s''' = insertLoc s'' vname (SP, offset) -- Now remember the location of vname in memory
    
    -- We now need to push the value in dst reg onto the stack
    declareI = rhsI ++ [ strVar dst SP size offset ]  
--------------------------------------------------------------------------------
transStat s (AssignStat (LhsIdent id) rhs it) = (s', assignI)
  where
    (dst:_)    = freeRegs s
    (s', rhsI) = transRhs s (rhs, it) 

    size       = sizeOf (IdentExpr id) it 
    (src, off) = lookupLoc s' id -- Figure out where the variable is in memory

    assignI = rhsI ++ [ strVar dst src size off ]
--------------------------------------------------------------------------------
transStat s (AssignStat (LhsPairElem pelem) rhs it) = (s'', pelemI)
  where
    (dst:nxt:_) = freeRegs s 
    (s', rhsI)  = transRhs s (rhs, it)

    (src, off) = lookupLoc s' (extractId pelem)

    magic4  = 4
    size    = sizeOf pelem it
    elemOff = if (pelem ~== Fst {}) then 0 else 4

    s'' = stateAddCheckNullPtr s'

    pelemI =
      rhsI ++
      [ ldrVar nxt src magic4 off 
      , MOV'Reg R0 nxt  
      , BL (JumpLabel "p_check_null_pointer") 
      , ldrVar nxt nxt magic4 elemOff 
      , strVar dst nxt size 0 ]
--------------------------------------------------------------------------------
transStat s (AssignStat (LhsArrayElem (id, exprs)) rhs it) = (s'''', assignI)
  where
    (dst:nxt:rs) = freeRegs s 
    (s', rhsI)   = transRhs s (rhs, it)

    size = sizeOf rhs it 

    s''           = s' { freeRegs = nxt:rs }
    (s''', elemI) = transExpr s'' (ArrayElemExpr (id, exprs), it)
    s''''         = s''' { freeRegs = dst:freeRegs s'' }

    -- We use init to remove the last load from transArrayElem
    assignI = rhsI ++ (init elemI) ++ [ strVar dst nxt size 0 ]
--------------------------------------------------------------------------------
transStat s (IfStat cond thens elses it) = (s'''', ifI)
  where
    (dst:_) = freeRegs s

    currL  = numJumpLabels s
    elseL  = "if_else_" ++ show currL 
    endifL = "if_end_"  ++ show (currL + 1)

    s' = s { numJumpLabels = currL + 2 } -- Two labels have been used

    (s'',   condI) = transExpr   s'  (cond, it)
    (s''',  thenI) = transScoped s''  thens 
    (s'''', elseI) = transScoped s''' elses 

    ifI =  
      condI ++                          
      [ CMP dst $ Op2'ImmVal 0     
      , BEQ (JumpLabel elseL)     
      ] ++ thenI ++ 
      [ B (JumpLabel endifL) 
      , DEFINE (JumpLabel $ elseL ++ ":")         
      ] ++ elseI ++ 
      [ DEFINE (JumpLabel $ endifL ++ ":") ]

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************       LHS Translation      *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transLhs :: Assembler (AssignLhs, It)
--------------------------------------------------------------------------------
transLhs s (LhsIdent id, it) = (s, lhsI)
  where
    (dst:_)    = freeRegs s
    (src, off) = lookupLoc s id -- Find location of identifier 

    lhsI       = [ ADD dst src $ Op2'ImmVal off ]
--------------------------------------------------------------------------------
transLhs s (LhsPairElem pairE, it) = transPairElem s (pairE, it)
--------------------------------------------------------------------------------
transLhs s (LhsArrayElem arrayE, it) = transExpr s (ArrayElemExpr arrayE, it)

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************       RHS Translation      *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transRhs :: Assembler (AssignRhs, It)
--------------------------------------------------------------------------------
transRhs s (RhsExpr e, it) = transExpr s (e, it)  
--------------------------------------------------------------------------------
transRhs s (RhsPairElem pairE, it) = (s', rhsI)
  where
    (dst:_)      = freeRegs s
    (s', pelemI) = transPairElem s (pairE, it)

    rhsI = pelemI ++ [ ldrVar dst dst (sizeOf pairE it) 0 ]
--------------------------------------------------------------------------------
transRhs s (RhsArrayLiter elems, it) = (s', rhsI)
  where
    (dst:nxt:_)  = freeRegs s
    (s', elemsI) = transArrayLitExpr s (elems, it) 

    arrLen  = length elems
    arrSize = sum (map (flip sizeOf it) elems) + sizeOf (ArrayType {}) it 

    rhsI = 
      -- Allocate memeory on the heap
      [ LDR R0 arrSize          
      , BL (JumpLabel "malloc") 
      , MOV'Reg dst R0 ]        ++ 
      -- Array elements 
      elemsI                    ++ 
      -- Length instructions 
      [ LDR nxt arrLen          
      , STR'Reg nxt dst ]
--------------------------------------------------------------------------------
transRhs s (RhsNewPair fstE sndE, it) = (s'', newpairI)
  where
    (dst:nxt:_) = freeRegs s
    (s',  fstI) = transPairElemExpr s  (fstE, it, 0)
    (s'', sndI) = transPairElemExpr s' (sndE, it, 1)
    
    size = 2 * sizeOf (PairType {}) it -- * 8

    newpairI =  
      [ LDR R0 size             
      , BL (JumpLabel "malloc") 
      , MOV'Reg dst R0 ]        ++
      fstI                      ++
      sndI
--------------------------------------------------------------------------------
transRhs s (RhsCall fname params, it) = (s'', callI)
  where
    (dst:_)       = freeRegs s 
    (s', paramsI) = mapAccumL transParam s (reverse params) -- transParams s (reverse params)

    oldMap = memoryMap s 
    s''    = s' { memoryMap = oldMap }

    totSize = sum (map (flip sizeOf it) params)

    callI =  
      concat paramsI                     ++
      [ BL (JumpLabel $ "f_" ++ fname) ] ++
      addVar SP SP totSize               ++
      [ MOV'Reg dst R0 ] 

    transParam :: ArmState -> Expr -> (ArmState, [ Instr ])
    transParam s e = (s'', paramI)
      where
        (s', eI) = transExpr s (e, it)
        eSize    = sizeOf e it 
        paramI   = eI ++ [ strArg dst SP eSize (-eSize) ]
        s''      = s' { memoryMap = Map.map (fmap (+eSize)) (memoryMap s') } 

    --transParams           :: ArmState -> [ Expr ] -> (ArmState, [ Instr ])
    --transParams s    []   =  (s, [])
    --transParams s (p:ps)  =  (s''', pI ++ strI ++ psI)
    --  where
    --    (s', pI) = transExpr s (p, it)
    --    pSize    = sizeOf p it 
    --    strI     = [ strArg dst SP pSize (-pSize) ]
    --    s''      = s' { memoryMap =  updateOff (memoryMap s') }
    --    (s''', psI) = transParam s'' ps  

    --    updateOff = Map.map (\(r, off) -> (r, off+pSize))  

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    PairElem Translation    *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transPairElem                :: Assembler (PairElem, It)
transPairElem s (pairE, it)  = 
  case pairE of 
    Fst e -> transPairElemExpr e 0
    Snd e -> transPairElemExpr e 4
  where 
    transPairElemExpr        :: Expr -> Int -> (ArmState, [ Instr ])
    transPairElemExpr e off  =  (s'', pelemI)
      where
        (dst:_)     = freeRegs s
        (s', exprI) = transExpr s (e, it)

        s'' = stateAddCheckNullPtr s'

        pelemI = 
          exprI ++
          [ MOV'Reg R0 dst 
          , BL (JumpLabel "p_check_null_pointer") 
          , ldrVar dst dst 4 off ] 

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     Scoped Translation     *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transScoped        :: Assembler Stat
transScoped s stat = (s''', scopedI)
  where
    -- Make room on the stack or free the stack
    editStack :: (Rd -> Rn -> Operand2 -> Instr) -> [ Instr ]
    editStack mne = 
             take chunks (cycle [ mne SP SP $ Op2'ImmVal 1024 ]) ++
      if left == 0 then [] else [ mne SP SP $ Op2'ImmVal left ] 

    bytes          = getBytesNeeded stat -- Work out how many variables are declared in the scope of this stat
    (chunks, left) = divMod bytes 1024   -- Can only add and sub from sp in chunks of 1024

    -- Update stack offset
    s' = s { 
      stackOffset = bytes + stackOffset s,
      memoryMap   = Map.map (fmap (+bytes)) (memoryMap s),
      memoryUsed  = bytes + memoryUsed s
    }

    -- Translate the statemente after reserving space on the stack
    (s'', statI) = transStat s' stat 

    scopedI = 
      editStack SUB ++
      statI         ++
      if hasReturned s'' then [] else editStack ADD 

    -- Restore stack offset
    s''' = s { 
      stackOffset = stackOffset s',
      hasReturned = False
    }
 

