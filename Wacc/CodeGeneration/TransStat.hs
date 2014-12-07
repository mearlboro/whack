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


-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Statement Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- Generate instructions for a statement
transStat :: Assembler Stat

-- No much we can do with a skip statement
transStat s SkipStat = (s, [])


---- TODO: to implement arrays, pairs (printing an address)
transStat s (FreeStat e it) 
  = (s'', freeI)
    where
      -- First translates the expressions
      (s', exprI) = transExpr s (e, it)

      -- Then sets the instructions for free
      freeI       = exprI ++ [ MOV'Reg R0 dst, BL (JumpLabel label) ]
      label       = case typeOf e it of 
                      ArrayType {} -> "p_free_array"
                      PairType  {} -> "p_free_pair"
      (dst:_)     = freeRegs s

      -- Also update the state 
      -- Labels depend on the type of the expression to be freed
      s'' = case typeOf e it of 
           ArrayType {} -> stateAddFreeArr  s'  
           PairType  {} -> stateAddFreePair s' 

      ls   = dataLabels   s'
      ps   = predefLabels s'


-- To exit we load the value of the expression into the first available reg
-- and then move its value into register 0. then call BL exit  
transStat s (ExitStat e it) 
  = (s', exitI)
    where
      -- Obtain the register that the expression value will be saved into
      (dst:_) = freeRegs s
      -- Translate the expression to exit
      (s', exprI) = transExpr s (e, it)
      -- Instructions for the exit statement
      exitI         
        =  exprI 
        ++ [ MOV R0 $ Op2'Reg dst  ] -- TODO: Comment
        ++ [ BL (JumpLabel "exit") ] -- TODO: Comment

-- 
-- 
transStat s (ReturnStat e it) = (s', returnI)
  where
    -- Obtain the register that the expression value will be saved into
    (dst:_)  =  freeRegs s
    -- Translate the expression to return
    (s', exprI) = transExpr s (e, it) 
    -- Move result of expression from dst into return register R0
    returnI = exprI ++ [ MOV'Reg R0 dst ] 


transStat s (PrintStat e it) 
  = ( s'', instrs')
    where
        -- First gets the instructions for the expr, then adds the print
        instrs'      = instrs ++ [ MOV'Reg R0 dst ] ++ [ BL $ JumpLabel label ]
        (s', instrs) = transExpr s (e, it)  
        (dst:_)      = freeRegs s

        -- The new state just updates the data labels
        s''          = stateAddPrint s' label 

        -- The print label/function called depends on the type
        label        = case typeOf e it of
                          IntType    -> "p_print_int"    
                          BoolType   -> "p_print_bool"   
                          CharType   -> "putchar"        
                          StringType -> "p_print_string"   
                          _          -> "p_print_reference" 


--
transStat s (PrintlnStat e it)
  = (s'', instrs')
    where
      -- The println instruction will be added to the set of print instrs
      instrs'      = instrs ++ [ BL ( JumpLabel label ) ]
      -- Println is a print, so do print first
      (s', instrs) = transStat s (PrintStat e it) 

      -- The new state will get the print and println labels
      s''     = stateAddPrint s' "p_print_ln" 

      label   = "p_print_ln"
 
-- 
transStat s (ScopedStat stat) = transScoped s stat

--
transStat s (ReadStat (LhsIdent id) it) 
  = (s', readI)
    where
      readL = case typeOf (IdentExpr id) it of 
                IntType  -> "p_read_int"
                CharType -> "p_read_char"
  
      dst = head (freeRegs s)
      (src, off) = lookupLoc s id  
      readI = 
        [ ADD dst src $ Op2'ImmVal off 
        , MOV'Reg R0 dst 
        , BL (JumpLabel readL) ]
  
      s' = stateAddRead s readL

--
transStat s (ReadStat (LhsPairElem pairE) it) 
  = (s'', readI)
    where
      readL = case typeOf pairE it of 
                IntType  -> "p_read_int"
                CharType -> "p_read_char"
  
      dst         = head (freeRegs s)
      (s', pairI) = transPairElem s (pairE, it)
      readI       =  
        pairI ++ 
        [ MOV'Reg R0 dst 
        , BL (JumpLabel readL) ]
  
      s'' = stateAddRead s' readL

--
transStat s (ReadStat (LhsArrayElem arrayE) it) 
  = (s'', readI)
    where
      readL = case typeOf arrayE it of 
                  IntType  -> "p_read_int"
                  CharType -> "p_read_char"
    
      dst          = head (freeRegs s)
      (s', arrayI) = transExpr s (ArrayElemExpr arrayE, it)
      readI       =  
        arrayI ++ 
        [ MOV'Reg R0 dst 
        , BL (JumpLabel readL) ]
  
      s'' = stateAddRead s' readL


-- 
transStat s (WhileStat cond body it) = (s''', whileI)
    where
      -- Obtain the next free label
      currL = numJumpLabels s
      -- We need two labels: one for the condition
      condL = "while_cond_" ++ show currL 
      -- And one for the body
      bodyL = "while_body_" ++ show (currL + 1)
      -- Obtain the register that the cond expression value will be saved into
      dst = head (freeRegs s)
      -- We have used up 2 labels so we need to update the state
      s' = s { numJumpLabels = currL + 2 }
      -- Now let's translate the loop condition expression with the new state
      (s'', condI) = transExpr s' (cond, it) 
      -- Now for the body statements, which are scoped           
      (s''', bodyI) = transScoped s'' body
      -- Now concatenate everything together
      whileI =  
        [ B (JumpLabel condL)      -- TODO: Comment        
        , DEFINE (JumpLabel $ bodyL ++ ":") -- TODO: Comment       
        ] ++ bodyI ++              
        [ DEFINE (JumpLabel $ condL ++ ":") -- TODO: Comment       
        ] ++ condI ++            
        [ CMP dst $ Op2'ImmVal 0   
        , BEQ (JumpLabel bodyL) ]  -- TODO: Comment  


-- mapAccumL for the win
transStat s (SeqStat x y) = (s', concat iss)
  where 
    (s', iss) = mapAccumL transStat s [x, y] 


--
transStat s (DeclareStat vtype vname rhs it) = (s''', declareI)
  where
    -- Obtain the register that the rhs value will be saved into
    dst = head (freeRegs s)
    -- Translate right hand side
    (s', rhsI) = transRhs s (rhs, it) 
    -- How many bytes does this variable take up?
    size = sizeOf vtype it 
    -- Work out the stack offset for this variable
    offset = stackOffset s' - size -- TODO: check s == s' here
    -- Update the stack offset for the next declaration
    s'' = s' { stackOffset = offset }
    -- Now remember the location of vname in memory
    s''' = insertLoc s'' vname (SP, offset)  -- TODO: check s == s'' here
    -- We now need to push the value in dst reg onto the stack
    declareI = rhsI ++ [ strVar dst SP size offset ]  


-- We are assigning a variable to a new value
transStat s (AssignStat (LhsIdent id) rhs it) = (s', assignI)
  where
    -- TODO
    size = sizeOf (IdentExpr id) it 
    -- Obtain the register that the rhs value will be saved into
    dst = head (freeRegs s)
    -- Generate instructions for the right hand side
    (s', rhsI) = transRhs s (rhs, it) 
    -- Figure out where the variable is in memory
    (src, off) = lookupLoc s' id  -- TODO check s == s' 
    -- TODO comment
    assignI = rhsI ++ [ strVar dst src size off ]


-- We are assigning the first or second element of a pair to a new value
transStat s (AssignStat (LhsPairElem pelem) rhs it) 
  = (s'', pelemInstr)
    where
      -- Obtain the free registers
      (dst:nxt:_) = freeRegs s 
      -- 
      (s', rhsInstrs) = transRhs s (rhs, it)
 
      (src, off) = fromJust (Map.lookup (getElemId pelem) (memoryMap s'))
  
      pelemType  = typeOf pelem it
      elemSize   = sizeOf pelemType it
      elemOff    = if (pelem ~== Fst {}) then 0 else 4

      pelemInstr 
        =  rhsInstrs 
        ++ [ ldrVar nxt src 4 off ] 
        ++ [ MOV'Reg R0 nxt ] -- check null
        ++ [ BL (JumpLabel "p_check_null_pointer") ]
        ++ [ ldrVar nxt nxt 4  elemOff ]
        ++ [ strVar dst nxt elemSize 0 ]
  
      -- Helper functions
      getElemId :: PairElem -> IdentName
      getElemId (Fst e) = fromJust $ getIdent e
      getElemId (Snd e) = fromJust $ getIdent e
  
      getIdent                                  :: Expr -> Maybe IdentName
      getIdent ( IdentExpr       ident       )  =  Just ident
      getIdent ( ArrayElemExpr ( ident , _ ) )  =  Just ident
      getIdent                           _      =  Nothing

      -- Now update the state with the labels and functions for checking null pointer error
      s'' = stateAddCheckNullPtr s'


transStat s (AssignStat (LhsArrayElem (id, exprs)) rhs it) = (s'', assignI)
  where
    (dst:nxt:rs) = freeRegs s 
    (s', rhsI)   = transRhs s (rhs, it)
    --(src, off)  = lookupLoc s id 
    size         = sizeOf rhs it 
    --arrelemI     = [ ADD dst src off ] ++ 
    (s'', elemI) = transExpr s' { freeRegs=nxt:rs } (ArrayElemExpr (id, exprs), it)
--    (s'', elemI) = transExpr s' {freeRegs = nxt:rs }  (ArrayElemExpr (id, exprs))
  
    -- We use init to remove the last load from transAraryElem
    assignI      = rhsI ++ (init elemI) ++ [ strVar dst nxt size 0 ]


-- 
transStat s (IfStat cond thens elses it) = (s'''', ifI)
  where
    -- Obtain next free label
    currL = numJumpLabels s
    -- We need 2 labels: one for the else branch
    elseL = "if_else_" ++ show currL 
    -- And one for after the if
    endifL = "if_end_" ++ show (currL + 1)
    -- Obtain the register that the cond expression value will be saved into
    dst = head (freeRegs s)
    -- We have used up 2 labels so we need to update the arm state
    s' = s { numJumpLabels = currL + 2 }
    -- Now let's translate the if condition expression
    (s'', condI) = transExpr s' (cond, it)
    -- Now for the then end else branch, in its own scope
    (s''', thenI) = transScoped s'' thens 
    -- Now for the else branch, also in its own scope
    (s'''', elseI) = transScoped s''' elses 
    -- Now concatenate everything together
    ifI =  
      condI ++                          
      [ CMP dst $ Op2'ImmVal 0      -- If condition false?
      , BEQ (JumpLabel elseL)       -- If so go to else
      ] ++ thenI ++ 
      [ B (JumpLabel endifL) 
      , DEFINE (JumpLabel $ elseL ++ ":")         
      ] ++ elseI ++ 
      [ DEFINE (JumpLabel $ endifL ++ ":") ]



-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************       RHS Translation      *********************** --
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
        s''         = stateAddCheckNullPtr s'

        dst = head (freeRegs s)
        (s', exprI) = transExpr s (e, it)
        size = sizeOf pairE it 
        pelemI = 
          exprI ++
          [ MOV'Reg R0 dst 
          , BL (JumpLabel "p_check_null_pointer") 
          , ldrVar dst dst size off ] 

--
transRhs :: Assembler (AssignRhs, It)

--
transRhs s (RhsExpr e, it) = transExpr s (e, it)  

--
transRhs s (RhsPairElem pairE, it) 
  = (s', rhsI)
    where
      dst = head (freeRegs s)
      (s', pelemI) = transPairElem s (pairE, it)
      size = sizeOf pairE it 
      rhsI = pelemI ++ [ ldrVar dst dst size 0 ]

-- 
transRhs arm (RhsArrayLiter exprs, it) 
  = (arm', rhsInstr)
    where
      rhsInstr = malloc ++ exprsInstr ++ lengthInstr 
      arrayLength = length exprs
      arraySize   = sum (map (flip sizeOf it) exprs) + sizeOf (ArrayType {}) it 
      -- Allocate memeory on the heap
      malloc = [ LDR R0 arraySize        ] ++ 
               [ BL (JumpLabel "malloc") ] ++
               [ MOV dst (Op2'Reg R0)    ]
      (dst:nxt:_) = freeRegs arm
      -- Add to heap each elem of the array 
      (arm' , exprsInstr) = transArrayLitExpr arm (exprs, it) 
      -- Add the length of the array to the heap  
      --offset = stackOffset arm' - sizeOf (ArrayType {})
      lengthInstr = [ LDR nxt arrayLength   ] ++ 
                    [ STR'Reg nxt dst       ] -- ++ 

-- 
transRhs s (RhsNewPair fstExpr sndExpr, it) 
  = (s'', newPairInstrs )
    where
      pairAdrSize = 8
  
      (dst:nxt:_) = freeRegs s
  
      (s',  fstInstrs) = transPairElemExpr s  (fstExpr, it, 0)
      (s'', sndInstrs) = transPairElemExpr s' (sndExpr, it, 1)
  
      -- To store the address of the pair onto the stack
      offset = stackOffset s'' 
  
      -- Reserved 8 bytes for the 2 addresses
      newPairInstrs 
        =  [ LDR R0 pairAdrSize      ] 
        ++ [ BL (JumpLabel "malloc") ] 
        ++ [ MOV'Reg dst R0          ]
        ++ fstInstrs
        ++ sndInstrs
        -- TODO magic pair

-- 
transRhs s (RhsCall fname params, it) = (s', callInstrs)
  where
    -- mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    -- x = Expr 
    --transFuncts :: -> ParamList -> (ArmState, [[Instr]])
    (dst:_) = freeRegs s
    -- Expression instructtion
    (s', instrs) = mapAccumR (\s p -> transExpr s (p, it)) s (reverse params)
    -- Generate instructions to push params on the stack

    pushArg e = let s = sizeOf e it in [[ strArg dst SP s (-s) ]]

    pushArgs = map pushArg params 

    paramInstrs = (concat . concat) (zipWith (:) instrs pushArgs)

    totSize = sum (map (flip sizeOf it) params)
  
    callInstrs 
      = paramInstrs 
      ++ [ BL (JumpLabel ("f_" ++ fname)) ]
      ++ (if totSize == 0 then [] else [ ADD SP SP $ Op2'ImmVal totSize ]) 
      ++ [ MOV'Reg dst R0 ] -- TODO


-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     Statement Helpers      *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- | 
transScoped          :: Assembler Stat
transScoped arm stat = (arm'''', editStack SUB ++ statInstr ++ editStack ADD)
      where
        -- Make room on the stack or free the stack
        editStack :: (Rd -> Rn -> Operand2 -> Instr) -> [ Instr ]
        editStack mne = 
                 take chunks (cycle [ mne SP SP $ Op2'ImmVal 1024 ]) ++
          if left == 0 then [] else [ mne SP SP $ Op2'ImmVal left ] 

        -- This is the old map
        oldMap = memoryMap arm

        oldOffset = stackOffset arm 
        -- Update stack offset
        arm' = arm { stackOffset = oldOffset + bytesNeeded }
        -- Comment later
        --allah = getBytesNeeded' stat 
        -- Update variables in map, change the map
        arm'' = arm' { memoryMap = Map.map (\(r, o) -> (r, o+bytesNeeded)) oldMap }
        -- Translate the statemente after reserving space on the stack
        (arm''', statInstr) = transStat arm'' stat 
        -- Restore stack offset
        arm'''' = arm''' { memoryMap = oldMap }

        -- Work out how many variables are declared in the scope of this 
        -- statement.
        bytesNeeded  =  getBytesNeeded stat 
        -- Can only add and sub from sp in chunks of 1024
        (chunks, left) = divMod bytesNeeded 1024 

