module Wacc.CodeGeneration.TransProgram 
( assembleProgram
, evaluateProgram
) where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.CodeGeneration.TransCommon
import Wacc.CodeGeneration.TransStat
import Wacc.CodeGeneration.TransExpr

import Wacc.Data.DataTypes

import qualified Data.Map  as Map (Map, empty, insert)
import           Data.List        (mapAccumL)

import Debug.Trace 

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     Program Translation    *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- Assembles an *augmented* Program AST into assembly instructions
assembleProgram  :: Program -> [ Instr ]
assembleProgram  =  snd . evaluateProgram 

-- Same as assembleProgram, but also returns the final state
evaluateProgram  :: Program -> (ArmState, [ Instr ])
evaluateProgram  =  transProgram s  
  where
    -- The initial state
    s = ArmState 
      { memoryMap     = Map.empty
      , stackOffset   = 0 
      , freeRegs      = [ R4 .. R10 ]
      , numJumpLabels = 0
      , dataLabels    = [] 
      , predefLabels  = []
      , memoryUsed    = 0 
      , hasReturned   = False }

-- Assembles an *augmented* Program AST given an initial state
transProgram                         :: Assembler Program
transProgram s (Program funcs body)  =  (s'', progI)
  where
    -- Translate each function 
    (s', funcsI) = mapAccumL transFunc s funcs 

    -- Main body is executed in its own scope
    (s'', bodyI) = transScoped s' body 

    -- The whole program translated to assembly
    progI = 
        concat funcsI ++             -- mapAccumL returns list of lists, hence concat
      [ DEFINE ( JumpLabel "main:" ) -- Define main function label
      , PUSH [ LR ]                  -- Pushes the current return address onto the stack
      ] ++ bodyI ++                  -- TODO: Comment
      [ LDR R0 0                     -- TODO: Comment
      , POP  [ PC ]                  -- TODO: Comment
      , INDIR Ltorg ]                -- TODO: Comment

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Function Translation    *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transFunc :: Assembler Func
transFunc s (Func ftype fname params body it)  =  (s'', funcI)
  where 
    -- Save the current memory map
    oldMap = memoryMap s 
 
    -- Produce a label for this function
    funcL = JumpLabel ("f_" ++ fname ++ ":")

    bytesNeeded = getBytesNeeded body 

    ---- Insert into the memory map m the function parameter variables ps
    ---- The location of each parameter on the stack is calculated as the size of 
    ---- the type of the parameter plus the previous parameters' summed offsets
    ----putParams []     _   m _       = m 
    ----putParams (p:ps) off m isFirst = putParams ps off' m' False
    ----  where
    ----    off' = if isFirst then off else off + sizeOf (ptypeOf p) it 
    ----    m'   = Map.insert (pnameOf p) (SP, off') m

    ------ Add the function parameters to the memory map
    --putParams []        _    m = m 
    --putParams [p]       _    m = putParam p (bytesNeeded+4) m
    --putParams (p:p':ps) off  m = 
    putParam m (p, off) = Map.insert (pnameOf p) (SP, off) m

    -- 
    offsets = (scanl (+) (4)) (map (flip sizeOf it . ptypeOf) (init params))

    newMap = foldl putParam oldMap (zip params (offsets))

    --newMap = putParams params (bytesNeeded+4) oldMap True

    -- Translate the function body in its own scope using the new map
    (s', bodyI) = transScoped s { memoryMap = newMap } body

    -- Restore the previous memory map
    s'' = s' { memoryMap = oldMap }

    -- The instructions generated for this function
    funcI =  
      [ DEFINE funcL  -- Define label with unique function name 
      , PUSH [ LR ]   -- Pushes current return address onto stack                       
      ] ++ bodyI ++   -- The instructions from the func body
      [ POP  [ PC ]   -- Restore program counter from the stack
      , INDIR Ltorg ] -- TODO: Comment           

-- ************************************************************************** --

{-
-- This demonstrates just how awesome mapAccumL is \\GOTO: this.line(44)
transFuncs :: [ Func ] -> ArmState -> (ArmState, [Instr])
transFuncs fs arm = transFuncs' fs (arm, [])
  where
    transFuncs' :: [ Func ] -> (ArmState, [Instr]) -> (ArmState, [ Instr ])
    transFuncs' []      result   = result
    transFuncs' (f:fs) (arm, is) = (arm'', is ++ is'')
      where
        s@(arm', _is') = transFunc  f arm
        (arm'', is'') = transFuncs' fs s
-}


-- Old paramenter insertion   
--insertParam ((Param _ pname), offset) = Map.insert pname (SP, offset)
--offsets = tail (scanl (+) 0 sizes)
--sizes = map (sizeOf . ptypeOf) args

--finalMap = foldl (flip insertParam) map'old (zip args offsets)

