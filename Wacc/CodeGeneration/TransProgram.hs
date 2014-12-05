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
      , predefLabels  = [] }

-- Assembles an *augmented* Program AST given an initial state
transProgram                         :: Assembler Program
transProgram s (Program funcs body)  =  (s'', prog'is)
  where
    -- Translate each function 
    (s', funcs'iss) = mapAccumL transFunc s funcs 

    -- Main body is executed in its own scope
    (s'', body'is) = transScoped s' body 

    -- The whole program translated to assembly
    prog'is = 
        concat funcs'iss ++          -- mapAccumL returns list of lists, hence concat
      [ DEFINE ( JumpLabel "main:" ) -- Define main function label
      , PUSH [ LR ]                  -- Pushes the current return address onto the stack
      ] ++ body'is ++                -- TODO: Comment
      [ LDR R0 0                     -- TODO: Comment
      , POP  [ PC ]                  -- TODO: Comment
      , INDIR Ltorg ]                -- TODO: Comment

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Function Translation    *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transFunc :: Assembler Func
transFunc s (Func ftype fname params body it)  =  (s'', func'is)
  where 
    -- Save the current memory map
    map'old = memoryMap s 

    -- Produce a label for this function
    label = JumpLabel ("f_" ++ fname ++ ":")

    -- Insert into the memory map m the function parameter variables ps
    -- The location of each parameter on the stack is calculated as the size of 
    -- the type of the parameter plus the previous parameters' summed offsets
    putParams []     _   m = m 
    putParams (p:ps) off m = putParams ps off' m'
      where
        off' = off + sizeOf (ptypeOf p)  
        m'   = Map.insert (pnameOf p) (SP, off') m

    -- Add the function parameters to the memory map
    map'new = putParams params 0 map'old 

    -- Translate the function body in its own scope using the new map
    (s', body'is) = transScoped s { memoryMap = map'new } body

    -- Restore the previous memory map
    s'' = s' { memoryMap = map'old }

    -- The instructions generated for this function
    func'is =  
      [ DEFINE label  -- Define label with unique function name 
      , PUSH [ LR ]   -- Pushes current return address onto stack                       
      ] ++ body'is ++ -- The instructions from the func body
      [ POP  [ PC ]   -- There is always a return statementa
      , POP  [ PC ]   -- Restore program counter from the stack
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

