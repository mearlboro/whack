module Wacc.CodeGeneration.WaccCodeGen where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.Data.DataTypes
import Wacc.Data.SymbolTable

import qualified Data.Map as Map
import Data.Array
import Data.List (intersperse)
import Data.Maybe
import Data.Char
import Debug.Trace

-- ************************************************************************** --
-- **************************                         *********************** --
-- **************************     Code generation     *********************** --
-- **************************                         *********************** -- 
-- ************************************************************************** --

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- The trans____ family of functions will get the AST of a statement, 
-- expression, or function, and an ARM state containing information about labels 
-- and registers. It will then return the set of instructions and the updated 
-- state wrapped in a tuple. 
-- trans____ 
--  :: *AST*         -- Func, Stat, Expr to be compiled
--  -> ARMState      -- Describing the state before transforming
--  -> ( ArmState  , -- Describing the new state 
--       [ Instr ] ) -- The instructions compiled out of the input
--
-- Properties of trans____ : normal, deterministic, total {I like this}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
transProgram 
  :: Program   -- The program Augmented AST
  -> (ArmState, [ Instr ]) -- The whole program translated into assembly

transProgram (Program funcs body)  =  (s'', progInstrs)
  where
    -- Translate each functions 
    (s', funcsInstrs)  =  transFuncs funcs state0

    -- Main body is executed in its own scope; discard final state
    (s'', bodyInstrs)  =  transScoped body s'

    -- The whole program translated to assembly
    progInstrs 
      =  funcsInstrs
      ++ [ DEFINE ( JumpLabel "main:" ) ]  -- Define main function label
      ++ [ PUSH [ LR ]                  ]  -- Pushes the current return address onto the stack
      ++ bodyInstrs                        -- TODO: Comment
      ++ [ LDR R0 0                     ]  -- TODO: Comment
      ++ [ POP  [ PC ]                  ]  -- TODO: Comment
      ++ [ INDIR Ltorg                  ]  -- TODO: Comment

    -- The initial ArmState
    state0 
      = ArmState 
      { stackMap      = Map.empty
      , stackOffset   = 0 
      , availableRegs = [R4 .. R10]
      , numJumpLabels = 0
      , dataLabels    = [] 
      , predefLabels  = [] }


-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     Function Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transFunc :: Func -> ArmState -> (ArmState, [Instr])
transFunc (Func ftype fname args body it) s  =  (s', functInstrs)
  where 
    -- Translate the function body in its own scope
    (s', bodyInstrs)  =  transScoped body s

    -- The whole function translated to assembly
    functInstrs       
      =  [ DEFINE ( JumpLabel fname ) ]  -- Define label with unique function name 
      ++ [ PUSH [ LR ]                ]  -- Pushes current return address onto stack                       
      ++ bodyInstrs                      -- The instructions from the func body
      ++ [ POP  [ PC ]                ]  -- There is always a return statementa
      ++ [ POP  [ PC ]                ]  -- Restore program counter from the stack
      ++ [ INDIR Ltorg                ] 

-- This is horrible and can be made into a one-liner. 
-- Honor and Glory to whoever can do it
transFuncs :: [ Func ] -> ArmState -> (ArmState, [Instr])
transFuncs fs arm = transFuncs' fs (arm, [])
  where
    transFuncs' :: [ Func ] -> (ArmState, [Instr]) -> (ArmState, [ Instr ])
    transFuncs' []      result   = result
    transFuncs' (f:fs) (arm, is) = (arm'', is ++ is' ++ is'')
      where
        s@(arm', is') = transFunc  f arm
        (arm'', is'') = transFuncs' fs s


-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Statement Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transStat :: Stat -> ArmState -> ( ArmState, [ Instr ] )

--
transStat SkipStat s = (s, [])

-- 
transStat (FreeStat e _) s = error "FreeStat"

-- To exit we load the value of the expression into the first available reg
-- and then move its value into register 0. then call BL exit  
transStat (ExitStat expr _) s = (s', exitInstrs)
  where
    -- Obtain the register that the expression value will be saved into
    (dst:_)  =  availableRegs s
    -- Translate the expression to exit
    (s', exprInstrs)  =  transExpr expr s
    -- Instructions for the exit statement
    exitInstrs         
      =  exprInstrs 
      ++ [ MOV R0 $ Op2'Reg dst  ]
      ++ [ BL (JumpLabel "exit") ]


-- 
transStat (ReturnStat expr _) s = (s', retInstrs)
  where
    -- Obtain the register that the expression value will be saved into
    (dst:_)  =  availableRegs s
    -- Translate the expression to return
    (s', exprInstrs) = transExpr expr s 
    -- Move result of expression from dst into return register R0
    moveResult = [ MOV R0 $ Op2'Reg dst ] 
    -- Instructions for the return statement
    retInstrs = exprInstrs ++ moveResult


-- TODO: to implement arrays, pairs (printing an address)
transStat (PrintStat e it) s
  = ( s'', instrs')
    where
        -- The new state just updates the data labels
        s''          = s' { dataLabels = ls', predefLabels = ps' }


        -- First gets the instructions for the expr, then adds the print
        instrs'      = instrs ++ [ MOV'Reg R0 dst ] ++ label
        (s', instrs) = transExpr e s 
        (dst:_)      = availableRegs s'
        -- The print label/function called depends on the type
        label        = case typeOfExpr e it of
                          IntType    -> [ BL $ JumpLabel "p_print_int"    ]
                          BoolType   -> [ BL $ JumpLabel "p_print_bool"   ]
                          CharType   -> [ BL $ JumpLabel "putchar"        ]
                          StringType -> [ BL $ JumpLabel "p_print_string" ]  
                          _          -> error "unimplemented print function"


        ls  = dataLabels   s'
        ps  = predefLabels s'
        -- Updates the data and ppredef labels with the strings/instructions
        (ls', ps') = case typeOfExpr e it of
                       IntType    -> let ls''@(l:_)    = intDataLabels   ls      in 
                                     let ps''          = intPrintPredef  l    ps in
                                     (ls'', ps'')
                       BoolType   -> let ls''@(l:l':_) = boolDataLabels  ls      in
                                     let ps''          = boolPrintPredef l l' ps in
                                     (ls'', ps'')
                       StringType -> let ls''@(l:_)    = strDataLabels   ls      in
                                     let ps''          = strPrintPredef  l    ps in 
                                     (ls'', ps'')
                       _          -> (ls, ps)        

        -- Generates the proper data labels for each type of the print param
        intDataLabels  ls =           newDataLabel "%d"    ls 
        boolDataLabels ls = let ls' = newDataLabel "true"  ls in
                                      newDataLabel "false" ls' 
        strDataLabels  ls =           newDataLabel "%.*s"  ls 

--
transStat (PrintlnStat e it) s = error "PrintlnStat"

-- 
transStat (ScopedStat stat) s = transStat stat s

-- 
transStat (ReadStat lhs it) s = error "ReadStat" 

-- 
transStat (WhileStat cond body it) s = (s''', whileInstr)
    where
      -- Obtain next free label
      currLabel = numJumpLabels s
      -- We need 2 labels: one for the condition
      condLabel = "while_cond_" ++ show currLabel
      -- And one for the body
      bodyLabel = "while_body_" ++ show (currLabel + 1)
      -- Obtain the register that the cond expression value will be saved into
      (dst:_) = availableRegs s
      -- We have used up 2 labels so we need to update the state
      s' = s { numJumpLabels = currLabel + 2 }
      -- Now let's translate the loop condition expression with the new state
      (s'', condInstrs) = transExpr cond s' 
      -- Now for the body statements, which are scoped           
      (s''', bodyInstrs) = transScoped body s''
      -- Now concatenate everything together
      whileInstr
        =  [ B (JumpLabel condLabel)      ] -- TODO: Comment        
        ++ [ DEFINE (JumpLabel bodyLabel) ] -- TODO: Comment       
        ++ bodyInstrs                 
        ++ [ DEFINE (JumpLabel condLabel) ] -- TODO: Comment       
        ++ condInstrs               
        ++ [ CMP dst $ Op2'ImmVal 0       ] 
        ++ [ BEQ (JumpLabel bodyLabel)    ] -- TODO: Comment  

-- 
transStat (SeqStat zun tak) s = (s'', zunInstrs ++ takInstrs)
  where
    (s',  zunInstrs) = transStat zun s 
    (s'', takInstrs) = transStat tak s'

--
transStat (DeclareStat vtype vname rhs it) s = (s''', declareInstrs)
  where
    -- Obtain the register that the rhs value will be saved into
    (dst:_) = availableRegs s
    -- Translate right hand side
    (s', rhsInstrs) = transRhs rhs it s
    -- How many bytes does this variable take up?
    size = sizeOf vtype
    -- Where is the current stack pointer with respect the free space on
    -- the stack? This is how we find the location of the variable on the stack
    offset = stackOffset s' - size -- Even s?
    -- Update the stack offset 
    s'' = s' { stackOffset = offset }
    -- We want to remember where vname is on the stack 
    -- Now remember the location of the variable on the stack
    s''' = s'' { stackMap = Map.insert vname (SP, offset) (stackMap s'') }
    -- We need to push the value in the dst reg onto the stack just cos
    -- TODO optimize in case of 0 offset
    pushDst = [ STR'Off dst SP offset ] -- [sp, #<off>] where 
    -- The whole instruction
    declareInstrs = rhsInstrs ++ pushDst

-- We are assigning a variable to a new value
transStat (AssignStat (LhsIdent id) rhs it) s = (s', assignInstrs)
  where
    -- Obtain the register that the rhs value will be saved into
    (dst:_) = availableRegs s
    -- Generate instructions for the right hand side
    (s', rhsInstrs) = transRhs rhs it s
    -- So we want to copy whatever is now in the destination register, into
    -- the register (or stack or whatever duuuude) where the variable @id@ is.
    -- but how do we know this? we need the stackmap yeeeeey
    -- This will need to change though when we use more than one register. cos
    -- the variable might just be in a register and not somewhere on the stack
    (src, off) = fromJust $ Map.lookup id (stackMap s') 
    -- Now for the actual freaking instruction. TODO use STRB when needed
    storeMe = [ STR'Off dst src off ] -- TODO use diff. instr. if off == 0
    -- The final thing
    assignInstrs = rhsInstrs ++ storeMe

-- 
transStat (IfStat cond thens elses it) s = (s'''', ifInstrs)
  where
    -- Obtain next free label
    currLabel = numJumpLabels s
    -- We need 2 labels: one for the else branch
    elseLabel = "if_else_" ++ show currLabel
    -- And one for after the if
    endifLabel = "if_end_" ++ show (currLabel + 1)
    -- Obtain the register that the cond expression value will be saved into
    (dst:_) = availableRegs s
    -- We have used up 2 labels so we need to update the arm state
    s' = s { numJumpLabels = currLabel + 2 }
    -- Now let's translate the if condition expression
    (s'', condInstrs) = transExpr cond s' 
    -- Now for the then branch, in its own scope
    (s''', thenInstrs) = transScoped thens s''
    -- Now for the else branch, also in its own scope
    (s'''', elseInstrs) = transScoped elses s'''
    -- Now concatenate everything together
    ifInstrs
      =  condInstrs                        -- Do the condition      
      ++ [ CMP dst $ Op2'ImmVal 0 ]        -- If condition false?
      ++ [ BEQ (JumpLabel elseLabel) ]     -- If so go to else
      ++ thenInstrs
      ++ [ B (JumpLabel endifLabel) ]
      ++ [ DEFINE (JumpLabel elseLabel) ]         
      ++ elseInstrs
      ++ [ DEFINE (JumpLabel endifLabel) ]


-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    RHS & LHS Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

--transLhs :: AssignLhs -> [ Instr ] 
--transLhs (LhsIdent id) = error "TODO"              
--transLhs (LhsPairElem pelem) = error "TODO"                 
--transLhs (LhsArrayElem (ArrayElem id exprs)) = error "TODO" 

transRhs :: AssignRhs -> IdentTable -> ArmState -> (ArmState, [Instr])
transRhs (RhsExpr       e            ) it arm = transExpr e arm          
transRhs (RhsPairElem   (Fst e)      ) it arm = error "TODO"
transRhs (RhsPairElem   (Snd e)      ) it arm = error "TODO"
transRhs (RhsArrayLiter (exprs)      ) it arm = (finalArm, rhsInstr)
  where
    rhsInstr = malloc ++ exprsInstr ++ lengthInstr 
    arrayLength = length exprs
    arraySize   = sum (map (sizeOf . flip typeOfExpr it) exprs) + sizeOf (ArrayType {})
    -- Allocate memeory on the heap
    malloc = [ LDR R0 arraySize        ] ++ 
             [ BL (JumpLabel "malloc") ] ++
             [ MOV dst (Op2'Reg R0)    ]
    (dst:nxt:_) = availableRegs arm
    -- Add to heap each elem of the array 
    (arm' , exprsInstr) = transArray exprs it arm 
    -- Add the length of the array to the heap  
    offset = stackOffset arm' - sizeOf (ArrayType {})
    lengthInstr = [ LDR nxt arrayLength   ] ++ 
                  [ STR'Reg nxt dst       ] ++ 
                  [ STR'Off dst SP offset ]
    finalArm = arm' { stackOffset = offset }


transRhs (RhsNewPair    e e'         ) it arm = error "TODO"
transRhs (RhsCall       fname  params) it arm = error "TODO"

-- Translates an array 
transArray :: [ Expr ] -> It -> ArmState -> (ArmState, [Instr])
transArray es it arm = transArray' es 0 (arm, [])
      where
        transArray' :: [ Expr ] -> Int -> (ArmState, [Instr]) -> (ArmState, [ Instr ])
        transArray' [] _ result            = result
        transArray' (e:es) index (arm, is) = (arm'', is ++ is' ++ is'')
          where
            s@(arm', is') = transArrayElem e it index arm
            (arm'', is'') = transArray' es (index+1) s


--Translates an array elem 
transArrayElem :: Expr -> It -> Int -> ArmState -> (ArmState, [Instr])
transArrayElem e it index arm = (arm' { availableRegs = r } , exprInstr ++ storeInstr)
    where
        
        r@(dst:nxt:regs) = availableRegs arm 

        -- Translate the expression 
        (arm', exprInstr) = transExpr e arm{availableRegs = nxt:regs}
        -- At what index in the heap the elem is
        -- +4 Beause the first 4 bytes are taken up by the length
        offset = (index * (sizeOf (typeOfExpr e it))) + 4 -- MAGIC NUMBER NOOO 
        storeInstr = if typeOfExpr e it == BoolType  -- TODO func for CHAR 
                        then [STRB'Off nxt dst offset]
                        else [STR'Off  nxt dst offset]  



-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************   Expression Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- |
transExpr :: Expr -> ArmState -> ( ArmState, [ Instr ] )

-- |
transExpr (IntLiterExpr i) s
  = (s, [ LDR dst i ])
    where 
      (dst:_) = availableRegs s

-- | Put the value of boolean @b@ into the first avaialble register @dst@
transExpr (BoolLiterExpr b) s
  = (s, [ MOV dst (Op2'ImmVal $ if b then 1 else 0) ])
    where
        (dst:_) = availableRegs s

-- | Put the string into a dataLabel and the label into the first register @dst@
transExpr (StrLiterExpr str) s
  = (s', [ LDR'Lbl dst l ])
    where
      (dst:_)  = availableRegs s
      s'       = s { dataLabels = ls }
      ls@(l:_) = newDataLabel str $ dataLabels s


-- | Put the char @c@ into the destination reg @dst@
transExpr (CharLiterExpr c) s
  = (s, [ MOV dst (Op2'ImmChr c) ])
    where
      (dst:_) = availableRegs s

-- |
transExpr PairLiterExpr s 
  = (s, [ LDR R0 8] ++ [ BL $ JumpLabel "malloc" ])


-- | TODO make ArrayElem a type synonym PLSSSSSSS
transExpr (ArrayElemExpr (ident, exprs)) s = error "ArrayElemExpr"  

-- TODO! TEST!
-- | Lookup what register variable @id@ is in, and copy its content in @dst@
transExpr (IdentExpr id) s = (s, pushDst) -- TODO LOL
    where
      (dst:_) = availableRegs s
      (src, off) = fromJust $ Map.lookup id (stackMap s) 
      pushDst = [ LDR'Off dst src off ] -- [sp, #<off>] where 

-- |
transExpr (ParenthesisedExpr e) s 
  = transExpr e s 

-- | Evaluates the expression and places it in the destination regster @dst@,
--   will perform  the unary operation on that reg 
transExpr (UnaryOperExpr op e) s
  = (s'', exprInstr ++ unopInstr)
    where
      (s' , exprInstr) = transExpr e  s 
      (s'', unopInstr) = transUnOp op s'


-- |
transExpr (BinaryOperExpr op e e') s
  = (s''', chikiInstr ++ chakaInstr ++ binopInstr)
    where
      (s'  , chikiInstr) = transExpr  e  s
      (_   , chakaInstr) = transExpr  e' s''
      (s''', binopInstr) = transBinOp op s'
      s''    = s' { availableRegs = rs }
      (r:rs) = availableRegs s'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Generate instructions for a unary operator
transUnOp :: UnaryOper -> ArmState -> ( ArmState, [ Instr ] )
transUnOp NotUnOp s 
  = (s, unopInstrs)
    where
      unopInstrs =  [ EOR     dst dst $ Op2'ImmVal 1 ]
                 ++ [ MOV'Reg R0  dst ]
      (dst:_)    =  availableRegs s
    
transUnOp LenUnOp s
  = (s, unopInstrs)
    where 
      unopInstrs =  [ LDR'Lbl dst l ]  -- stores in dst the adrress of a string
                 ++ [ LDR'Reg dst dst ]-- puts into dst the length of the addr,
                                        -- meaning the legth of the string
                  -- REDO comment
      (l:_)      =  dataLabels    s
      (dst:_)    =  availableRegs s

-- | Ints and chars are treated the same by ARM, so there is not need to do
-- anything out of the ordinary regarding Ord and Chr  
transUnOp OrdUnOp s = (s, [])

transUnOp ChrUnOp s = (s, []) 

transUnOp NegUnOp s 
  = (s, negUnOpInstrs)
    where 
      negUnOpInstrs =  [ RSBS dst dst $ Op2'ImmVal 0]  -- reverse subtract | dst := 0 - dst
                    ++ [ BLVS $ l]     -- jumps to pThrow if overflow
                                             -- |_Change this
      (l:_)      =  dataLabels    s
      (dst:_)    =  availableRegs s



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Generate instructions for a unary operator
transBinOp :: BinaryOper -> ArmState -> ( ArmState, [ Instr ] )
transBinOp AddBinOp s
  = ( s', instrs )
    where
      s'       =  s { dataLabels = ls'', predefLabels = ps''' }

      instrs   =  ( [ ADDS r r r' ] 
               ++ [ BLVS ( JumpLabel "p_throw_overflow_error") ] )
      (r:r':_) =  availableRegs s
      
      ps'''        = strPrintPredef l     ps''
      ls''@(l:_)   = newDataLabel "%.*s"  ls' 
      ps''         = runtErrPredef     ps'
      (ls', ps' )  = ovfErrPredef  ls  ps
      ls           = dataLabels    s
      ps           = predefLabels  s

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************           Labels           *********************** -- 
-- ***********************                            *********************** --
-- ************************************************************************** -- 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the so-called predefLabels for print. They consist
--   of lists of instructions generated similarly to main and the other functions,
--   but are defined as a sub-type of Label for logic convenience, and for easing 
--   up their use in a BL instruction.

intPrintPredef dataLabel ps
  = ps ++ [ PredefLabel name instrs ]
    where
      name   =  "p_print_int"                         
      instrs =  ( [ DEFINE $ JumpLabel name ] 
             ++ [ PUSH [ LR ] ]
             ++ [ MOV'Reg R1 R0 ]
             ++ [ LDR'Lbl R0 dataLabel ]
             ++ [ ADD R0 R0 $ Op2'ImmVal 4 ] 
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )


boolPrintPredef dataLabel1 dataLabel2 ps
  = ps ++ [ PredefLabel name instrs ]
    where
      name   =  "p_print_bool"                         
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ CMP R0 $ Op2'ImmVal 0 ]
             ++ [ LDRNE'Lbl R0 dataLabel1 ]
             ++ [ LDRNQ'Lbl R0 dataLabel2 ]
             ++ [ ADD R0 R0 $ Op2'ImmVal 4  ]  -- How do we know it's 4?
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )


strPrintPredef dataLabel ps
  = ps ++ [ PredefLabel name instrs ]
    where
      name   = "p_print_string"
      instrs =  ( [ DEFINE $ JumpLabel name ]
             ++ [ PUSH [ LR ] ]
             ++ [ LDR'Reg R1 R0 ]
             ++ [ ADD R2 R0 $ Op2'ImmVal 4 ]
             ++ [ LDR'Lbl R0 dataLabel ]
             ++ [ ADD R0 R0$ Op2'ImmVal 4 ]
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the so-called predefLabels for errors. These funcs
--   will print an error message and exit the program if necessary.

-- Integer overflow error 
ovfErrPredef ls ps
  = (ls', ps ++ [ PredefLabel name instrs ])
    where
      -- Creates a data label for printing an overflow error -- TODO \n \0 label issue
      ls'@(ovfLbl:_) =  newDataLabel ( "OverflowError: the result is too small/large" ++ 
                                       "to store in a 4-byte signed-integer."          )
                                     ls 
      name   =  "p_throw_overflow_error"
      -- The set of instructions calls runtime error which exits the program
      instrs =  ( [ DEFINE $ PredefLabel name [] ]
             ++ [ LDR'Lbl R0 ovfLbl ]
             ++ [ BL ( JumpLabel "p_throw_runtime_error" ) ] )


-- Runtime error
runtErrPredef ps 
  = ps ++ [ PredefLabel name instrs ]
    where
      name   = "p_throw_runtime_error"
      instrs =  ( [ DEFINE $ JumpLabel name ] 
             ++ [ BL $ JumpLabel "p_print_string" ] 
             ++ [ MOV R0 $ Op2'ImmVal (-1) ]
             ++ [ BL ( JumpLabel "exit" )  ] )


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | Create a new data label and return the list with the label added.
newDataLabel str ls 
  = (l:ls) 
    where 
      l     = DataLabel lName str
      lName = "msg_" ++ ( show $ length ls )




-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     Statement Helpers      *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- | 
transScoped          :: Stat -> ArmState -> (ArmState, [Instr])
transScoped stat arm = (arm', editStack SUB ++ statInstr ++ editStack ADD)
      where
        -- Make room on the stack or free the stack
        editStack :: (Rd -> Rn -> Operand2 -> Instr) -> [ Instr ]
        editStack mne = 
                   take chunks (cycle [ mne SP SP $ Op2'ImmVal 1024 ]) ++
          if left == 0 then [] else [ mne SP SP $ Op2'ImmVal left ] 

        -- Translate the statemente after reserving space on the stack
        (arm', statInstr) = transStat stat arm { stackOffset = freeBytes }
        -- Work out how many variables are declared in the scope of this 
        -- statement.
        freeBytes      = getBytesNeeded stat
        -- Can only add and sub from sp in chunks of 1024
        (chunks, left) = divMod freeBytes 1024 

-- | Does the statement introduce variables in its scope?
getBytesNeeded                            :: Stat -> Int -- In Bytes
getBytesNeeded (SeqStat s s'           )  =  getBytesNeeded s + getBytesNeeded s'                   
getBytesNeeded (DeclareStat vtype _ _ _)  =  sizeOf vtype
getBytesNeeded _                          =  0 

sizeOf                :: Type -> Int  -- In bytes 
sizeOf IntType        =  4                                 
sizeOf BoolType       =  1                             
sizeOf CharType       =  1                             
sizeOf StringType     =  4 -- Addresss                              
sizeOf (PairType  _)  =  4 -- Address   
sizeOf (ArrayType _)  =  4 -- Address                         
sizeOf NullType       =  0 -- ?                                
sizeOf EmptyType      =  0 -- ?

typeOfExpr                                        :: Expr -> It -> Type    
typeOfExpr ( BoolLiterExpr     _            ) _   =  BoolType    
typeOfExpr ( CharLiterExpr     _            ) _   =  CharType      
typeOfExpr ( IdentExpr         id           ) it  =  fromJust (findType' id it)       
typeOfExpr ( UnaryOperExpr     NotUnOp  _   ) _   =  BoolType
typeOfExpr ( UnaryOperExpr     LenUnOp  _   ) _   =  IntType
typeOfExpr ( UnaryOperExpr     OrdUnOp  _   ) _   =  IntType
typeOfExpr ( UnaryOperExpr     ChrUnOp  _   ) _   =  CharType
typeOfExpr ( UnaryOperExpr     NegUnOp  _   ) _   =  IntType
typeOfExpr ( ParenthesisedExpr e            ) it  =  typeOfExpr e it        
typeOfExpr ( IntLiterExpr      _            ) _   =  IntType   
typeOfExpr ( StrLiterExpr      _            ) _   =  StringType
typeOfExpr ( PairLiterExpr                  ) _   =  NullType       
typeOfExpr ( ArrayElemExpr     arrelem      ) it  =  typeOfArrElem arrelem it     
typeOfExpr ( BinaryOperExpr    AddBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    SubBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    MulBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    DivBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    ModBinOp _ _ ) _   =  IntType
typeOfExpr ( BinaryOperExpr    AndBinOp _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    OrrBinOp _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    LsBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    GtBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    LEBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    GEBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    EqBinOp  _ _ ) _   =  BoolType
typeOfExpr ( BinaryOperExpr    NEBinOp  _ _ ) _   =  BoolType        
 
typeOfArrElem :: ArrayElem -> It -> Type
typeOfArrElem _ _ = NullType -- lazy to make it compile, irrelephant atm
--typeOfArrElem (id, es) it   = deepen (length es + 1) $ fromJust (findType' id it) 
--  where
--    deepen 0  t             =  t
--    deepen n (ArrayType t)  =  deepen (n-1) t
--    deepen _  t             =  t


sizeOfType :: Type -> Int  -- In bytes 
sizeOfType IntType       = 4                                 
sizeOfType BoolType      = 1                             
sizeOfType CharType      = 1                             
sizeOfType StringType    = 4  -- Addresss                              
sizeOfType (PairType  _) = 4 -- Address   
sizeOfType (ArrayType _) = 4 -- Address                         
sizeOfType NullType      = 0 -- ?                                
sizeOfType EmptyType     = 0 -- ?


-- ************************************************************************** --
-- **************************                         *********************** --
-- **************************   Code Prettification   *********************** --
-- **************************                         *********************** -- 
-- ************************************************************************** --

-- TODO Do Not Rename 
makePretty :: ( ArmState, [ Instr ] ) -- computed by transProgram
           ->   String                -- printable compiled program
makePretty (s, instrs) 
    =  show ( INDIR Data )  ++ "\n"
    ++ concatMap putDataLabel ( reverse ( dataLabels s ) )
    ++ show ( INDIR Text )  ++ "\n"                  
    ++ show ( INDIR ( Global ( "main" ) ) ) ++ "\n"
    ++ ( concat $ intersperse "\n" $ map show instrs ) ++ "\n"
    ++ concatMap putPredefLabel ( predefLabels s )
      where
        putDataLabel ( DataLabel l str ) 
          =  l
          ++ "\n\t.word " ++ show ( length str + 1 )
          ++ "\n\t.ascii \"" ++ str  ++ "\\0\"\n"

        putPredefLabel ( PredefLabel l instrs )
          = ( concat $ intersperse "\n\t" $ map show instrs ) ++ "\n"

-- TODO move somewhere else
nextLabel :: Int -> Label
nextLabel i = JumpLabel $ "L" ++ show i


