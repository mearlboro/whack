module Wacc.CodeGeneration.WaccCodeGen where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.Data.DataTypes
import Wacc.Data.SymbolTable

import qualified Data.Map as Map
import Data.Array
import Data.List (intersperse, mapAccumR, mapAccumL)
import Data.Maybe
import Data.Char
import Debug.Trace
import Wacc.Data.ShowInstances

import Debug.Trace
import Data.Tuple (swap)


--null_reference_error_msg 
--  = DataLabel "null_reference_error_msg" 
--              "NullReferenceError: dereference a null reference\n\0"

--p_throw_runtime_error 
--  = [ BL "p_print_string"
--    , MOV'Reg R0 (-1) 
--    , BL "exit" ]

--p_check_null_pointer 
--  = [ PUSH LR 
--    , CMP R0 0 
--    , LDREQ R0 "null_reference_error_msg"
--    , BLEQ "p_throw_runtime_error" 
--    , POP PC ]

-- The string label in .data that contains the message to print
iPrintString :: String -> [ Instr ] 
iPrintString = undefined


-- int x = 10
-- int x = (trace "bla" 10)

-- If no data, do not output .data

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
    -- Translate each function 
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
      , stackPointer  = 0
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
transFunc (Func ftype fname args body it) s  =  (s' {stackMap = oldMap}, functInstrs)
  where 
    oldMap = stackMap s 

    insertParam ((Param _ pname), offset) = Map.insert pname (SP, offset)
    offsets = tail (scanl (+) 0 sizes)
    sizes = map (sizeOf . ptypeOf) args
    finalMap = foldl (flip insertParam) oldMap (zip args offsets)

    -- Translate the function body in its own scope
    (s', bodyInstrs)  =  transScoped body s { stackMap = finalMap }

    -- The whole function translated to assembly
    functInstrs       
      =  [ DEFINE ( JumpLabel ("f_" ++ fname ++ ":")) ]  -- Define label with unique function name 
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
    transFuncs' (f:fs) (arm, is) = (arm'', is ++ is'')
      where
        s@(arm', _is') = transFunc  f arm
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
      ++ [ MOV R0 $ Op2'Reg dst  ] -- TODO: Comment
      ++ [ BL (JumpLabel "exit") ] -- TODO: Comment

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

-- TODO: to implement arrays, pairs (printing an address), identifiers 
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
        intDataLabels  ls = let (l,  ls' ) = newDataLabel "%d"    ls  in
                            ls'
        boolDataLabels ls = let (l,  ls' ) = newDataLabel "true"  ls  in
                            let (l', ls'') = newDataLabel "false" ls' in
                            ls''
        strDataLabels  ls = let (l,  ls' ) = newDataLabel "%.*s"  ls  in
                            ls'

--
transStat (PrintlnStat e it) s = error "PrintlnStat"

-- 
transStat (ScopedStat stat) s = transScoped stat s

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
    (s', zunInstrs) = transStat zun s 
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
    newMap = Map.insert vname (SP, offset) (stackMap s'')
    s''' = s'' { stackMap = newMap }
    -- We need to push the value in the dst reg onto the stack just cos
    -- TODO optimize in case of 0 offset
    pushDst = [ strVar dst SP size offset ] -- [sp, #<off>] where 
    -- The whole instruction
    declareInstrs = rhsInstrs ++ pushDst

-- We are assigning a variable to a new value
transStat (AssignStat (LhsIdent id) rhs it) s = (s', assignInstrs)
  where
    -- TODO magic constant 4
    -- Obtain the register that the rhs value will be saved into
    (dst:_) = availableRegs s
    -- Generate instructions for the right hand side
    (s', rhsInstrs) = transRhs rhs it s
    -- So we want to copy whatever is now in the destination register, into
    -- the register (or stack or whatever duuuude) where the variable @id@ is.
    -- but how do we know this? we need the stackmap yeeeeey
    -- This will need to change though when we use more than one register. cos
    -- the variable might just be in a register and not somewhere on the stack
    (src, off) = fromJust $ Map.lookup id (stackMap s) 
    -- Now for the actual freaking instruction. TODO use STRB when needed
    storeMe = [ strVar dst src 4 off ] -- TODO use diff. instr. if off == 0
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


strVar :: Rn -> Rd -> Int -> Int -> Instr 
strVar rd rn size off 
  | size == 1 = if off == 0 then STRB'Reg rd rn else STRB'Off rd rn off
  | otherwise = if off == 0 then STR'Reg rd rn else STR'Off rd rn off

ldrVar :: Rn -> Rd -> Int -> Int -> Instr 
ldrVar rd rn size off 
  | size == 1 = if off == 0 then LDRSB'Reg rd rn else LDRSB'Off rd rn off
  | otherwise = if off == 0 then LDR'Reg rd rn else LDR'Off rd rn off

strArg :: Rn -> Rn -> Int -> Int -> Instr 
strArg rd rn size off 
  | size == 1 = STRB'Arg rd rn off 
  | otherwise = STR'Arg rd rn off 

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************         Printing           *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** -- 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | These functions will create the so-called predefLabels for print. They consist
--   of lists of instructions generated similarly to main and the other functions,
--   but are defined as a sub-type of Label for logic convenience, and for easing 
--   up their use in a BL instruction.

-- TODO: if is ident, add MOV r1 r0 after push
intPrintPredef dataLabel ps
  = ps ++ [ PredefLabel name instrs ]
    where
      name   =  "p_print_int"                         
      instrs =  ( [ DEFINE $ PredefLabel name [] ] -- we don't need instructions here
             ++ [ PUSH [ LR ] ]
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
      instrs =  ( [ DEFINE $ PredefLabel name [] ]
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
      name = "p_print_string"
      instrs =  ( [ DEFINE $ PredefLabel name [] ]
             ++ [ PUSH [ LR ] ]
             ++ [ LDR'Reg R1 R0 ]
             ++ [ ADD R2 R0 $ Op2'ImmVal 4 ]
             ++ [ LDR'Lbl R0 dataLabel ]
             ++ [ ADD R0 R0$ Op2'ImmVal 4 ]
             ++ [ BL ( JumpLabel "printf" ) ]
             ++ [ MOV R0 $ Op2'ImmVal 0 ]
             ++ [ BL ( JumpLabel "fflush" ) ]
             ++ [ POP [ PC ] ] )


-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    RHS & LHS Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transLhs :: AssignLhs -> [ Instr ] 
transLhs (LhsIdent id)              = error "TODO"              
transLhs (LhsPairElem pelem)        = error "TODO"                 
transLhs (LhsArrayElem (id, exprs)) = error "TODO" 

transRhs :: AssignRhs -> It -> ArmState -> (ArmState, [Instr])
transRhs (RhsExpr       e            ) it arm = transExpr e arm          
transRhs (RhsPairElem   (Fst e)      ) it s = (s', pelemInstr)
  where
    (dst:_)          = availableRegs s
    (s', exprInstrs) = transExpr e s

    PairType (Just (fstType, sndType)) = typeOfExpr e it 
    size = sizeOf fstType

    pelemInstr 
      =  exprInstrs 
      ++ [ MOV'Reg R0 dst ]
      ++ [ BL (JumpLabel "p_check_null_pointer") ]
      ++ [ LDR'Reg dst dst ]
      ++ [ ldrVar dst dst size 0 ] 

transRhs (RhsPairElem (Snd e)) it s = (s', pelemInstr)
  where
    (dst:_) = availableRegs s
    (s', exprInstrs) = transExpr e s 

    PairType (Just (fstType, sndType)) = typeOfExpr e it 
    size = sizeOf sndType

    pelemInstr 
      =  exprInstrs 
      ++ [ MOV'Reg R0 dst                        ]
      ++ [ BL (JumpLabel "p_check_null_pointer") ]
      ++ [ LDR'Off dst dst 4                     ]
      ++ [ ldrVar dst dst size 0                 ]


transRhs (RhsArrayLiter (exprs)) it arm = (arm', rhsInstr)
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
    (arm' , exprsInstr) = transArray exprs (it) arm 
    -- Add the length of the array to the heap  
    --offset = stackOffset arm' - sizeOf (ArrayType {})
    lengthInstr = [ LDR nxt arrayLength   ] ++ 
                  [ STR'Reg nxt dst       ] -- ++ 
                  -- [ STR'Off dst SP offset ]
    --finalArm = arm' { stackOffset = offset }


transRhs (RhsNewPair fstExpr sndExpr) it s = (s'',  newPairInstrs )
  where
    pairAdrSize = 8

    (dst:nxt:_) = availableRegs s

    (s',  fstInstrs) = transPairElemExpr fstExpr it 0 s
    (s'', sndInstrs) = transPairElemExpr sndExpr it 1 s'

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


transRhs (RhsCall fname params) it s = (s', callInstrs)
  where
    -- mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    -- x = Expr 
    --transFuncts :: -> ParamList -> (ArmState, [[Instr]])
    (dst:_) = availableRegs s
    -- Expresssion instructtion
    (s', instrs) = mapAccumR (flip transExpr) s (reverse params)
    -- Generate instructions to push params on the stack
    sizeOfExpr = sizeOf . flip typeOfExpr it -- ex

    pushArg e = let s = sizeOfExpr e in [[ strArg dst SP s (-s) ]]

    pushArgs = map pushArg params 

    paramInstrs = (concat . concat) (zipWith (:) instrs pushArgs)

    totSize = sum (map sizeOfExpr params)

    callInstrs 
      = paramInstrs 
      ++ [ BL (JumpLabel ("f_" ++ fname ++ ":")) ]
      ++ (if totSize == 0 then [] else [ ADD SP SP $ Op2'ImmVal totSize ]) 
      ++ [ MOV'Reg dst R0 ] -- TODO


type WhichOfTheTwo = Int 
transPairElemExpr :: Expr -> It -> WhichOfTheTwo -> ArmState -> (ArmState, [ Instr ])
transPairElemExpr expr it wott s = (s' { availableRegs = rs }, instr)
  where 
    rs@(dst:nxt:regs) = availableRegs s 
    (s', exprInstr) = transExpr expr s { availableRegs = nxt:regs }
    size = (sizeOf (typeOfExpr expr it))
    instr 
      =  exprInstr 
      ++ [ LDR R0 size             ] 
      ++ [ BL (JumpLabel "malloc") ]
      ++ [ strVar nxt R0 size 0    ]
      ++ [ strVar R0 dst 4 (wott * 4) ]
  

-- Translates an array 
transArray :: [ Expr ] -> It -> ArmState -> (ArmState, [Instr])
transArray es it arm = transArray' es 0 (arm, [])
      where
        transArray' :: [ Expr ] -> Int -> (ArmState, [Instr]) -> (ArmState, [ Instr ])
        transArray' [] _ result            = result
        transArray' (e:es) index (arm, is) = (arm'', is ++ is'')
          where
            s@(arm', _is') = transArrayElem e it index arm
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
      s'       = s { dataLabels = ls' }
      (l, ls') = newDataLabel str $ dataLabels s


-- | Put the char @c@ into the destination reg @dst@
transExpr (CharLiterExpr c) s
  = (s, [ MOV dst (Op2'ImmChr c) ])
    where
      (dst:_) = availableRegs s

-- TODO! TEST!
-- | Lookup what register variable @id@ is in, and copy its content in @dst@
transExpr (IdentExpr id) s = (s, pushDst) -- TODO LOL
    where
      (dst:_) = availableRegs s
      
      hello = Map.lookup id (stackMap s)
      (src, off) = fromJust $ Map.lookup id (stackMap s)

      pushDst = [ LDR'Off dst src off ] -- [sp, #<off>] where TODO check 0 case

transExpr (UnaryOperExpr NegUnOp (IntLiterExpr i)) s 
  = transExpr (IntLiterExpr (-i)) s

-- | Evaluates the expression and places it in the destination regster @dst@,
--   will perform  the unary operation on that reg 
transExpr (UnaryOperExpr op e) s
  = (s'', exprInstr ++ unopInstr)
    where
      (s' , exprInstr) = transExpr e  s 
      (s'', unopInstr) = transUnOp op s'

-- |
transExpr (ParenthesisedExpr e) s 
  = transExpr e s 

-- |
transExpr (PairLiterExpr) s = (s, [ LDR dst size ])
  where
    (dst:_) = availableRegs s 
    size = 0 -- sizeOf NullType



-- | TODO make ArrayElem a type synonym PLSSSSSSS
transExpr (ArrayElemExpr (ident, exprs)) s = error "ArrayElemExpr"  

-- |
transExpr (BinaryOperExpr op e e') s = error "BinaryOperExpr"  
 

--------------------------------------------------------------------------------
-- | Create a new data label and return the list with the label added.
newDataLabel str ls 
  = (l, ls ++ [l]) 
    where 
      l     = DataLabel lName str
      lName = "msg_" ++ ( show $ length ls )


--------------------------------------------------------------------------------
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
                    ++ [ BLVS $ l ]     -- jumps to pThrow if overflow
                                             -- |_Change this
      (l:_)      =  dataLabels    s
      (dst:_)    =  availableRegs s

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     Statement Helpers      *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- | 
transScoped          :: Stat -> ArmState -> (ArmState, [Instr])
transScoped stat arm = (arm'''', editStack SUB ++ statInstr ++ editStack ADD)
      where
        -- Make room on the stack or free the stack
        editStack :: (Rd -> Rn -> Operand2 -> Instr) -> [ Instr ]
        editStack mne = 
                 take chunks (cycle [ mne SP SP $ Op2'ImmVal 1024 ]) ++
          if left == 0 then [] else [ mne SP SP $ Op2'ImmVal left ] 

        -- This is the old map
        oldMap = stackMap arm

        oldOffset = stackOffset arm 
        -- Update stack offset
        arm' = arm { stackOffset = oldOffset + bytesNeeded }
        -- Comment later
        allah = getBytesNeeded' stat 
        -- Update variables in map, change the map
        arm'' = arm' { stackMap = Map.map (\(r, o) -> (r, o+bytesNeeded)) oldMap }
        -- Translate the statemente after reserving space on the stack
        (arm''', statInstr) = transStat stat arm''
        -- Restore stack offset
        arm'''' = arm''' { stackMap = oldMap }

        -- Work out how many variables are declared in the scope of this 
        -- statement.
        bytesNeeded  =  getBytesNeeded stat 
        -- Can only add and sub from sp in chunks of 1024
        (chunks, left) = divMod bytesNeeded 1024 

        --arm'' = arm' { stackOffset = oldOffset }

-- | Does the statement introduce variables in its scope?
getBytesNeeded                            :: Stat -> Int -- In Bytes
getBytesNeeded (SeqStat s s'           )  =  getBytesNeeded s + getBytesNeeded s'                   
getBytesNeeded (DeclareStat vtype _ _ _)  =  sizeOf vtype
getBytesNeeded _                          =  0 

-- | Does the statement introduce variables in its scope?
getBytesNeeded'    :: Stat -> Int -- In Bytes
getBytesNeeded' s  =  snd $ getBytesNeeded'' s (False, 0)
  where
    getBytesNeeded'' (SeqStat s s') (stop, acc)  =  if stop' then (undefined, acc') else (stop'', acc'')
        where
          (stop' , acc' ) = getBytesNeeded'' s  (stop, acc) 
          (stop'', acc'') = getBytesNeeded'' s' (stop', acc')

    getBytesNeeded'' (DeclareStat vtype _ _ _) (_, acc)  =  (False, acc + sizeOf vtype)
    getBytesNeeded'' (ScopedStat  _          ) (_, acc)  =  (True, acc)    
    getBytesNeeded'' (WhileStat   _ _ _      ) (_, acc)  =  (True, acc)        
    getBytesNeeded'' (IfStat      _ _ _ _    ) (_, acc)  =  (True, acc)       
    getBytesNeeded'' (_                      ) (_, acc)  =  (False, acc)             


sizeOf                :: Type -> Int  -- In bytes 
sizeOf IntType        =  4                                 
sizeOf BoolType       =  1                             
sizeOf CharType       =  1                             
sizeOf StringType     =  4 -- Addresss                              
sizeOf (PairType  _)  =  4 -- Address   
sizeOf (ArrayType _)  =  4 -- Address                         
sizeOf NullType       =  4 -- ?                                
sizeOf EmptyType      =  0 -- ?

typeOfExpr                                        :: Expr -> It -> Type    
typeOfExpr ( BoolLiterExpr     _            ) _   =  BoolType    
typeOfExpr ( CharLiterExpr     _            ) _   =  CharType      
typeOfExpr ( IdentExpr         id           ) it  =  fromJust $ findType' id it
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

pairUp                :: [a] -> [a] -> [a] 
pairUp    []     []   =  [] 
pairUp    xs     []   =  xs 
pairUp    []     ys   =  ys 
pairUp (x:xs) (y:ys)  =  x:y:(pairUp xs ys)

-- TODO Do Not Rename 
makePretty :: ( ArmState, [ Instr ] ) -- computed by transProgram
           ->   String                -- printable compiled program
makePretty (s, instrs) 
    =  show ( INDIR Data )  ++ "\n"
    ++ concatMap putDataLabel ( dataLabels s )
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


