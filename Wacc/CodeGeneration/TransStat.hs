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

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Statement Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transStat :: Assembler Stat

--
transStat s SkipStat = (s, [])

-- 
transStat s (FreeStat e _) = error "FreeStat"

-- To exit we load the value of the expression into the first available reg
-- and then move its value into register 0. then call BL exit  
transStat s (ExitStat expr it) = (s', exitInstrs)
  where
    -- Obtain the register that the expression value will be saved into
    (dst:_)  =  freeRegs s
    -- Translate the expression to exit
    (s', exprInstrs)  =  transExpr s (expr, it) 
    -- Instructions for the exit statement
    exitInstrs         
      =  exprInstrs 
      ++ [ MOV R0 $ Op2'Reg dst  ] -- TODO: Comment
      ++ [ BL (JumpLabel "exit") ] -- TODO: Comment

-- 
transStat s (ReturnStat e it) = (s', return'is)
  where
    -- Obtain the register that the expression value will be saved into
    (dst:_)  =  freeRegs s
    -- Translate the expression to return
    (s', expr'is) = transExpr s (e, it) 
    -- Move result of expression from dst into return register R0
    move = [ MOV R0 $ Op2'Reg dst ] 
    -- Instructions for the return statement
    return'is = expr'is ++ move

-- TODO: to implement arrays, pairs (printing an address), identifiers 
transStat s (PrintStat e it) 
  = ( s'', instrs')
    where
        -- The new state just updates the data labels
        s''          = s' { dataLabels= ls', predefLabels= ps' }


        -- First gets the instructions for the expr, then adds the print
        instrs'      = instrs ++ [ MOV'Reg R0 dst ] ++ label
        (s', instrs) = transExpr s (e, it) 
        (dst:_)      = freeRegs s'
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
        intDataLabels  l= let (l,  ls' ) = newDataLabel "%d"    ls  in
                            ls'
        boolDataLabels l= let (l,  ls' ) = newDataLabel "true"  ls  in
                            let (l', ls'') = newDataLabel "false" ls' in
                            ls''
        strDataLabels  l= let (l,  ls' ) = newDataLabel "%.*s"  ls  in
                            ls'

--
transStat s (PrintlnStat e it) = error "PrintlnStat"

-- 
transStat s (ScopedStat stat) = transScoped s stat 

-- 
transStat s (ReadStat lhs it) = error "ReadStat" 

-- 
transStat s (WhileStat cond body it) = (s''', whileInstr)
    where
      -- Obtain next free label
      currLabel = numJumpLabels s
      -- We need 2 labels: one for the condition
      condLabel = "while_cond_" ++ show currLabel
      -- And one for the body
      bodyLabel = "while_body_" ++ show (currLabel + 1)
      -- Obtain the register that the cond expression value will be saved into
      (dst:_) = freeRegs s
      -- We have used up 2 labels so we need to update the state
      s' = s { numJumpLabels= currLabel + 2 }
      -- Now let's translate the loop condition expression with the new state
      (s'', condInstrs) = transExpr s' (cond, it) 
      -- Now for the body statements, which are scoped           
      (s''', bodyInstrs) = transScoped s'' body
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
transStat s (SeqStat zun tak) = (s'', zunInstrs ++ takInstrs)
  where
    (s', zunInstrs) = transStat s zun  
    (s'', takInstrs) = transStat s' tak 

--
transStat s (DeclareStat vtype vname rhs it) = (s''', declareInstrs)
  where
    -- Obtain the register that the rhs value will be saved into
    (dst:_) = freeRegs s
    -- Translate right hand side
    (s', rhsInstrs) = transRhs s (rhs, it) 
    -- How many bytes does this variable take up?
    size = sizeOf vtype
    -- Where is the current stack pointer with respect the free space on
    -- the stack? This is how we find the location of the variable on the stack
    offset = stackOffset s' - size -- Even s?
    -- Update the stack offset 
    s'' = s' { stackOffset = offset }
    -- We want to remember where vname is on the stack 
    -- Now remember the location of the variable on the stack
    newMap = Map.insert vname (SP, offset) (memoryMap s'')
    s''' = s'' { memoryMap = newMap }
    -- We need to push the value in the dst reg onto the stack just cos
    -- TODO optimize in case of 0 offset
    pushDst = [ strVar dst SP size offset ] -- [sp, #<off>] where 
    -- The whole instruction
    declareInstrs = rhsInstrs ++ pushDst

-- We are assigning a variable to a new value
transStat s (AssignStat (LhsIdent id) rhs it) = (s', assignInstrs)
  where
    -- TODO magic constant 4
    -- Obtain the register that the rhs value will be saved into
    (dst:_) = freeRegs s
    -- Generate instructions for the right hand side
    (s', rhsInstrs) = transRhs s (rhs, it) 
    -- So we want to copy whatever is now in the destination register, into
    -- the register (or stack or whatever duuuude) where the variable @id@ is.
    -- but how do we know this? we need the stackmap yeeeeey
    -- This will need to change though when we use more than one register. cos
    -- the variable might just be in a register and not somewhere on the stack
    (src, off) = fromJust $ Map.lookup id (memoryMap s) 
    -- Now for the actual freaking instruction. TODO use STRB when needed
    storeMe = [ strVar dst src 4 off ] -- TODO use diff. instr. if off == 0
    -- The final thing
    assignInstrs = rhsInstrs ++ storeMe


-- We are assigning a variable to a new value
transStat s (AssignStat (LhsPairElem pelem) rhs it) = (s', pelemInstr)
  where
    (dst:nxt:_) = freeRegs s 
    (s', rhsInstrs) = transRhs s (rhs, it) 
    (rg, off) = fromJust (Map.lookup (getElemId pelem) (memoryMap s'))

    --PairType (Just (fstType, sndType)) = fromJust (getPairElemType pelem it) 
    pelemType = fromJust $ getPairElemType pelem it

    elemSize = sizeOf pelemType -- sizeOf $ if (pelem ~== Fst {}) then fstType else sndType
    elemOff = if (pelem ~== Fst {}) then 0 else 4
    pelemInstr 
      =  rhsInstrs 
      ++ [ ldrVar nxt rg 4 off ] 
      ++ [ MOV'Reg R0 nxt ] -- check null
      ++ [ BL (JumpLabel "p_check_null_pointer") ]
      ++ [ ldrVar nxt nxt 4  elemOff ]
      ++ [ strVar dst nxt elemSize 0 ]

    getElemId :: PairElem -> IdentName
    getElemId (Fst e) = fromJust $ getIdent e
    getElemId (Snd e) = fromJust $ getIdent e

    getIdent                                  :: Expr -> Maybe IdentName
    getIdent ( IdentExpr       ident       )  =  Just ident
    getIdent ( ArrayElemExpr ( ident , _ ) )  =  Just ident
    getIdent                           _      =  Nothing


-- We are assigning a variable to a new value
--transStat (AssignStat (LhsArrayElem (id, exprs)) rhs it) = (s', pelemInstr)
--  where



-- 
transStat s (IfStat cond thens elses it) = (s'''', ifInstrs)
  where
    -- Obtain next free label
    currLabel = numJumpLabels s
    -- We need 2 labels: one for the else branch
    elseLabel = "if_else_" ++ show currLabel
    -- And one for after the if
    endifLabel = "if_end_" ++ show (currLabel + 1)
    -- Obtain the register that the cond expression value will be saved into
    (dst:_) = freeRegs s
    -- We have used up 2 labels so we need to update the arm state
    s' = s { numJumpLabels = currLabel + 2 }
    -- Now let's translate the if condition expression
    (s'', condInstrs) = transExpr s' (cond, it)  
    -- Now for the then branch, in its own scope
    (s''', thenInstrs) = transScoped s'' thens 
    -- Now for the else branch, also in its own scope
    (s'''', elseInstrs) = transScoped s'''' elses 
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

--
transRhs :: Assembler (AssignRhs, It)

--
transRhs s (RhsExpr e, it) = transExpr s (e, it)  

--
transRhs s (RhsPairElem (Fst e), it) = (s', pelemInstr)
  where
    (dst:_)          = freeRegs s
    (s', exprInstrs) = transExpr s (e, it)

    PairType (Just (fstType, sndType)) = typeOfExpr e it 
    size = sizeOf fstType

    pelemInstr 
      =  exprInstrs 
      ++ [ MOV'Reg R0 dst ]
      ++ [ BL (JumpLabel "p_check_null_pointer") ]
      ++ [ LDR'Reg dst dst ]
      ++ [ ldrVar dst dst size 0 ] 

--
transRhs s (RhsPairElem (Snd e), it) = (s', pelemInstr)
  where
    (dst:_) = freeRegs s
    (s', exprInstrs) = transExpr s (e, it) 

    PairType (Just (fstType, sndType)) = typeOfExpr e it 
    size = sizeOf sndType

    pelemInstr 
      =  exprInstrs 
      ++ [ MOV'Reg R0 dst                        ]
      ++ [ BL (JumpLabel "p_check_null_pointer") ]
      ++ [ LDR'Off dst dst 4                     ]
      ++ [ ldrVar dst dst size 0                 ]

-- 
transRhs arm (RhsArrayLiter (exprs), it) = (arm', rhsInstr)
  where
    rhsInstr = malloc ++ exprsInstr ++ lengthInstr 
    arrayLength = length exprs
    arraySize   = sum (map (sizeOf . flip typeOfExpr it) exprs) + sizeOf (ArrayType {})
    -- Allocate memeory on the heap
    malloc = [ LDR R0 arraySize        ] ++ 
             [ BL (JumpLabel "malloc") ] ++
             [ MOV dst (Op2'Reg R0)    ]
    (dst:nxt:_) = freeRegs arm
    -- Add to heap each elem of the array 
    (arm' , exprsInstr) = transArray arm (exprs, it) 
    -- Add the length of the array to the heap  
    --offset = stackOffset arm' - sizeOf (ArrayType {})
    lengthInstr = [ LDR nxt arrayLength   ] ++ 
                  [ STR'Reg nxt dst       ] -- ++ 
                  -- [ STR'Off dst SP offset ]
    --finalArm = arm' { stackOffset = offset }

-- 
transRhs s (RhsNewPair fstExpr sndExpr, it) = (s'',  newPairInstrs )
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
    -- Expresssion instructtion
    (s', instrs) = mapAccumR (\_s _e -> transExpr _s (_e, it)) s (reverse params)
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
transPairElemExpr :: Assembler (Expr, It, WhichOfTheTwo) 
transPairElemExpr s (expr, it, wott) = (s' { freeRegs = rs }, instr)
  where 
    rs@(dst:nxt:regs) = freeRegs s 
    (s', exprInstr) = transExpr s { freeRegs = nxt:regs } (expr, it) 
    size = (sizeOf (typeOfExpr expr it))
    instr 
      =  exprInstr 
      ++ [ LDR R0 size             ] 
      ++ [ BL (JumpLabel "malloc") ]
      ++ [ strVar nxt R0 size 0    ]
      ++ [ strVar R0 dst 4 (wott * 4) ]
  

-- Translates an array 
transArray :: Assembler ([ Expr ], It)
transArray arm (es, it) = transArray' es 0 (arm, [])
      where
        transArray' :: [ Expr ] -> Int -> (ArmState, [Instr]) -> (ArmState, [ Instr ])
        transArray' [] _ result            = result
        transArray' (e:es) index (arm, is) = (arm'', is ++ is'')
          where
            s@(arm', _is') = transArrayElem arm (e, it, index) 
            (arm'', is'') = transArray' es (index+1) s


--Translates an array elem 
transArrayElem :: Assembler (Expr, It, Int) 
transArrayElem arm (e, it, index) = (arm' { freeRegs= r } , exprInstr ++ storeInstr)
    where
        
        r@(dst:nxt:regs) = freeRegs arm 

        -- Translate the expression 
        (arm', exprInstr) = transExpr arm{freeRegs = nxt:regs} (e, it)
        -- At what index in the heap the elem is
        -- +4 Beause the first 4 bytes are taken up by the length
        offset = (index * (sizeOf (typeOfExpr e it))) + 4 -- MAGIC NUMBER NOOO 
        storeInstr = if typeOfExpr e it == BoolType  -- TODO func for CHAR 
                        then [STRB'Off nxt dst offset]
                        else [STR'Off  nxt dst offset]  

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
        allah = getBytesNeeded' stat 
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

