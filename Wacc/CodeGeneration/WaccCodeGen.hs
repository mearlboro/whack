module Wacc.CodeGeneration.WaccCodeGen where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.Data.DataTypes
import Wacc.Data.SymbolTable

import qualified Data.Map as Map
import Data.Array
import Data.List ( intersperse )
import Data.Maybe
import Data.Char

-- TODO rename lol
makePretty :: ( [ Instr ], ArmState ) -- computed by transProgram
           ->   String                -- printable compiled program
makePretty (instrs, arm) = ""
    -- =  show ( INDIR Text )  ++ "\n"                  
    -- ++ ( concat $ intersperse "\n" $ map show ls  ) 
    -- ++ show ( INDIR ( Global ( JumpLabel "main" ) ) ) ++ "\n"
    -- ++ ( concat $ intersperse "\n" $ map show instrs )


-- ************************************************************************** --
-- **************************                         *********************** --
-- **************************     Code generation     *********************** --
-- **************************                         *********************** -- 
-- ************************************************************************** --

type VarStackMap = Map.Map IdentName Int -- Int: bytes location allah

whereAreYou :: VarStackMap -> IdentName -> Int 
whereAreYou m id = fromJust $ Map.lookup id m 

addToStackMap :: VarStackMap -> IdentName -> Int -> VarStackMap 
addToStackMap m id off = Map.insert id off m

type FreeBytes = Int -- Num of bytes you know what i mean

data ArmState 
  = ArmState 
  { armStackMap :: VarStackMap
  , armStackOff :: FreeBytes
  , armFreeRegs :: [ Reg ]
  , armNextLbl  :: Int 
  } deriving (Eq, Show) 

armDstReg :: ArmState -> Reg 
armDstReg = head . armFreeRegs



-- ************************************************************************** --
-- ************************                         ************************* --
-- ************************   Program Translation   ************************* --
-- ************************                         ************************* -- 
-- ************************************************************************** --

transProgram :: Program   -- * The program Augmented AST 
             -> [ Instr ] -- * The instructions the program translates to
-- | 
transProgram (Program funcs body) = progInstr
  where
    -- This is the initila state 
    arm                 = ArmState Map.empty 0 [R4 .. R10] 0
    -- Translate each function -- TODO
    (arm', funcsInstr)  = transFuncs funcs arm
    -- Main body is executed in its own scope 
    (_arm'', bodyInstr) = transScoped body arm'
    -- 
    progInstr  
      =  funcsInstr
      ++ [ DEFINE ( JumpLabel "main:" ) ]  -- Define main function label
      ++ [ PUSH [ LR ]                  ]  -- Pushes the current return address onto the stack
      ++ bodyInstr                         -- 
      ++ [ LDR R0 0                     ]  -- 
      ++ [ POP  [ PC ]                  ]  --
      ++ [ INDIR Ltorg                  ] 

    -- Translate all the functions TODO use foldMap you silly
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
-- ***********************     Function Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- | 
transFunc :: Func -> ArmState -> (ArmState, [Instr])
transFunc (Func ftype fname args body it) arm  = (arm', functInstrs)
  where 
    (arm', bodyInstrs) = transScoped body arm
    functInstrs       
      =  [ DEFINE ( JumpLabel fname ) ]  -- Define label with unique function name 
      ++ [ PUSH [ LR ]                ]  -- Pushes current return address onto stack                       
      ++ bodyInstrs                      -- The instructions from the func body
      ++ [ POP  [ PC ]                ]  -- There is always a return statementa
      ++ [ POP  [ PC ]                ]  -- Restore program counter from the stack
      ++ [ INDIR Ltorg                ] 


-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Statement Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

--
transStat :: Stat -> ArmState -> (ArmState, [Instr])

-- 
transStat SkipStat arm = (arm, []) 

-- 
transStat (FreeStat expr it) arm = error "TODO"

-- 
transStat (ReturnStat expr it) s = (s', retInstr)
  where
    (s', exprInstr) = transExpr expr s 
    -- Obtain where the expresion has been put (from the original state s)
    dst = armDstReg s
    -- Move result of expression from dst into return register
    moveResult = [ MOV R0 $ Op2'Reg dst ] 
    -- 
    retInstr = exprInstr ++ moveResult

-- To exit we load the value of the expression into the first available reg
-- and then move its value into register 0. then call BL exit  
transStat (ExitStat expr it) arm = (arm', exitInstr)
  where
    (arm', exprInstr) = transExpr expr arm
    exitInstr         
      =  exprInstr 
      ++ [ MOV R0 (Op2'Reg $ armDstReg arm') ]
      ++ [ BL (JumpLabel "exit") ] 

-- 
transStat (PrintStat expr it) arm = error "TODO"

--  
transStat (PrintlnStat expr it) arm = error "TODO"

--
transStat (ScopedStat stat) arm = transScoped stat arm

-- 
transStat (ReadStat lhs it) arm = error "TODO"

-- 
transStat (WhileStat cond body it) arm = (arm''', whileInstr)
    where
      -- Obtain next free label
      currLabel = armNextLbl arm
      -- We need 2 labels: one for the condition
      condLabel = "while_cond_" ++ show currLabel
      -- And one for the body
      bodyLabel = "while_body_" ++ show (currLabel + 1)
      -- We have used up 2 labels so we need to update the arm state
      arm' = arm { armNextLbl = currLabel + 2 }
      -- Now let's translate the loop condition expression
      (arm'', condInstr) = transExpr cond arm' 
      -- The value of the expression will be stored into the destination reg
      dst = armDstReg arm''
      -- Now for the body statements, which are scoped           
      (arm''', bodyInstr) = transScoped body arm''
      -- Now concatenate everything together
      whileInstr
        =  [ B (JumpLabel condLabel) ]          
        ++ [ DEFINE (JumpLabel bodyLabel) ]         
        ++ bodyInstr                 
        ++ [ DEFINE (JumpLabel condLabel) ]         
        ++ condInstr                
        ++ [ CMP dst $ Op2'ImmVal 0 ] 
        ++ [ BEQ (JumpLabel bodyLabel) ]    

-- 
transStat (SeqStat stat0 stat1) arm = (arm'', stat0Instr ++ stat1Instr)
  where
    (arm',  stat0Instr) = transStat stat0 arm 
    (arm'', stat1Instr) = transStat stat1 arm'

-- We are assigning a variable to a new value
transStat (AssignStat (LhsIdent id) rhs it) arm = (arm', assignInstr)
  where
    -- Work out the value of the rhs
    (arm', rhsInstr) = transRhs rhs arm 
    -- The value of rhs will be saved in the destination register, so get it!
    dst = armDstReg arm'
    -- So we want to copy whatever is now in the destination register, into
    -- the register (or stack or whatever duuuude) where the variable @id@ is.
    -- but how do we know this? we need the stackmap yeeeeey
    -- This will need to change though when we use more than one register. cos
    -- the variable might just be in a register and not somewhere on the stack
    whereIsId = whereAreYou (armStackMap arm') id 
    -- Now for the actual freaking instruction. TODO use STRB when needed
    storeMe = [ STR'Off dst SP whereIsId ]
    -- The final thing
    assignInstr = rhsInstr ++ storeMe


--
transStat (AssignStat (LhsPairElem pelem) rhs it) arm = error "TODO"

--  
transStat (AssignStat (LhsArrayElem (id, es)) rhs it) arm = error "TODO"

--
transStat (IfStat cond thens elses it) arm = (arm'''', ifInstr)
  where
    -- Obtain next free label
    currLabel = armNextLbl arm 
    -- We need 2 labels: one for the else branch
    elseLabel = "if_else_" ++ show currLabel
    -- And one for after the if
    endifLabel = "if_end_" ++ show (currLabel + 1)
    -- We have used up 2 labels so we need to update the arm state
    arm' = arm { armNextLbl = currLabel + 2 }
    -- Now let's translate the if condition expression
    (arm'', condInstr) = transExpr cond arm' 
    -- The value of the expression will be stored into the destination reg
    dst = armDstReg arm''
    -- Now for the then branch, in its own scope
    (arm''', thenInstr) = transScoped thens arm''
    -- Now for the else branch, also in its own scope
    (arm'''', elseInstr) = transScoped elses arm'''
    -- Now concatenate everything together
    ifInstr
      =  condInstr                         -- Do the condition      
      ++ [ CMP dst $ Op2'ImmVal 0 ]        -- If condition false?
      ++ [ BEQ (JumpLabel elseLabel) ]     -- If so go to else
      ++ thenInstr
      ++ [ B (JumpLabel endifLabel) ]
      ++ [ DEFINE (JumpLabel elseLabel) ]         
      ++ elseInstr
      ++ [ DEFINE (JumpLabel endifLabel) ]

--
transStat (DeclareStat vtype vname rhs it) arm = (arm''', declareInstr)
  where
    -- Translate right hand side
    (arm', rhsInstr) = transRhs rhs arm
    -- Result will be stored in dst reg so get it!
    dst = armDstReg arm -- ??? or arm' ???
    -- How many bytes does this variable take up?
    size = sizeOf vtype
    -- Where is the current stack pointer with respect the free space on the stack?
    stackOff = armStackOff arm'
    -- This is how we find the location of the variable on the stack
    offset = stackOff - size
    -- Now the new stack offset ish what 
    arm'' = arm' { armStackOff = offset }
    -- We want to remember where vname is on the stack 
    -- get the map
    stackMap = armStackMap arm''
    -- Now insert the variable wow
    arm''' = arm'' { armStackMap = addToStackMap stackMap vname offset  }
    -- We need to push the value in the dst reg onto the stack just cos
    -- TODO optimize in case of 0 offset
    pushDst = [ STR'Off dst SP offset ] -- [sp, #<off>] where 
    -- The whole instruction
    declareInstr = rhsInstr ++ pushDst

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     LHS and RHS Transl     *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --



transRhs :: AssignRhs -> ArmState -> (ArmState, [Instr])
transRhs (RhsExpr       e            ) arm = transExpr e arm          
transRhs (RhsPairElem   (Fst e)      ) arm = error "TODO"
transRhs (RhsPairElem   (Snd e)      ) arm = error "TODO"
transRhs (RhsArrayLiter (exprs)      ) arm = error "TODO"
  where
    arrayLenght = length exprs
    --arraySize   = sum (map $ (sizeOf . `typeOfExpr` it) exprs) + sizeOf (ArrayType {})
    -- Allocate memeory on the heap
    --malloc = [ LDR R0 arraySize        ] ++ 
    --         [ BL (JumpLabel "malloc") ] ++
    --         [ ]


transRhs (RhsNewPair    e e'         ) arm = error "TODO"
transRhs (RhsCall       fname  params) arm = error "TODO"

transLhs :: AssignLhs -> ArmState -> (ArmState, [Instr])
transLhs (LhsIdent id)           arm = error "TODO"
transLhs (LhsPairElem (Fst e))   arm = error "TODO"
transLhs (LhsPairElem (Snd e))   arm = error "TODO"
transLhs (LhsArrayElem (id, es)) arm = error "TODO"

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************   Expression Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

transExpr (IdentExpr id) s = (s, pushDst) -- TODO LOL
    where
      dst = armDstReg s
      offset  = whereAreYou (armStackMap s) id
      pushDst = [ LDR'Off dst SP offset ] -- [sp, #<off>] where 

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
        (arm', statInstr) = transStat stat arm{ armStackOff = freeBytes }
        -- Work out how many variables are declared in the scope of this 
        -- statement.
        freeBytes      = getBytesNeeded stat
        -- Can only add and sub from sp in chunks of 1024
        (chunks, left) = divMod freeBytes 1024 

-- | Does the statement introduce variables in its scope?
getBytesNeeded                            :: Stat -> FreeBytes -- In Bytes
getBytesNeeded (SeqStat s s'           )  =  getBytesNeeded s + getBytesNeeded s'                   
getBytesNeeded (DeclareStat vtype _ _ _)  =  sizeOf vtype
getBytesNeeded _                          =  0 

-- |
sizeOf                :: Type -> Int  -- In bytes 
sizeOf IntType        =  4                                 
sizeOf BoolType       =  1                             
sizeOf CharType       =  1                             
sizeOf StringType     =  4 -- Addresss                              
sizeOf (PairType  _)  =  4 -- Address   
sizeOf (ArrayType _)  =  4 -- Address                         
sizeOf NullType       =  0 -- ?                                
sizeOf EmptyType      =  0 -- ?
 
-- | 
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
 
-- | TODO factor out cos its used in semChecker
typeOfArrElem :: ArrayElem -> It -> Type
typeOfArrElem (id, es) it  =  deepen (length es + 1) $ fromJust (findType' id it) 
  where
    deepen 0  t             =  t
    deepen n (ArrayType t)  =  deepen (n-1) t
    deepen _  t             =  t

