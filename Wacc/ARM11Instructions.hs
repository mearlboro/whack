module ARM11Instructions where

import Data.List (intersperse)

-- The available registers of ARM11
data Register 
  = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 
  | SP -- R13 | Stack Pointer
  | LR -- R14 | Link Register (which holds return addresses)
  | PC -- R15 | Program Counter

-- Data synonyms
data Rd = Register 
data Rn = Register 
data Rm = Register 
data Rs = Register 

-- This is used as an operand in many instructions
data Operand2
  = Op2'ImmVal Imm8m -- Immediate value                              | #<imm8m>                  
  | Op2'Reg    Rm    -- Register                                     | Rm  
  | Op2'LSL    Rm Rs -- Register, logical    shift left  by register | Rm, LSL Rs    
  | Op2'LSR    Rm Rs -- Register, logical    shift right by register | Rm, LSR Rs    
  | Op2'ASR    Rm Rs -- Register, arithmetic shift right by register | Rm, ASR Rs    
  | Op2'ROR    Rm Rs -- Register, rotate           right by register | Rm, ROR Rs  
  | Op2'LSL'Sh Rm Sh -- Register, logical    shift left  by constant | Rm, LSL #<shift> | Allowed shifts 0-31    
  | Op2'LSR'Sh Rm Sh -- Register, logical    shift right by constant | Rm, LSR #<shift> | Allowed shifts 1-32    
  | Op2'ASR'Sh Rm Sh -- Register, arithmetic shift right by constant | Rm, ASR #<shift> | Allowed shifts 1-32    
  | Op2'ROR'Sh Rm Sh -- Register, rotate           right by constant | Rm, ROR #<shift> | Allowed shifts 1-31   

-- A 32-bit constant, formed by right-rotating an 8-bit value by an even number of bits.   
type Imm8m = Int 

-- Shift constant, whose range depends on the instruction it appears in
type Sh = Int 

-- ARM11 instruction
data Instr 
  
  -- Arithemtics
  = ADD  Rd Rn Operand2 -- Add              | ADD{S} Rd, Rn, <Operand2> | Rd := Rn + Operand2
  | SUB  Rd Rn Operand2 -- Subtract         | SUB{S} Rd, Rn, <Operand2> | Rd := Rn – Operand2
  | MUL  Rd Rm Rs       -- Multiply         | MUL{S} Rd, Rm, Rs         | Rd := (Rm * Rs)[31:0]
  | MLA  Rd Rm Rs Rn    -- Mul & accumulate | MLA{S} Rd, Rm, Rs, Rn     | Rd := (Rn + (Rm * Rs))[31:0]
  | SDIV Rd Rn Rm       -- Divide signed    | SDIV   Rd, Rn, Rm         | Rd := Rn / Rm
  | UDIV Rd Rn Rm       -- Divide unsigned  | UDIV   Rd, Rn, Rm         | Rd := Rn / Rm
  
  -- Moving     
  | MOV Rd Operand2     -- Move | MOV{S} Rd, <Operand2> | Rd := Operand2
  
  -- Shifting
  | ASR    Rd Rm Rs     -- Arithmetic shift right by register | ASR{S} Rd, Rm, Rs | Rd := ASR(Rm, Rs) 
  | LSL    Rd Rm Rs     -- Logical    shift left  by register | LSL{S} Rd, Rm, Rs | Rd := LSL(Rm, Rs) 
  | LSR    Rd Rm Rs     -- Logical    shift right by register | LSR{S} Rd, Rm, Rs | Rd := LSR(Rm, Rs) 
  | ROR    Rd Rm Rs     -- Rotate           right by register | ROR{S} Rd, Rm, Rs | Rd := ROR(Rm, Rs) 
  | ASR'Sh Rd Rm Sh     -- Arithmetic shift right by constant | ASR{S} Rd, Rm, sh | Rd := ASR(Rm, sh) | Allowed shifts 0-31
  | LSL'Sh Rd Rm Sh     -- Logical    shift left  by constant | LSL{S} Rd, Rm, sh | Rd := LSL(Rm, sh) | Allowed shifts 1-32
  | LSR'Sh Rd Rm Sh     -- Logical    shift right by constant | LSR{S} Rd, Rm, sh | Rd := LSR(Rm, sh) | Allowed shifts 1-32
  | ROR'Sh Rd Rm Sh     -- Rotate           right by constant | ROR{S} Rd, Rm, sh | Rd := ROR(Rm, sh) | Allowed shifts 1-31       
  
  -- Compares
  | CMP Rn Operand2     -- Compare          | CMP Rn, <Operand2> | Update CPSR flags on Rn – Operand2
  | CMN Rn Operand2     -- Compare negative | CMN Rn, <Operand2> | Update CPSR flags on Rn + Operand2
  
  -- Logical              
  | TST Rn    Operand2  -- Test             | TST Rn,     <Operand2> | Update CPSR flags on Rn AND Operand2
  | TEQ Rn    Operand2  -- Test equivalence | TEQ Rn,     <Operand2> | Update CPSR flags on Rn EOR Operand2
  | AND Rd Rn Operand2  -- AND              | AND Rd, Rn, <Operand2> | Rd := Rn AND Operand2
  | EOR Rd Rn Operand2  -- EOR              | EOR Rd, Rn, <Operand2> | Rd := Rn EOR Operand2
  | ORR Rd Rn Operand2  -- ORR              | ORR Rd, Rn, <Operand2> | Rd := Rn OR Operand2
  | ORN Rd Rn Operand2  -- ORN              | ORN Rd, Rn, <Operand2> | Rd := Rn OR NOT Operand2
  | BIC Rd Rn Operand2  -- Bit Clear        | BIC Rd, Rn, <Operand2> | Rd := Rn AND NOT Operand2
  

instance Show Register where
  show R0  = "r0"   
  show R1  = "r1"   
  show R2  = "r2"   
  show R3  = "r3"   
  show R4  = "r4"   
  show R5  = "r5"   
  show R6  = "r6"   
  show R7  = "r7"   
  show R8  = "r8"   
  show R9  = "r9"  
  show R10 = "r10"    
  show R11 = "r11"    
  show R12 = "r12"    
  show SP  = "r13" -- R13 | Stack Pointer
  show LR  = "r14" -- R14 | Link Register (which holds return addresses)
  show PC  = "r15" -- R15 | Program Counter


instance Show Operand2 where
    show Op2'ImmVal imm8m = "#" ++ show imm8m   -- #<imm8m>                  
    show Op2'Reg    rm    = show  rm            -- Rm  
    show Op2'LSL    rm rs = show' rm "LSL "  rs -- Rm, LSL Rs    
    show Op2'LSR    rm rs = show' rm "LSR "  rs -- Rm, LSR Rs    
    show Op2'ASR    rm rs = show' rm "ASR "  rs -- Rm, ASR Rs    
    show Op2'ROR    rm rs = show' rm "ROR "  rs -- Rm, ROR Rs  
    show Op2'LSL'Sh rm sh = show' rm "LSL #" sh -- Rm, LSL #<shift> | Allowed shifts 0-31
    show Op2'LSR'Sh rm sh = show' rm "LSR #" sh -- Rm, LSR #<shift> | Allowed shifts 1-32
    show Op2'ASR'Sh rm sh = show' rm "ASR #" sh -- Rm, ASR #<shift> | Allowed shifts 1-32
    show Op2'ROR'Sh rm sh = show' rm "ROR #" sh -- Rm, ROR #<shift> | Allowed shifts 1-31
  where
    show' :: Show a => a -> String -> a -> String 
    show' op mne op' = show op ++ ", " ++ mne ++ show op'

instance Show Instr where
    show ADD    rd rn  op2    = show' "ADD" [ rd, rn,  op2     ] -- ADD{S} Rd, Rn,        <Operand2>
    show SUB    rd rn  op2    = show' "SUB" [ rd, rn,  op2     ] -- SUB{S} Rd, Rn,        <Operand2>
    show MUL    rd rm  rs     = show' "MUL" [ rd, rm,  rs      ] -- MUL{S} Rd, Rm,        Rs        
    show MLA    rd rm  rs  rn = show' "MLA" [ rd, rm,  rs,  rn ] -- MLA{S} Rd, Rm,        Rs,         Rn    
    show SDIV   rd rn  rm     = show' "SDI" [ rd, rn,  rm      ] -- SDIV   Rd, Rn,        Rm        
    show UDIV   rd rn  rm     = show' "UDI" [ rd, rn,  rm      ] -- UDIV   Rd, Rn,        Rm        
    show MOV    rd op2        = show' "MOV" [ rd, op2,         ] -- MOV{S} Rd, <Operand2>
    show ASR    rd rm  rs     = show' "ASR" [ rd, rm,  rs      ] -- ASR{S} Rd, Rm,        Rs  
    show LSL    rd rm  rs     = show' "LSL" [ rd, rm,  rs      ] -- LSL{S} Rd, Rm,        Rs  
    show LSR    rd rm  rs     = show' "LSR" [ rd, rm,  rs      ] -- LSR{S} Rd, Rm,        Rs  
    show ROR    rd rm  rs     = show' "ROR" [ rd, rm,  rs      ] -- ROR{S} Rd, Rm,        Rs  
    show ASR'Sh rd rm  sh     = show' "ASR" [ rd, rm,  sh      ] -- ASR{S} Rd, Rm,        sh  
    show LSL'Sh rd rm  sh     = show' "LSL" [ rd, rm,  sh      ] -- LSL{S} Rd, Rm,        sh  
    show LSR'Sh rd rm  sh     = show' "LSR" [ rd, rm,  sh      ] -- LSR{S} Rd, Rm,        sh  
    show ROR'Sh rd rm  sh     = show' "ROR" [ rd, rm,  sh      ] -- ROR{S} Rd, Rm,        sh  
    show CMP    rn op2        = show' "CMP" [ rn, op2,         ] -- CMP    Rn, <Operand2>  
    show CMN    rn op2        = show' "CMN" [ rn, op2,         ] -- CMN    Rn, <Operand2>  
    show TST    rn op2        = show' "TST" [ rn, op2,         ] -- TST    Rn, <Operand2>
    show TEQ    rn op2        = show' "TEQ" [ rn, op2,         ] -- TEQ    Rn, <Operand2>
    show AND    rd rn  op2    = show' "AND" [ rd, rn,  op2     ] -- AND    Rd, Rn,        <Operand2>
    show EOR    rd rn  op2    = show' "EOR" [ rd, rn,  op2     ] -- EOR    Rd, Rn,        <Operand2>
    show ORR    rd rn  op2    = show' "ORR" [ rd, rn,  op2     ] -- ORR    Rd, Rn,        <Operand2>
    show ORN    rd rn  op2    = show' "ORN" [ rd, rn,  op2     ] -- ORN    Rd, Rn,        <Operand2>
    show BIC    rd rn  op2    = show' "BIC" [ rd, rn,  op2     ] -- BIC    Rd, Rn,        <Operand2>
  where
    show'          :: Show a => String -> [a] -> String    
    show' mne ops  =  mne ++ " " ++ intersperse ", " (map show ops)

-- All test
-- modules
-- error messages
-- try problem =)4
-- * line number 





