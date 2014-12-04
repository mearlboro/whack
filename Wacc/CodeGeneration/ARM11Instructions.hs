module Wacc.CodeGeneration.ARM11Instructions where

import Data.List (intersperse)
import qualified Data.Map as Map
 
import Wacc.Data.SymbolTable
import Wacc.Data.DataTypes


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- The available registers of ARM11
data Register
  = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12
  | SP -- R13 | Stack Pointer
  | LR -- R14 | Link Register (which holds return addresses)
  | PC -- R15 | Program Counter
  deriving (Enum)

type Reg = Register

type Rd = Register
type Rn = Register
type Rm = Register
type Rs = Register

-- List of registers | <reglist> = {Ri, Rj, Rn,...}
type RegList = [Register]


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
  show SP  = "{sp}" -- R13 | Stack Pointer
  show LR  = "{lr}" -- R14 | Link Register (which holds return addresses)
  show PC  = "{pc}" -- R15 | Program Counter

showRegs [] = ""
showRegs rs = concat $ intersperse ", " $ map show rs


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- The state
data ArmState 
  = ArmState { stackMap      :: Map.Map IdentName Int
             , stackOffset   :: Int
             , availableRegs :: [ Reg ]
             , numJumpLabels :: Int
             , dataLabels    :: [ Label ]
             , predefLabels  :: [ Label ]
             }


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Labels 
type LabelName = String
data Label 
  = JumpLabel   LabelName           -- Name of a label in ARM
  | DataLabel   LabelName String    -- msg_i: label for strings
  | PredefLabel LabelName [ Instr ] 


instance Show Label where
  show ( JumpLabel   l   ) = l 
  show ( DataLabel   l _ ) = l 
  show ( PredefLabel l _ ) = l



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Directives
data Directive
  = Text
  | Data
  | Global Label -- TODO: Should be just JumpLabel 
  | Ltorg


instance Show Directive where
  show Text       = ".text"
  show Data       = ".data"
  show (Global l) = ".global " ++ show l
  show Ltorg      = "\t.ltorg"



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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


instance Show Operand2 where
  show (Op2'ImmVal imm8m) = "#" ++ show imm8m     -- #<imm8m>
  show (Op2'Reg    rm   ) = show rm               -- Rm
  show (Op2'LSL    rm rs) = showOp2 rm "LSL "  rs -- Rm, LSL Rs
  show (Op2'LSR    rm rs) = showOp2 rm "LSR "  rs -- Rm, LSR Rs
  show (Op2'ASR    rm rs) = showOp2 rm "ASR "  rs -- Rm, ASR Rs
  show (Op2'ROR    rm rs) = showOp2 rm "ROR "  rs -- Rm, ROR Rs
  show (Op2'LSL'Sh rm sh) = showOp2 rm "LSL #" sh -- Rm, LSL #<shift> | Allowed shifts 0-31
  show (Op2'LSR'Sh rm sh) = showOp2 rm "LSR #" sh -- Rm, LSR #<shift> | Allowed shifts 1-32
  show (Op2'ASR'Sh rm sh) = showOp2 rm "ASR #" sh -- Rm, ASR #<shift> | Allowed shifts 1-32
  show (Op2'ROR'Sh rm sh) = showOp2 rm "ROR #" sh -- Rm, ROR #<shift> | Allowed shifts 1-31

showOp2             :: (Show a, Show b) => a -> String -> b -> String
showOp2 op mne op'  =  show op ++ ", " ++ mne ++ show op'



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- ARM11 instructions
data Instr

  -- Arithemtics
  = ADD  Rd Rn Operand2 -- Add              | ADD{S} Rd, Rn, <Operand2> | Rd := Rn + Operand2
  | SUB  Rd Rn Operand2 -- Subtract         | SUB{S} Rd, Rn, <Operand2> | Rd := Rn – Operand2
  | MUL  Rd Rm Rs       -- Multiply         | MUL{S} Rd, Rm, Rs         | Rd := (Rm * Rs)[31:0]
  | MLA  Rd Rm Rs Rn    -- Mul & accumulate | MLA{S} Rd, Rm, Rs, Rn     | Rd := (Rn + (Rm * Rs))[31:0]
  | SDIV Rd Rn Rm       -- Divide signed    | SDIV   Rd, Rn, Rm         | Rd := Rn / Rm
  | UDIV Rd Rn Rm       -- Divide unsigned  | UDIV   Rd, Rn, Rm         | Rd := Rn / Rm

  -- Moving
  | MOV     Rd Operand2     -- Move | MOV{S} Rd, <Operand2> | Rd := Operand2
  | MOV'Reg Rd Rs           -- ... TODO:

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

  -- Branching (jump)
  | B       Label  -- Branch                       | B        <label> | PC := label. label is this instruction ±32MB (T2: ±16MB, T: –252 - +256B)
  | BL      Label  -- Branch with link             | BL       <label> | LR := address of next instruction, PC := label. label is this instruction ±32MB (T2: ±16MB).
  | BEQ     Label
  | CBZ  Rn Label  -- Compare, branch if zero      | CBZ  Rn, <label> | If Rn == 0 then PC := label. label is (this instruction + 4-130).
  | CBNZ Rn Label  -- Compare, branch if non-zero  | CBNZ Rn, <label> | If Rn != 0 then PC := label. label is (this instruction + 4-130).
  | DEFINE  Label  -- This is NOT an ARM instruction -- It is just to tell where a label is defined

  -- Push & Pop
  | PUSH RegList  --  Push | POP  <reglist> | <reglist> = {Ri, Rj, Rn,...}
  | POP  RegList  --  Pop  | PUSH <reglist> | <reglist> = {Ri, Rj, Rn,...}

  -- Load and Store
  | LDR  Rd Integer    -- | LDR rd, =numeric constant
  | STR  Rd Integer    -- |
  | STRB Rd Integer    -- |

  | LDR'Lbl  Rd Label  -- | LDR rd, =label
  | STR'Lbl  Rd Label  -- |
  | STRB'Lbl Rd Label  -- |

  | STR'Reg  Rd Rd     -- |
  | STRB'Reg Rd Rd     -- |

  -- Directive
  | INDIR Directive


instance Show Instr where
    show (ADD    rd rn  op2   ) = "\tADD "  ++ show rd ++ ", " ++ show rn  ++ ", " ++ show op2                   -- ADD{S} Rd, Rn,        <Operand2>
    show (SUB    rd rn  op2   ) = "\tSUB "  ++ show rd ++ ", " ++ show rn  ++ ", " ++ show op2                   -- SUB{S} Rd, Rn,        <Operand2>
    show (MUL    rd rm  rs    ) = "\tMUL "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show rs                    -- MUL{S} Rd, Rm,        Rs
    show (MLA    rd rm  rs rn ) = "\tMLA "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show rs ++ ", " ++ show rn -- MLA{S} Rd, Rm,        Rs,         Rn
    show (SDIV   rd rn  rm    ) = "\tSDI "  ++ show rd ++ ", " ++ show rn  ++ ", " ++ show rm                    -- SDIV   Rd, Rn,        Rm
    show (UDIV   rd rn  rm    ) = "\tUDI "  ++ show rd ++ ", " ++ show rn  ++ ", " ++ show rm                    -- UDIV   Rd, Rn,        Rm
    show (MOV    rd op2       ) = "\tMOV "  ++ show rd ++ ", " ++ show op2                                       -- MOV{S} Rd, <Operand2>
    show (MOV'Reg   rd  rs    ) = "\tMOV "  ++ show rd ++ ", " ++ show rs                                        -- MOV{S} Rd, <Operand2>
    show (ASR    rd rm  rs    ) = "\tASR "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show rs                    -- ASR{S} Rd, Rm,        Rs
    show (LSL    rd rm  rs    ) = "\tLSL "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show rs                    -- LSL{S} Rd, Rm,        Rs
    show (LSR    rd rm  rs    ) = "\tLSR "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show rs                    -- LSR{S} Rd, Rm,        Rs
    show (ROR    rd rm  rs    ) = "\tROR "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show rs                    -- ROR{S} Rd, Rm,        Rs
    show (ASR'Sh rd rm  sh    ) = "\tASR "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show sh                    -- ASR{S} Rd, Rm,        sh
    show (LSL'Sh rd rm  sh    ) = "\tLSL "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show sh                    -- LSL{S} Rd, Rm,        sh
    show (LSR'Sh rd rm  sh    ) = "\tLSR "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show sh                    -- LSR{S} Rd, Rm,        sh
    show (ROR'Sh rd rm  sh    ) = "\tROR "  ++ show rd ++ ", " ++ show rm  ++ ", " ++ show sh                    -- ROR{S} Rd, Rm,        sh
    show (CMP    rn op2       ) = "\tCMP "  ++ show rn ++ ", " ++ show op2                                       -- CMP    Rn, <Operand2>
    show (CMN    rn op2       ) = "\tCMN "  ++ show rn ++ ", " ++ show op2                                       -- CMN    Rn, <Operand2>
    show (TST    rn op2       ) = "\tTST "  ++ show rn ++ ", " ++ show op2                                       -- TST    Rn, <Operand2>
    show (TEQ    rn op2       ) = "\tTEQ "  ++ show rn ++ ", " ++ show op2                                       -- TEQ    Rn, <Operand2>
    show (AND    rd rn  op2   ) = "\tAND "  ++ show rd ++ ", " ++ show rn  ++ ", " ++ show op2                   -- AND    Rd, Rn,        <Operand2>
    show (EOR    rd rn  op2   ) = "\tEOR "  ++ show rd ++ ", " ++ show rn  ++ ", " ++ show op2                   -- EOR    Rd, Rn,        <Operand2>
    show (ORR    rd rn  op2   ) = "\tORR "  ++ show rd ++ ", " ++ show rn  ++ ", " ++ show op2                   -- ORR    Rd, Rn,        <Operand2>
    show (ORN    rd rn  op2   ) = "\tORN "  ++ show rd ++ ", " ++ show rn  ++ ", " ++ show op2                   -- ORN    Rd, Rn,        <Operand2>
    show (BIC    rd rn  op2   ) = "\tBIC "  ++ show rd ++ ", " ++ show rn  ++ ", " ++ show op2                   -- BIC    Rd, Rn,        <Operand2>
    show (B      l            ) = "\tB "    ++ show l
    show (BL     l            ) = "\tBL "   ++ show l
    show (BEQ    l            ) = "\tBEQ "  ++ show l
    show (CBZ    rn l         ) = "\tCBZ "  ++ show rn ++ ", " ++ show l
    show (CBNZ   rn l         ) = "\tCBNZ " ++ show rn ++ ", " ++ show l
    show (DEFINE l            ) = "\t" ++ show l 
    show (PUSH   regs         ) = "\tPUSH " ++ showRegs regs
    show (POP    regs         ) = "\tPOP "  ++ showRegs regs
    show (LDR    rd n         ) = "\tLDR "  ++ show rd ++ ", =" ++ show n
    show (STR    rd n         ) = "\tSTR "  ++ show rd ++ ", =" ++ show n
    show (STRB   rd n         ) = "\tSTRB " ++ show rd ++ ", =" ++ show n
    show (LDR'Lbl   rd l      ) = "\tLDR "  ++ show rd ++ ", =" ++ show l
    show (STR'Lbl   rd l      ) = "\tSTR "  ++ show rd ++ ", =" ++ show l
    show (STRB'Lbl  rd l      ) = "\tSTRB " ++ show rd ++ ", =" ++ show l
    show (STR'Reg   rd rs     ) = "\tSTR "  ++ show rd ++ ", = [" ++ show rs ++ " ]" 
    show (STRB'Reg  rd rs     ) = "\tSTRB " ++ show rd ++ ", = [" ++ show rs ++ " ]" 
    show (INDIR  dir          ) = show dir    



