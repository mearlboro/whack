module Wacc.CodeGeneration.WaccCodeGen where

import Wacc.CodeGeneration.ARM11Instructions
import Wacc.Data.DataTypes

import qualified Data.Map as Map
import Data.Array
import Data.List ( intersperse )
import Data.Maybe
import Data.Char
-- import Data.Tuple.Select ( sel3 )

--------------------------------------------------------------------------------
--  REGISTER MAP  --------------------------------------------------------------
--------------------------------------------------------------------------------

type RegMap = Map.Map IdentName Reg

findReg       :: IdentName -> RegMap -> Reg 
findReg m id  =  fromJust $ Map.lookup m id 

type AvailRegs = [ Register ]

type ArmState = (RegMap, [Label], Int, AvailRegs) -- Int is number of labels used

type ExitCode = Int

nextLabel :: ExitCode -> Label
nextLabel i = JumpLabel $ "L" ++ show i

--------------------------------------------------------------------------------
--  PROGRAM  -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | 
-- TODO!
-- TODO!
-- TODO!
-- TODO!
-- TODO!
-- TODO!
-- TODO!
-- TODO!
-- DO SOMETHING WITH FUNCS


transProgram :: Program        -- | The program Augmented AST
             -> ( [ Instr ] ,  -- | Program state, to extract the string labels
                  ArmState  ,  -- | The instructions the program translates to
                  ExitCode  )  -- | An integer value containing the exit int
transProgram (Program funcs body) 
    = (instrs, s, x)
      where
        instrs 
          =  [ DEFINE ( JumpLabel "main:" ) ]  -- Define main function label
          ++ [ PUSH [ LR ]                  ]  -- Pushes the current return address onto the stack
          ++ instrs'                           -- 
          ++ [ LDR R0 0                     ]  -- 
          ++ [ POP  [ PC ]                  ]  --
          ++ [ INDIR Ltorg                  ] 
        (instrs', s, x) = transStat body state0 (-1)
        state0          = (Map.empty, [], 0, [R4 .. R10])


getExit :: ( [ Instr ], ArmState, ExitCode )    -- computed by transProgram
        ->                        ExitCode      -- extracts exit code
getExit (_, _, x)
    = x  -- sel3

-- TODO rename lol
makePretty :: ( [ Instr ], ArmState, ExitCode ) -- computed by transProgram
           ->   String                          -- printable compiled program
makePretty (instrs, s@(_, ls, _, _), _) 
    =  show ( INDIR Text )  ++ "\n"                  
    ++ ( concat $ intersperse "\n\t" $ map show ls  ) 
    ++ show ( INDIR ( Global ( JumpLabel "main" ) ) ) ++ "\n"
    ++ ( concat $ intersperse "\n\t" $ map show instrs )



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- | The following transform functions will get the AST of a statement, expression,
--   or function, an ARM state and the exit code returned so far. 
--   It will return a set of instructions instead of the given piece of program and
--   an updated state and exit code, wrapped in a tuple. The exit code starts as -1 
--   and will only be altered in case of ExitStmt.

-- trans____ :: *AST*      -- | Func, Stat, Expr to be compiled
--           -> ArmState   -- | Describing the state before transforming
--           -> ExitCode   -- | The exit code thrown so far
--           -> ( [ Instr ], -- | The instructions compiled out of the input
--                ArmState,  -- | Describing the new state
--                ExitCode ) -- | The new return code

-- properties of trans : normal, deterministic, total

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************     Function Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- | 
transFunc :: Func      -> ArmState -> ExitCode
          -> ( [ Instr ], ArmState,   ExitCode )
              
transFunc (Func ftype fname args body it) s x 
  = (functInstrs, s', x')
    where
      functInstrs       
        =  [ DEFINE ( JumpLabel fname ) ]  -- Define label with unique function name 
        ++ [ PUSH [ LR ]                ]  -- Pushes current return address onto stack                       
        ++ bodyInstrs                      -- The instructions obtained from the func body
        ++ [ POP  [ PC ]                ]  -- Restore program counter from the stack
      (bodyInstrs, s', x') = transStat body s x



-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************    Statement Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- | 
transStat :: Stat      -> ArmState -> ExitCode
          -> ( [ Instr ], ArmState,   ExitCode )

-- |
transStat SkipStat s x 
  = ([], s, x)

-- |
--transStat (FreeStat e it) s@(m, ls, rs)s = error "TODO"
--  where
--    is = transExpr e rs s ++  [{- Special instructions to free expression? -}]

---- | 
transStat (ExitStat e _) s x 
  = (exitInstr, s', x')
    where
      (exprInstr, s') = transExpr e s x
      exitInstr       = exprInstr ++ [ BL ( JumpLabel "exit" ) ]

-- | 
--transStat (ReturnStat e _) s@(m, ls, rs) =  ((m, l', rs), instr)
--  where
--    (exprInstr, l') = transExpr e rs m l 
--    instr           = [ BL "exit" ]


---- | 
--transStat (PrintStat e _) s@(m, ls, rs)  =  ((m, l', rs), instr)
--  where
--    (exprInstr, l') = transExpr e rs m l 
--    instr           = exprInstr ++ [{- Special instructions to print? -}]


---- |                
--transStat (PrintlnStat e it) s@(m, ls, rs)  =  ((m, l', rs), instr)
--  where
--    (exprInstr, l') = transExpr e rs m l 
--    instr           = exprInstr ++ [{- Special instructions to print? -}]


-- |                 
transStat (ScopedStat stat) s x 
  = transStat stat s x


---- |                               
--transStat (ReadStat lhs it) h s rs = error "TODO" 


-- |                    
transStat (WhileStat cond body it) s@(m, ls, i, rs@(dst:_)) x 
  = (whileInstrs, s'', x'')
    where
      label0                  = nextLabel i 
      label1                  = nextLabel (i + 1)
      (condInstrs, s'', x'')  = transExpr cond s'                 x'
      (bodyInstr,  s',  x' )  = transStat body (m, ls, i + 2, rs) x

      whileInstrs             =  [ B label0      ]          
                              ++ [ DEFINE label1 ]         
                              ++ bodyInstr                 
                              ++ [ DEFINE label0 ]         
                              ++ condInstrs                 
                              ++ [ CMP dst $ Op2'ImmVal 0 ] 
                              ++ [ BEQ label1    ]

-- |             
transStat (SeqStat stat stat') s x
  = (stat0Instr ++ stat1Instr, s'', x'')
    where
      (stat0Instr, s',  x' ) = transStat stat  s  x
      (stat1Instr, s'', x'') = transStat stat' s' x'


-- |        
transStat (DeclareStat vtype vname rhs it) s x 
  = (declInstr, s', x)
    where
      instrSTR :: Type -> (Rd -> Integer -> Instr)
      instrSTR t = if sizeOfType t == 1 then STRB else STR

      size                                = sizeOfType vtype
      (rhsInstr, s'@(_, _, _, dst:_), x') = transRHS rhs s x -- TODO: que/
      declInstr                           =  [ SUB SP SP $ Op2'ImmVal size ] -- Reserve space on the stack
                                          ++ rhsInstr
                                          ++ [ instrSTR vtype SP dst ]
                                          ++ [ ADD SP SP $ Op2'ImmVal size ]

    -- Check type of varialbe. If primitive save its value into register

---- |                  
--transStat (AssignStat lhs rhs it) h s rs =  error "TODO" 

---- |              
--transStat (IfStat cond sthen selse it) h s rs = error "TODO" 

---- |
--transLHS :: AssignLhs -> [ Instr ] 
--transLHS (LhsIdent id) = error "TODO"              
--transLHS (LhsPairElem pelem) = error "TODO"                 
--transLHS (LhsArrayElem (ArrayElem id exprs)) = error "TODO" 

transRHS = error "TODO"

-- ************************************************************************** --
-- ***********************                            *********************** --
-- ***********************   Expression Translation   *********************** --
-- ***********************                            *********************** -- 
-- ************************************************************************** --

-- |
transExpr :: Expr       -> ArmState -> ExitCode
          -> ( [ Instr ],  ArmState,   ExitCode )


-- | Put the value of boolean @b@ into the first avaialble register @dst@
transExpr (BoolLiterExpr b) s@(_, _, _, (dst:_)) x 
  = ( [ MOV dst (Op2'ImmVal $ if b then 1 else 0) ], s, x)


-- | Put the corresponding integer value of @c@ into the destination reg @dst@
transExpr (CharLiterExpr c) s@(_, _, _, (dst:_)) x
  = ( [ MOV dst (Op2'ImmVal $ ord c) ], s, x)  -- todO use LDR


-- | Lookup what register variable @id@ is in, and copy its content in @dst@ reg
transExpr (IdentExpr id) s@(m, _, _, (dst:_)) x
  = ([ MOV dst (Op2'Reg src) ], s, x) -- TODO LOL
    where
      src = findReg id m 


-- | Evaluates the expression and places it in the dest reg(dst) , performs the unary operation on that reg 
transExpr (UnaryOperExpr op e) s@(m, _, _, (dst:_)) x
  = (exprInstr ++ unopInstr, s'', x'')
    where
      ( exprInstr, s',  x' ) = transExpr e  s  x
      ( unopInstr, s'', x'') = transUnOp op s' x'

-- |
transExpr (ParenthesisedExpr e) s x 
  = transExpr e s x

-- |
transExpr (IntLiterExpr i) s@(_, _, _, rs@(dst:_)) x = 
   ([ LDR dst i , MOV R0 (Op2'Reg dst) ], s, x)

-- |
transExpr (StrLiterExpr str) s@(m, ls, i, rs@(dst:_)) x = error "NEED A DIRECTIVE FIELD IN ARM STATE" 

-- |
transExpr PairLiterExpr s@(m, ls, i, rs@(dst:_)) x = error "TODO" 

-- | TODO make ArrayElem a type synonym PLSSSSSSS
transExpr (ArrayElemExpr (ArrayElem ident exprs)) s@(m, ls, i, rs@(dst:_)) x = error "TODO"  

-- |
transExpr (BinaryOperExpr op e e') s@(m, ls, i, rs@(dst:_)) x = error "TODO"  
 


-- | Generate instructions for a unary operator
transUnOp :: UnaryOper -> ArmState -> ExitCode
          -> ( [ Instr ], ArmState,   ExitCode )
transUnOp NotUnOp s@(m, ls, i, rs@(dst:_)) x = (unopInstrs, (m, ls, i + 1, rs), x)
  where
    label      =  nextLabel i
    unopInstrs =  [ CBZ dst label ]          ++
                  [ MOV dst $ Op2'ImmVal 0 ] ++
                  [ DEFINE label ]           ++
                  [ MOV dst $ Op2'ImmVal 1 ]


transUnOp LenUnOp s@(m, ls, i, rs@(dst:_)) x = error "TODO"
transUnOp OrdUnOp s@(m, ls, i, rs@(dst:_)) x = error "TODO"
transUnOp ChrUnOp s@(m, ls, i, rs@(dst:_)) x = error "TODO"
transUnOp NegUnOp s@(m, ls, i, rs@(dst:_)) x = error "TODO"


----------------------------------------------------------------------------------


--extractVar                       :: Expr -> Store -> Variable 
--extractVar (IdentExpr ident) s   =  lookupStore s ident
--extractVar _                 _   =  error "extractIdent: Expecting IdentExpr"



-- sizeof :: Expr -> Word -- How many consecutive addresses does it take 
sizeof (BoolLiterExpr     _    ) = 1            
sizeof (CharLiterExpr     _    ) = 1            
sizeof (IdentExpr         name ) = error "WHHHHHHHAT"
sizeof (UnaryOperExpr _ _      ) = 1
sizeof (ParenthesisedExpr e    ) = sizeof e               
sizeof (IntLiterExpr      _    ) = 1               
sizeof (StrLiterExpr      str  ) = length str              
sizeof (PairLiterExpr          ) = 0  -- ?                
sizeof (ArrayElemExpr (ArrayElem arrName exprs)) = error "TODO"           
sizeof (BinaryOperExpr  _ e1 e2) = error "TODO"  

sizeOfType :: Type -> Int  -- In bytes 
sizeOfType IntType       = 4                                 
sizeOfType BoolType      = 1                             
sizeOfType CharType      = 1                             
sizeOfType StringType    = 4  -- Addresss                              
sizeOfType (PairType  _) = 4 -- Address   
sizeOfType (ArrayType _) = 4 -- Address                         
sizeOfType NullType      = 0 -- ?                                
sizeOfType EmptyType     = 0 -- ?


