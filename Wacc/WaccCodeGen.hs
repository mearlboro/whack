module Wacc.WaccCodeGen where

import Wacc.ARM11Instructions
import Wacc.WaccDataTypes
import qualified Data.Map as Map


{-

CANNOT FIND MODULES... WHY??????

-}

--------------------------------------------------------------------------------
--  HEAP  ----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | One word of information yey, 32 bits
type Word = Int   
-- | Address on the heap
type Address = Word 

-- | The address of the first free memory word on the heap
type HeapPtr = Address 
-- | The heap maps addresses to values in memory 
type Heap = (HeapPtr, Array Address Word)  

-- | RAM memory on the Raspberry Pi
MAX_MEMORY = 256 * 2^20 - 1 -- 256 Megabytes

-- | Contructrs a new empyt heap
emptyHeap = (0, array (0, MAX_MEMORY) [ (i, 0) | i <- [0..MAX_MEMORY] ])

-- | Frees @n@ consecutive words from memory starting from @adr@ 
--   Pre: n > 0
heapFree          
  :: Heap    -- Starting heap
  -> Address -- First address to free
  -> Int     -- How many consecutive memory words to free
  -> Heap    -- The new heap
heapFree h adr n  =  error "GOOD LUCK!"  

-- | Writes the words starting from the heap pointer onwards
heapWrite               
 :: Heap     -- Starting heap
 -> [ Word ] -- Values to write in consecutive memory
 -> Heap     -- The new heap
heapWrite (prt, mem) xs  =  error "GOOD LUCK"

-- | Reads @n@ words of data from the heap starting from @adr@
--   Pre: n > 0
heapRead 
  :: Heap     -- Given a heap
  -> Address  -- The addresss to start reading from
  -> Int      -- How many words to read
  -> [ Word ] -- The memory raed from the heap
heapRead h adr n  =  error "GOOD LUCK"

--------------------------------------------------------------------------------
--  STORE  ---------------------------------------------------------------------
--------------------------------------------------------------------------------

type Length = Int 

type Store = [ (IdentName, Word) ]

lookUpStore       :: Store -> IdentName -> Word32 
lookUpStore id s  =  fromJust $ lookup id s 

data Variable  
  = V'Prim  IdentName             
  | V'Pair  IdentName Type Type   
  | V'Array IdentName Type Length  -- Address 

lookupStore :: Store -> IdentName -> Variable 
lookupStore s name = head . filter ((==) name . varNameOf) $ s

varNameOf                     :: Variable -> IndetName
varNameOf V'Prim  name _      =  name             
varNameOf V'Pair  name _ _ _  =  name
varNameOf V'Array name _ _ _  =  name

--------------------------------------------------------------------------------
--  PROGRAM  -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | 
transProgram :: Program -> [ Instr ]
transProgram p = error "TODO"

--------------------------------------------------------------------------------
--  FUNC  ----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | 
transFunc :: Func -> Heap -> Store -> [ Instr ]
transFunc p = error "TODO"

--------------------------------------------------------------------------------
--  STAT  ----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | 
transStat 
  :: Stat 
  -> Heap
  -> Store 
  -> [ Reg ] 
  -> (Heap, Store, [ Reg ], [ Intr ])

-- |
transStat SkipStat h s rs = (h, s, rs, [])

-- |
transStat (FreeStat e it) h s rs = error "TODO"
  where
    is = transExpr e rs s ++  [{- Special instructions to free expression? -}]

-- | 
transStat (ExitStat e it) h s rs = error "TODO"
  where
    is = transExpr e rs h ++ [{- Special instructions to exit program? -}]

-- | 
transStat (ReturnStat e it) h s rs = error "TODO"
  where
    is = transExpr e rs h ++ []

-- | 
transStat (PrintStat e it) h s rs = error "TODO" 

-- |                
transStat (PrintlnStat e it) h s rs = error "TODO" 

-- |                 
transStat (ScopedStat s) h s rs = error "TODO"  

-- |                               
transStat (ReadStat lhs it) h s rs = error "TODO" 

-- |                    
transStat (WhileStat cond body it) h s rs = error "TODO"   

-- |             
transStat (SeqStat s s') h s rs = error "TODO"       

-- |        
transStat (DeclareStat vtype vname rhs it) h s rs = error "TODO"  
    -- Check type of varialbe. If primitive save its value into register

-- |                  
transStat (AssignStat lhs rhs it) h s rs =  error "TODO" 

-- |              
transStat (IfStat cond sthen selse it) h s rs = error "TODO" 

-- |
transLHS :: AssignLhs -> [ Instr ] 
transLHS (LhsIdent id) = error "TODO"              
transLHS (LhsPairElem pelem) = error "TODO"                 
transLHS (LhsArrayElem (ArrayElem id exprs)) = error "TODO" 

--------------------------------------------------------------------------------

transExpr :: Expr -> [ Reg ] -> Store -> [ Instr ] 

-- | Put the value of boolean b into the first avaialble register r 
transExpr (BoolLiterExpr b) (r:_) _ = [ MOV r (Op2'ImmVal $ if b then 1 else 0) ] 

-- | Put the corresponding integer value of c into the first avaialble register r 
transExpr (CharLiterExpr c) (r:_) _ = [ MOV r (Op2'ImmVal $ ord c) ]   

-- | 
transExpr (IdentExpr id) (r:_) s = [ MOV r (Op2'ImmVal value) ]
  where
    -- TODO can we just put a constant into it?
    value = asImm8m (lookUpStore id s)

-- | 
transExpr (UnaryOperExpr NotUnOp e) rs@(r:r':_) s = error "TODO"
  where
    is = transExpr e rs ++ [ CMP r (Op2'ImmVal 0) ] ++ []

-- | 
transExpr (UnaryOperExpr LenUnOp e) rs s = error "TODO"

-- |   
transExpr (UnaryOperExpr OrdUnOp e) rs s = error "TODO" 

-- |                                   
transExpr (UnaryOperExpr ChrUnOp e) rs s = error "TODO" 

-- |                                   
transExpr (UnaryOperExpr NegUnOp e) rs s = error "TODO"

-- |
transExpr (ParenthesisedExpr e) rs s = transExpr e rs s

-- |
transExpr (IntLiterExpr i) rs s = error "TODO"  

-- |
transExpr (StrLiterExpr str) rs s = error "TODO"  

-- |
transExpr PairLiterExpr rs s = error "TODO" 

-- | TODO make ArrayElem a type synonym PLSSSSSSS
transExpr (ArrayElemExpr (ArrayElem ident exprs)) rs = error "TODO"  

-- |
transExpr (BinaryOperExpr op e e') rs = error "TODO"  

-- | 
transExpr (BinaryOperExpr AddBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr SubBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr MulBinOp e e') rs = error "TODO"

-- |   
transExpr (BinaryOperExpr DivBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr ModBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr AndBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr OrrBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr LsBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr GtBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr LEBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr GEBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr EqBinOp e e') rs = error "TODO" 

-- |   
transExpr (BinaryOperExpr NEBinOp e e') rs = error "TODO"  

--------------------------------------------------------------------------------

-- | Converts integer constant @n@ so that it can be used as operand2 #<imm8m>
asImm8m :: Int -> Int 
asImm8m n = error "TODO"

--------------------------------------------------------------------------------


extractVar                       :: Expr -> Store -> Variable 
extractVar (IdentExpr ident) s   =  lookupStore s ident
extractVar _                 _   =  error "extractIdent: Expecting IdentExpr"



sizeof :: Expr -> Word32 -- How many consecutive addresses does it take 
sizeof (BoolLiterExpr     _    ) = 1            
sizeof (CharLiterExpr     _    ) = 1            
sizeof (IdentExpr         name ) = error "WHHHHHHHAT"
sizeof (UnaryOperExpr _ _      ) = 1
sizeof (ParenthesisedExpr e    ) = sizeof e               
sizeof (IntLiterExpr           ) = 1               
sizeof (StrLiterExpr      str  ) = length str              
sizeof (PairLiterExpr          ) = 0  -- ?                
sizeof (ArrayElemExpr (ArrayElem arrName exprs)) = error "TODO"           
sizeof (BinaryOperExpr (BinaryOper Expr Expr)) = error "TODO"  

--sizeOfType :: Type -> Word32 
--sizeOfType IntType  = 1                                 
--sizeOfType BoolType = 1                             
--sizeOfType CharType = 1                             
--sizeOfType StringType                               
--sizeOfType PairType ( Maybe ( Type , Type ) ) =    
--sizeOfType ArrayType Type =                          
--sizeOfType NullType  = 0 -- ?                                
--sizeOfType EmptyType = 0 -- ?                             




