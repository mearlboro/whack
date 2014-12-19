module Wacc.Optimization.Optimizer where

import Wacc.Data.DataTypes
import Wacc.CodeGeneration.ARM11Instructions
import Wacc.Semantics.Checker
import Wacc.Syntax.Parser

import qualified Data.Map as Map 
import qualified Wacc.Data.SymbolTable as St

import Data.Char (ord)

{-

Here's the deal:

begin
  int i = 10          
  int j = i + 5          -->  j = 10 + 5 = 15
  
  while (i * j > 10) do  --> while 10 * 15 > 150 --> while (150 > 150) --> while (false)
   ...
  end 

  bool p = true
  bool q = p || !p       --> q = true || !true --> true || false --> true 

  if (p && q)            --> if (true && true) --> if (true)
  
  char a = 'a'
  char b = 'b'
  
  bool r = a < b         --> r = true 
end 

We want these replacements/sbstitutions/semplifications to be done at 
**Compile Time** before we generate instructions. 
This means modifing the Program AST once more before we call translateProgram.

The idea is to associate each Identifier (i, j, p, q, a, b, r) with its 
right-hand-side Expr and map each Identifier to such Expr.
So the SymbolTable has been modified so that now IdentObj also has a field for 
the expression it contains, in addition to type and context.
So i would have: IntLiterExpr 10
   j would have: BinaryOperExpr AddBinOP (IdentExpr i) (IntLiterExpr 5)

The problem now is that we need to add Expr information whenever we encounter
a DeclareStat but also an AssignStat. 
The best (only) place to do so is in the Agumenter, so that the identifier 
tables remain intact.

Assuming that works (hardly), then CanReplace should work fine (hopefully) 
and what's left to do is **instance CanSimplify Expr where** 
(atm it gets stuck in a black hole. 
The answer might be the almighty fix function)

-}

_it = undefined -- Mock identifier table

class CanReplace a where
  replace :: a -> It -> a 

instance (CanReplace a) => CanReplace [ a ] where
  replace xs it = map (`replace` it) xs 

instance CanReplace Program where
  replace (Program funcs body) _ = Program (replace funcs _it) (replace body _it)

instance CanReplace Func where
  replace f _ = let body = bodyOf f in f { bodyOf = replace body _it }

instance CanReplace Stat where
  replace  SkipStat                _ = SkipStat
  replace (FreeStat    e       it) _ = FreeStat    (replace e it) it 
  replace (ReturnStat  e       it) _ = ReturnStat  (replace e it) it 
  replace (ExitStat    e       it) _ = ExitStat    (replace e it) it 
  replace (PrintStat   e       it) _ = PrintStat   (replace e it) it 
  replace (PrintlnStat e       it) _ = PrintlnStat (replace e it) it 
  replace (ScopedStat  s         ) _ = ScopedStat  (replace s _it) 
  replace (ReadStat    lhs     it) _ = ReadStat    lhs            it  
  replace (WhileStat   e s     it) _ = WhileStat   (replace e it) (replace s _it) it 
  replace (IfStat      e s s'  it) _ = IfStat      (replace e it) (replace s _it) (replace s' _it) it 
  replace (AssignStat  lhs rhs it) _ = AssignStat  lhs (replace rhs it) it
  replace (DeclareStat t n rhs it) _ = DeclareStat t n (replace rhs it) it  
  replace (SeqStat     s s'      ) _ = SeqStat     (replace s _it) (replace s' _it) 

instance CanReplace AssignRhs where
  replace (RhsExpr       e      ) it = RhsExpr (replace e it)           
  replace (RhsPairElem   (Fst e)) it = RhsPairElem (Fst (replace e it))          
  replace (RhsPairElem   (Snd e)) it = RhsPairElem (Snd (replace e it))           
  replace (RhsArrayLiter es     ) it = RhsArrayLiter (replace es it)       
  replace (RhsNewPair    e e'   ) it = RhsNewPair (replace e it) (replace e' it) 
  replace (RhsCall       id es  ) it = RhsCall id (replace es it)     

instance CanReplace Expr where
  replace (IdentExpr id) it = 
    case St.getExpr id it of 
      -- * Magic happens right here *--
      Nothing   -> IdentExpr id
      Just expr -> expr 

  replace (UnaryOperExpr  op e    ) it = UnaryOperExpr op (replace e it) 
  replace (ParenthesisedExpr     e       ) it = ParenthesisedExpr (replace e it)
  replace (ArrayElemExpr  (id, es)) it = ArrayElemExpr (id, replace es it)
  replace (BinaryOperExpr op e e' ) it = BinaryOperExpr op (replace e it) (replace e' it) 
  replace                 e         _  = e

class CanSimplify a where
  simplify :: a -> a 

instance (CanSimplify a) => CanSimplify [ a ] where
  simplify = map simplify

instance CanSimplify Program where
  simplify (Program fs main) = Program (simplify fs) (simplify main)

instance CanSimplify Func where
  simplify f = f { bodyOf = simplify (bodyOf f) }

instance CanSimplify Stat where 
  simplify  SkipStat                 = SkipStat
  simplify (FreeStat    e        it) = FreeStat    (simplify e) it 
  simplify (ReturnStat  e        it) = ReturnStat  (simplify e) it 
  simplify (ExitStat    e        it) = ExitStat    (simplify e) it
  simplify (PrintStat   e        it) = PrintStat   (simplify e) it
  simplify (PrintlnStat e        it) = PrintlnStat (simplify e) it
  simplify (ScopedStat  s          ) = ScopedStat  (simplify s) 
  simplify (ReadStat    lhs      it) = ReadStat    (simplify lhs) it 
  simplify (WhileStat   e s      it) = WhileStat   (simplify e)   (simplify s) it
  simplify (SeqStat     s s'       ) = SeqStat     (simplify s)   (simplify s')
  simplify (AssignStat  lhs rhs  it) = AssignStat  (simplify lhs) (simplify rhs) it
  simplify (IfStat      e s s'   it) = IfStat      (simplify e)   (simplify s)   (simplify s') it 
  simplify (DeclareStat t n rhs  it) = DeclareStat t n (simplify rhs) it

instance CanSimplify AssignLhs where
  simplify (LhsPairElem  (Fst e))  = LhsPairElem  (Fst (simplify e))
  simplify (LhsPairElem  (Snd e))  = LhsPairElem  (Snd (simplify e))
  simplify (LhsArrayElem (id, es)) = LhsArrayElem (id,  simplify es)
  simplify                   lhs   = lhs  

instance CanSimplify AssignRhs where
  simplify (RhsExpr       e      ) = RhsExpr          (simplify e)         
  simplify (RhsPairElem   (Fst e)) = RhsPairElem (Fst (simplify e))              
  simplify (RhsPairElem   (Snd e)) = RhsPairElem (Snd (simplify e))
  simplify (RhsArrayLiter es     ) = RhsArrayLiter    (simplify es)            
  simplify (RhsNewPair    e e'   ) = RhsNewPair       (simplify e) (simplify e')            
  simplify (RhsCall       id es  ) = RhsCall id       (simplify es)


instance CanSimplify Expr where
  simplify (BinaryOperExpr op (IntLiterExpr  i) (IntLiterExpr  j))  =  simplifyInt op i j
  simplify (BinaryOperExpr op (BoolLiterExpr p) (BoolLiterExpr q))  =  simplifyBool op p q
  simplify (BinaryOperExpr op (CharLiterExpr a) (CharLiterExpr b))  =  simplifyInt op (ord a) (ord b) 
  simplify (BinaryOperExpr op                e                 e')  =  simplify' (BinaryOperExpr op (simplify e) (simplify e'))
  simplify (UnaryOperExpr  op                e                   )  =  UnaryOperExpr op (simplify e)      
  simplify (ParenthesisedExpr e)                                    =  (simplify e)  
  simplify (ArrayElemExpr     (id, es))                             =  ArrayElemExpr (id, simplify es)            
  simplify                                                     e    =  e   


simplify' (BinaryOperExpr op (IntLiterExpr  i) (IntLiterExpr  j))  =  simplifyInt op i j
simplify' (BinaryOperExpr op (BoolLiterExpr p) (BoolLiterExpr q))  =  simplifyBool op p q
simplify' (BinaryOperExpr op (CharLiterExpr a) (CharLiterExpr b))  =  simplifyInt op (ord a) (ord b) 
simplify' (UnaryOperExpr  op                e                   )  =  UnaryOperExpr op (simplify e)      
simplify' (ParenthesisedExpr e)                                    =  (simplify e)  
simplify' (ArrayElemExpr     (id, es))                             =  ArrayElemExpr (id, simplify es)            
simplify'                                                     e    =  e   


simplifyInt AddBinOp i j = IntLiterExpr  (i   +   j) 
simplifyInt SubBinOp i j = IntLiterExpr  (i   -   j)
simplifyInt MulBinOp i j = IntLiterExpr  (i   *   j)
simplifyInt DivBinOp i j = IntLiterExpr  (i `div` j)
simplifyInt ModBinOp i j = IntLiterExpr  (i `mod` j)
simplifyInt LsBinOp  i j = BoolLiterExpr (i   <   j)
simplifyInt GtBinOp  i j = BoolLiterExpr (i   >   j)
simplifyInt LEBinOp  i j = BoolLiterExpr (i   <=  j)
simplifyInt GEBinOp  i j = BoolLiterExpr (i   >=  j)
simplifyInt EqBinOp  i j = BoolLiterExpr (i   ==  j)
simplifyInt NEBinOp  i j = BoolLiterExpr (i   /=  j)

simplifyBool AndBinOp p q = BoolLiterExpr (p && q)   
simplifyBool OrrBinOp p q = BoolLiterExpr (p || q)  
simplifyBool EqBinOp  p q = BoolLiterExpr (p == q)  
simplifyBool NEBinOp  p q = BoolLiterExpr (p /= q) 


test :: String -> IO () 
test file = do 
  source <- readFile ("/Users/Zeme/Desktop/wacc_24/WaccTesting/wacc_examples/valid/" ++ file)
  let Right prog       = parseWithEof pProgram source
      (prog', semErrs) = checkProgram prog

  putStrLn $ "BeforeOptimizing: " 
  putStrLn $ show prog
  putStrLn $ "AfterOptimizing: " 
  putStrLn $ show (replace prog' _it)

----------------------------------------------------------------------------------
--extractIt                        :: Stat -> Maybe It 
--extractIt  SkipStat              =  Nothing
--extractIt (FreeStat    _     it) =  Just it 
--extractIt (ReturnStat  _     it) =  Just it 
--extractIt (ExitStat    _     it) =  Just it 
--extractIt (PrintStat   _     it) =  Just it 
--extractIt (PrintlnStat _     it) =  Just it 
--extractIt (ScopedStat  s       ) =  extractIt s
--extractIt (ReadStat    _     it) =  Just it 
--extractIt (WhileStat   _ _   it) =  Just it 
--extractIt (IfStat      _ _ _ it) =  Just it 
--extractIt (AssignStat  _ _   it) =  Just it 
--extractIt (DeclareStat _ _ _ it) =  Just it
--extractIt (SeqStat     s s'    ) =  
--  case extractIt s' of 
--    Nothing -> extractIt s 
--    it      -> it 
-- --------------------------------------------------------------------------------
--replaceIt                        :: Stat -> Maybe It 
--replaceIt  SkipStat              =  Nothing
--replaceIt (FreeStat    _     it) =  Just it 
--replaceIt (ReturnStat  _     it) =  Just it 
--replaceIt (ExitStat    _     it) =  Just it 
--replaceIt (PrintStat   _     it) =  Just it 
--replaceIt (PrintlnStat _     it) =  Just it 
--replaceIt (ScopedStat  s       ) =  extractIt s
--replaceIt (ReadStat    _     it) =  Just it 
--replaceIt (WhileStat   _ _   it) =  Just it 
--replaceIt (IfStat      _ _ _ it) =  Just it 
--replaceIt (AssignStat  _ _   it) =  Just it 
--replaceIt (DeclareStat _ _ _ it) =  Just it
--replaceIt (SeqStat     s s'    ) =  
--  case extractIt s' of 
--    Nothing -> extractIt s 
--    it      -> it 
--    where
--    rhs' = replace rhs it 
--    it'  = case (lhs, rhs') of 
--            (LhsIdent id, RhsExpr e) -> setExpr it id e
--            _                        -> it 
--replace (DeclareStat vtype vname rhs it) =  DeclareStat vtype vname rhs' it'
--  where
--    rhs' = replace rhs it 
--    it'  = case rhs' of 
--             RhsExpr e -> setExpr it vname e
--             _         -> it 
--replace (SeqStat fstS sndS) = SeqStat fstS' sndS' 
--  where
--    fstS' = replace fstS
--    sndS' = 



--extractRhsExpr :: AssignRhs -> Expr 
--extractRhsExpr (RhsExpr       e      ) = 
--extractRhsExpr (RhsPairElem   (Fst e)) = 
--extractRhsExpr (RhsPairElem   (Snd e)) = 
--extractRhsExpr (RhsArrayLiter es     ) = 
--extractRhsExpr (RhsNewPair    e e'   ) = 
--extractRhsExpr (RhsCall       id es  ) = 

--extractLhsIdent :: AssignLhs -> IdentName 
--extractLhsIdent (LhsIdent                          id     ) = id 
--extractLhsIdent (LhsPairElem  (Fst (IdentExpr      id   ))) = id  
--extractLhsIdent (LhsPairElem  (Fst (ArrayElemExpr (id, _))) = id  
--extractLhsIdent (LhsPairElem  (Fst (ParensExpr     e      ) = extractLhsIdent (LhsPairElem (Fst e)) 
--extractLhsIdent (LhsPairElem  (Snd (IdentExpr      id   ))) = id  
--extractLhsIdent (LhsPairElem  (Snd (ArrayElemExpr (id, _))) = id  
--extractLhsIdent (LhsPairElem  (Snd (ParensExpr     e      ) = extractLhsIdent (LhsPairElem (Snd e)) 
--extractLhsIdent (LhsArrayElem                     (id, _) ) = id 
--format      :: [ Char ] -> [ Char ]
--format txt  = 
--  where 
--    cols  :: [[[ Char ]]]
--    cols  =  (transpose . transpose . map words . lines) txt  

--    align       :: [[ Char ]] -> [[ Char ]] -- Add spaces
--    align cols  =  map (++ spaces) cols 
--      where 
--        spaces col  =  replicate (length col - mcol + 1) ' '
--        mcol        =  max        cols  


