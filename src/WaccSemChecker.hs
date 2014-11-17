module WaccSemChecker where

import Data.Maybe                 ( isNothing , fromMaybe , fromJust , isJust )
import Data.Map                   ( empty                                     )
import Control.Applicative hiding ( empty                                     )
import Data.Char                  ( isSpace                                   )
import Data.List                  ( group , sort                              )
import Control.Monad              ( when                                      )

import WaccDataTypes
import WaccSymbolTable
import WaccSemAugmenter
import WaccShowInstances

-- ************************************************************************** --
-- *************************                       ************************** --
-- *************************   Program Semantics   ************************** --
-- *************************                       ************************** -- 
-- ************************************************************************** --

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Check for semantic errors in a program. Check for duplicate functions, and 
-- for each function check for duplicate parameter names and parameter-function
-- name clashes. If there are errors we stop here. Reason being that agumenting 
-- the program when there are duplicate identifiers will overwrite existing 
-- identifiers in the tables resulting in inaccurate semantic error messages.
-- If there are no errors we proceed and augment the program and check each 
-- function body and the main body for semantic errors. 
checkProgram                           :: Program -> [ SemanticErr ]
checkProgram prog@( Program funcs _ )  = 
    if null duplicateErrs then statementErrs else duplicateErrs
  where
    Program funcs' main  =  augmentProgram prog
    statementErrs        =  concatMap checkStat ( map bodyOf funcs' ) ++
                            checkStat main 
    duplicateErrs        =  checkFuncs            funcs ++ 
                            concatMap checkParams funcs ++ 
                            concatMap checkClash  funcs


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Check that there is no parameter that shares its function name
checkClash                             :: Func -> [ SemanticErr ]
checkClash ( Func _ name params _ _ )  =
    checkDupl "Parameter Clash @" $ [ name ] ++ map pnameOf params 


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Check for duplicate parameter names in a function parameter list
checkParams  :: Func -> [ SemanticErr ]
checkParams  =  checkDupl "Duplicate Parameter @" . map pnameOf . paramsOf  
  

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Check for duplicate function names in a program
checkFuncs  :: [ Func ] -> [ SemanticErr ] 
checkFuncs  =  checkDupl "Duplicate Function @" . map nameOf  
  

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Check for duplicate elements in a list and output a semantic error message
checkDupl      :: String -> [ String ] -> [ SemanticErr ]
checkDupl msg  =  map ( (++) msg. head ). filter ( (<) 1. length ). group. sort


-- ************************************************************************** --
-- ************************                         ************************* --
-- ************************   Statement Semantics   ************************* --
-- ************************                         ************************* -- 
-- ************************************************************************** --

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Semantic check for a Stat
checkStat 
  :: Stat            -- Given an augmented statement
  -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Skip statement
checkStat SkipStat  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- A Free statement can only free a *variable* that points to a *pair* or to an
-- *array* type. So we pattern match to capture an IdentExpr and check that it
-- is a Variable or a Paramteer and that it is of type array or pair
checkStat s@( FreeStat expr@( IdentExpr _ ) it )  =  error "TODO"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Any attempt to free another kind of expression is invalid
checkStat s@( FreeStat _ _ )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- A Return statement must not appear inside the main function body and it must 
-- evaluate to a result that is the same type of the result of the function
-- it appears in
checkStat s@( ReturnStat expr it )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- An Exit statement must evaluate to a value of type integer 
checkStat s@( ExitStat expr it )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- A Print statement is legal as long as it doesn't use undefined identifiers
checkStat s@( PrintStat expr it )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Semantic rules identical to those of PrintStat
checkStat s@( PrintlnStat expr it )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- For a Scoped statement we check the enclosed statement
checkStat ( ScopedStat stat )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- For a Read statement we check semantic errors in its lhs expression
checkStat s@( ReadStat lhs it )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- A While statement requires the condition expression to be of BoolType
checkStat ( WhileStat cond body it )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Sequential checking of statements
checkStat ( SeqStat stat stat' )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- For an If statement the condition must be bool
checkStat ( IfStat cond sthen selse it )  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Not an easy one
checkStat s@( AssignStat lhs rhs it ) =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Check for redeclarations in the same scope
checkStat s@( DeclareStat vtype name rhs it )  =  error "TODO"


-- ************************************************************************** --
-- ***************************                   **************************** --
-- ***************************   LHS Semantics   **************************** --
-- ***************************                   **************************** -- 
-- ************************************************************************** --

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- | Semantic check for AssignLhs
checkAssignLhs :: AssignLhs -> It -> [ Context ] -> [ Type ] -> [ SemanticErr ]
checkAssignLhs lhs it ctxs types  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- A PairElem is either `fst` or `snd` of some expression. The only valid 
-- expressions are identifier expressions and array element expressions.
-- In both cases they must point to an identifier of PairType
checkPairElem :: PairElem -> It -> [ Context ] -> [ Type ] -> [ SemanticErr ]
checkPairElem pelem it _ctxs types =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- For an ArrayElem we first check that the identifier has been declared, then
-- we count the number of dimensions on exprs and check that the obtained type
-- matches one of the expected types. We also check that all exprs representing
-- the indices, evaluate to integers
checkArrayElem  :: ArrayElem -> It -> [ Context ] -> [ Type ] -> [ SemanticErr ]
checkArrayElem aelem@( ArrayElem ident exprs ) it ctxs types  =  error "TODO"


-- ************************************************************************** --
-- ******************************               ***************************** --
-- ******************************   LHS Types   ***************************** --
-- ******************************               ***************************** -- 
-- ************************************************************************** --

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Get the type of a lhs assignment
getLhsType         :: AssignLhs -> It -> Maybe Type
getLhsType lhs it  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Get the type of a pair element. We try to check the containing expression
-- and it must be of type PairType. If it fails there's no type to return.
-- Otherwise we can safely pattermatch on PairType and retrieve the pair 
-- element type
getPairElemType           :: PairElem -> It -> Maybe Type
getPairElemType pelem it  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Get the type of an array element. 
getArrayElemType                               :: ArrayElem -> It -> Maybe Type
getArrayElemType ( ArrayElem ident exprs ) it  =  error "TODO"


-- ************************************************************************** --
-- ************************                         ************************* --
-- ************************   AssignRhs Semantics   ************************* --
-- ************************                         ************************* -- 
-- ************************************************************************** --

checkAssignRhs 
  :: AssignRhs       -- Similar to checkExpr, this is the rhs to check
  -> It              -- The identifier table
  -> [ Context ]     -- The expected contexts
  -> [ Type ]        -- The expected types
  -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Simple expression check here
checkAssignRhs ( RhsExpr expr ) it ctxs types  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Delegate the check to checkPairElem
checkAssignRhs ( RhsPairElem pelem ) it ctxs types  =  error "TODO"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- ArrayLiter only ever appears in AssignRhs, no need to create a separate
-- function. Check that all the expressions in the ArrayLiter are valid.
-- Then check that they are of the same type and of one of the types provided.
-- Since AssignRhs only occurs in AssignStat and DeclareStat we are certain 
-- that `types` will be a singleton list containing the array type, so we
-- don't actually need to check explicitly that they are all of the same type
checkAssignRhs ( RhsArrayLiter exprs ) it ctxs types  =  error "TODO"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Once again, since AssignRhs only occurs in AssignStat and DeclareStat we are 
-- certain that `types` is a singleton list containing a PairType. 
checkAssignRhs ( RhsNewPair efst esnd ) it ctxs types  =  error "TODO"
  

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Here we are calling a function so we need to check that the name provided
-- refers to a function object that is in scope. Then check that the argument
-- list of expressions evaluates to a list of types that matches the number and 
-- the types of the parameters of the function we are calling. 
checkAssignRhs rhs@( RhsCall fname args ) it ctxs types  =  error "TODO"


-- ************************************************************************** --
-- **************************                    **************************** --
-- **************************   Expr Semantics   **************************** --
-- **************************                    **************************** -- 
-- ************************************************************************** --

checkExpr 
  :: Expr            -- Given an expression
  -> It              -- And its identifier table
  -> [ Context ]     -- In case of an IdentExpr, check that the identifier 
                     -- appears as one of the contexes provided, where empty 
                     -- list means any Context
  -> [ Type ]        -- Check that the expression evaluates to one of the 
                     -- expected types, where empty list means any Type
  -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Simple literals are only checked against the exptected types
checkExpr ( BoolLiterExpr _ ) _ _ types  =  checkType BoolType   types
checkExpr ( CharLiterExpr _ ) _ _ types  =  checkType CharType   types
checkExpr ( IntLiterExpr  _ ) _ _ types  =  checkType IntType    types
checkExpr ( StrLiterExpr  _ ) _ _ types  =  checkType StringType types
checkExpr   PairLiterExpr     _ _ types  =  checkType NullType   types


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -e- -- -- 
-- For a ParenthesisedExpr we check the expression contained therein
checkExpr ( ParenthesisedExpr expr ) it ctxs types 
  = checkExpr expr it ctxs types


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- For an IdentExpr we check that the identifier has beed declared and is 
-- within scope. If it is we check for type and context as well.
checkExpr ( IdentExpr ident ) it ctxs types  
  =  if  isJust identObj
      then checkType itype types ++
          checkCtx  ctx   ctxs
      else notFoundErr
  where
    identObj      = findIdent' ident it
    notFoundErr   = checkFound ident identObj
    (,) itype ctx = fromJust identObj


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Let checkArrayElem do the job 
checkExpr ( ArrayElemExpr arr ) it ctxs types
  =  checkArrayElem arr it ctxs types


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- For a UnaryOperExpr we make sure the unary operation returns one of the 
-- expected types and also that the expression argument for the operator is 
-- of the expected type needed by the operator itself
checkExpr unop@( UnaryOperExpr op expr ) it ctxs types
  =  case op of  
      NotUnOp -> checkUnOpExpr BoolType     BoolType    
      LenUnOp -> checkUnOpExpr ArrayType {} IntType
      OrdUnOp -> checkUnOpExpr CharType     IntType
      ChrUnOp -> checkUnOpExpr IntType      CharType
      NegUnOp -> checkUnOpExpr IntType      IntType
      where
        checkUnOpExpr     
          :: Type            -- The type expected by the unary operator
          -> Type            -- The type returned by the unary operator
          -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)
        checkUnOpExpr inType outType
          =  checkExpr expr it ctxs [ inType ] ++ 
                               checkType outType types 



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- For a BinaryOperExpr we make sure the binary operator returns one of the 
-- expected types and also that the expression arguments for the operator  
-- are of the expected type needed by the operator itself
checkExpr binop@( BinaryOperExpr op expr expr' ) it ctxs types
  =  case op of
      -- Integer Binary Operators
      AddBinOp -> checkBinOpExpr [ IntType ] IntType
      SubBinOp -> checkBinOpExpr [ IntType ] IntType
      MulBinOp -> checkBinOpExpr [ IntType ] IntType
      DivBinOp -> checkBinOpExpr [ IntType ] IntType
      ModBinOp -> checkBinOpExpr [ IntType ] IntType
      -- Boolean Binary Operators
      AndBinOp -> checkBinOpExpr [ BoolType ] BoolType
      OrrBinOp -> checkBinOpExpr [ BoolType ] BoolType
      -- Integer & Char Binary Operators
      LsBinOp  -> checkBinOpExpr [ IntType , CharType ] BoolType
      GtBinOp  -> checkBinOpExpr [ IntType , CharType ] BoolType
      LEBinOp  -> checkBinOpExpr [ IntType , CharType ] BoolType
      GEBinOp  -> checkBinOpExpr [ IntType , CharType ] BoolType
      -- Equality Operators
      NEBinOp  -> checkBinOpExpr [] BoolType
      EqBinOp  -> checkBinOpExpr [] BoolType
      where
        checkBinOpExpr 
          :: [ Type ]        -- The type(s) expected by the binary operator
          -> Type            -- The type returned by the binary operator
          -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)
        checkBinOpExpr inTypes outType    
          = checkExpr expr  it ctxs inTypes ++
            checkExpr expr' it ctxs inTypes ++
            checkType outType types 

-- ************************************************************************** --
-- *************************                      *************************** --
-- *************************   Error Generation   *************************** --
-- *************************                      *************************** -- 
-- ************************************************************************** --

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- A SemanticErr is just a string definitg the error message
type SemanticErr  =  [ Char ]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Given an error message and a predicate, puts the message in a singleton
-- list if the pridicate fails, otherwise it returns the empty list
toSemErr            :: String -> Bool -> [ SemanticErr ] 
toSemErr msg True   =  [] 
toSemErr msg False  =  [ msg ]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Appends a statement to the error messages 
onStat            :: Stat -> [ SemanticErr ] -> [ SemanticErr ]
onStat stat errs  =  map ( \err -> show' "" stat ++ " -> " ++ err ) errs


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Appends an expression to the error messages 
onExpr            :: Expr -> [ SemanticErr ] -> [ SemanticErr ]
onExpr expr errs  =  map ( \err -> show expr ++ " -> " ++ err ) errs


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Given an input type and a list of expected types, will produce a single 
-- semantic error in case of a type mismatch 
checkType       :: Type -> [ Type ] -> [ SemanticErr ]
checkType t ts  =  toSemErr errMsg ( null ts || any ( ~== t ) ts )
  where 
    errMsg  =  "Expecting Types: " ++ show ts ++ " | Found Type: " ++ show t


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Given an input context and a list of expected contexts, will produce a single 
-- semantic error in case of a context mismatch 
checkCtx      :: Context -> [ Context ] -> [ SemanticErr ]
checkCtx c cs =  toSemErr errMsg ( null cs || any ( ~== c ) cs )
  where
    errMsg  =  "Expecting Ctxs: " ++ show cs ++ " | Found Ctx: " ++ str c

    str   Variable      =  "Variable"  -- TODO eventually remove and derive Show
    str   Parameter     =  "Parameter"
    str ( Function _ )  =  "Function"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Given an identifier name and Maybe its associated identifier objcet, will
-- produce a single semantic error in case the identifier object is Nothing
checkFound           :: IdentName -> Maybe IdentObj -> [ SemanticErr ]
checkFound name obj  =  toSemErr errMsg ( isJust obj )
  where
    errMsg  =  "Identifier Not Found @" ++ name 







