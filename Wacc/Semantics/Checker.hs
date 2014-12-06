module Wacc.Semantics.Checker where

import Wacc.Data.DataTypes
import Wacc.Data.ShowInstances
import Wacc.Data.SymbolTable
import Wacc.Semantics.Augmenter

import Control.Applicative hiding ( empty                                     )
import Control.Monad              ( when                                      )
import Data.Char                  ( isSpace                                   )
import Data.List                  ( group , sort                              )
import Data.Map                   ( empty                                     )
import Data.Maybe                 ( isNothing , fromMaybe , fromJust , isJust )


-- ************************************************************************** --
-- *************************                       ************************** --
-- *************************   Semantic Checking   ************************** --
-- *************************                       ************************** --
-- ************************************************************************** --

-- | Program ............................................................  32 --    
-- | Statements .........................................................  74 -- 
-- | LHS ................................................................ 174 -- 
-- | RHS ................................................................ 289 -- 
-- | Expression ......................................................... 371 -- 

-- | Errors ............................................................. 471 --


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Program Semantics ::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- |Check for semantic errors in a program. Check for duplicate functions, and
--  for each function check for duplicate parameter names and parameter-function
--  name clashes. If there are errors we stop here. Reason being that agumenting
--  the program when there are duplicate identifiers will overwrite existing
--  identifiers in the tables resulting in inaccurate semantic error messages.
--  If there are no errors we proceed and augment the program and check each
--  function body and the main body for semantic errors.
checkProgram                           :: Program -> (Program, [ SemanticErr ])
checkProgram prog@( Program funcs _ )  =
    (aug, if null duplicateErrs then statementErrs else duplicateErrs)
  where
    aug@(Program funcs' main)  =  augmentProgram prog
    statementErrs        =  concatMap checkStat ( map bodyOf funcs' ) ++
                            checkStat main
    duplicateErrs        =  checkFuncs            funcs ++
                            concatMap checkParams funcs
                            --concatMap checkClash  funcs

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Check that there is no parameter that shares its function name
checkClash                             :: Func -> [ SemanticErr ]
checkClash ( Func _ name params _ _ )  =
    checkDupl "Parameter Clash @" $ [ name ] ++ map pnameOf params

-- Check for duplicate parameter names in a function parameter list
checkParams  :: Func -> [ SemanticErr ]
checkParams  =  checkDupl "Duplicate Parameter @" . map pnameOf . paramsOf

-- Check for duplicate function names in a program
checkFuncs  :: [ Func ] -> [ SemanticErr ]
checkFuncs  =  checkDupl "Duplicate Function @" . map nameOf

-- Check for duplicate elements in a list and output a semantic error message
checkDupl      :: String -> [ String ] -> [ SemanticErr ]
checkDupl msg  =  map ( (++) msg. head ). filter ( (<) 1. length ). group. sort



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Statement Semantics ::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- Semantic check for a Statement
checkStat
  :: Stat            -- Given an augmented statement
  -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Skip statement, nothing to do here
checkStat SkipStat  =  []

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- A Free statement can only free a *variable* that points to a *pair* or to an
-- *array* type. So we pattern match to capture an IdentExpr and check that it
-- is a Variable or a Paramteer and that it is of type array or pair
checkStat s@( FreeStat expr@( IdentExpr _ ) it )  =
    onStat s $ checkExpr expr it nonFunction [ PairType {} , ArrayType {} ]

-- Any attempt to free another kind of expression is invalid
checkStat s@( FreeStat _ _ )  =
    onStat s [ "Cannot Free A Non-Identifier Expression" ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- A Return statement must not appear inside the main function body and it must
-- evaluate to a result that is the same type of the result of the function
-- it appears in
checkStat s@( ReturnStat expr it )  =
    onStat s $ if isNothing enclFunc then mainErr else enclErr
  where
    enclFunc = findEnclFunc it
    enclErr  = checkExpr expr it nonFunction [ fType ( fromJust enclFunc ) ]
    mainErr  = [ "Cannot Return From Main Function Body" ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- An Exit statement must evaluate to a value of type integer
checkStat s@( ExitStat expr it )  =
    onStat s $ checkExpr expr it nonFunction [ IntType ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- A Print statement is legal as long as it doesn't use undefined identifiers
checkStat s@( PrintStat expr it )  =
    onStat s $ checkExpr expr it nonFunction []

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Semantic rules identical to those of PrintStat
checkStat s@( PrintlnStat expr it )  =
    onStat s $ checkExpr expr it nonFunction []

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- For a Scoped statement we check the enclosed statement
checkStat ( ScopedStat stat )  =  checkStat stat

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- For a Read statement we check semantic errors in its lhs expression
checkStat s@( ReadStat lhs it )  =
    onStat s $ checkAssignLhs lhs it nonFunction readLhsTypes
  where
    -- Read is allowed only on int, char, string types
    readLhsTypes = [ IntType, CharType, StringType ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- A While statement requires the condition expression to be of BoolType
checkStat ( WhileStat cond body it )  =
    onExpr cond $ checkExpr cond it nonFunction [ BoolType ] ++ checkStat body

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Sequential checking of statements
checkStat ( SeqStat stat stat' )  =
    checkStat stat ++ checkStat stat'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- For an If statement the condition must be bool
checkStat ( IfStat cond sThen sElse it )  =
    onExpr cond $ checkExpr cond it nonFunction [ BoolType ] ++
    checkStat sThen ++
    checkStat sElse

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Not an easy one
checkStat s@( AssignStat lhs rhs it )  =
    onStat s $ if null lhsErr then rhsErr else lhsErr
  where
    lhsErr   =  checkAssignLhs lhs it nonFunction []
    lhsType  =  fromJust ( getLhsType lhs it )
    rhsErr   =  checkAssignRhs rhs it nonFunction [ lhsType ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Check for redeclarations in the same scope
checkStat s@( DeclareStat vtype name rhs it )  =
    onStat s $ if null definedErr then rhsErr else definedErr
  where
    rhsErr         =  checkAssignRhs rhs it nonFunction [ vtype ]
    definedErr     =  toSemErr definedErrMsg ( not $ isRedefined name it )
    definedErrMsg  =  "Variable Already Defined @" ++ name



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: LHS Semantics ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

checkAssignLhs :: AssignLhs -> It -> [ Context ] -> [ Type ] -> [ SemanticErr ]
checkAssignLhs lhs it ctxs types  =  case lhs of
    LhsIdent     ident    -> checkExpr      ( IdentExpr ident ) it ctxs types
    LhsPairElem  pElem    -> checkPairElem    pElem             it ctxs types
    LhsArrayElem arrElem  -> checkArrayElem   arrElem           it ctxs types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- A PairElem is either `fst` or `snd` of some expression. The only valid
-- expressions are identifier expressions and array element expressions.
-- In both cases they must point to an identifier of PairType
checkPairElem :: PairElem -> It -> [ Context ] -> [ Type ] -> [ SemanticErr ]
checkPairElem pElem it _ctxs types  =
    if   isJust pElemType
    then typeErr
    else [ "Could Not Retrieve Pair Type" ]
  where
    pElemType  =  getPairElemType pElem it
    typeErr    =  checkType ( fromJust pElemType ) types

-- For an ArrayElem we first check that the identifier has been declared, then
-- we count the number of dimensions on exprs and check that the obtained type
-- matches one of the expected types. We also check that all exprs representing
-- the indices, evaluate to integers
checkArrayElem  :: ArrayElem -> It -> [ Context ] -> [ Type ] -> [ SemanticErr ]
checkArrayElem aElem@( ident , exprs ) it ctxs types  =
    if   isJust aElemType
    then typeErr ++ exprsErr
    else [ "Could Not Retrieve Array Type @" ++ ident ]
  where
    aElemType  =  getArrayElemType aElem it
    typeErr    =  checkType ( fromJust aElemType ) types
    exprsErr   =  concatMap (\e -> checkExpr e it ctxs [ IntType ] ) exprs


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: LHS Semantics :: Types :::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Get the type of a lhs assignment
getLhsType         :: AssignLhs -> It -> Maybe Type
getLhsType lhs it  =  case lhs of
    LhsIdent     ident -> findType'        ident it
    LhsPairElem  pElem -> getPairElemType  pElem it
    LhsArrayElem aElem -> getArrayElemType aElem it

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Get the type of a pair element. We try to check the containing expression
-- and it must be of type PairType. If it fails there's no type to return.
-- Otherwise we can safely pattermatch on PairType and retrieve the pair
-- element type
getPairElemType           :: PairElem -> It -> Maybe Type
getPairElemType pElem it  =
  case pElem of

    Fst expr -> getPairElemType' expr True  
    Snd expr -> getPairElemType' expr False

  where

    getPairElemType' expr first = 
      case getPairElemType'' expr of
        Just (PairType (Just (ftype, stype))) -> Just $ if first then ftype else stype
        _                                     -> Nothing

    getPairElemType'' (IdentExpr       ident  ) = findType' ident it  
    getPairElemType'' (ArrayElemExpr   arrelem) = getArrayElemType arrelem it 
    getPairElemType''                        _  = Nothing 




-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Get the type of an array element.
getArrayElemType                               :: ArrayElem -> It -> Maybe Type
getArrayElemType ( ident , exprs ) it  =
    if isValidArray then arrElemType else Nothing
  where
    arrayObj                  =  findIdent' ident it
    IdentObj arrayType arrayCtx    =  fromJust arrayObj
    isValidArray              =  isJust arrayObj            &&
                                 arrayType ~== ArrayType {} &&
                                 arrayCtx ~/= Function {}
    arrElemType               =  Just $
                                   if   arrayType == StringType
                                   then CharType
                                   else deepen ( length exprs + 1 ) arrayType

    deepen 0   t              =  t
    deepen n ( ArrayType t )  =  deepen ( n - 1 ) t
    deepen _   t              =  t



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: RHS Semantics ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

checkAssignRhs
  :: AssignRhs       -- Similar to checkExpr, this is the rhs to check
  -> It              -- The identifier table
  -> [ Context ]     -- The expected contexts
  -> [ Type ]        -- The expected types
  -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Simple expression check here
checkAssignRhs ( RhsExpr expr ) it ctxs types  =
    checkExpr expr it ctxs types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Delegate the check to checkPairElem
checkAssignRhs ( RhsPairElem pelem ) it ctxs types  =
    checkPairElem pelem it ctxs types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- ArrayLiter only ever appears in AssignRhs, no need to create a separate
-- function. Check that all the expressions in the ArrayLiter are valid.
-- Then check that they are of the same type and of one of the types provided.
-- Since AssignRhs only occurs in AssignStat and DeclareStat we are certain
-- that `types` will be a singleton list containing the array type, so we
-- don't actually need to check explicitly that they are all of the same type
checkAssignRhs ( RhsArrayLiter exprs ) it ctxs types  =
    concatMap ( \e -> checkExpr e it ctxs [ arrElemType ] ) exprs
  where
    arrElemType = case head types of
                      ArrayType t -> t
                      _           -> error "Weird"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Once again, since AssignRhs only occurs in AssignStat and DeclareStat we are
-- certain that `types` is a singleton list containing a PairType. So we can
-- safely pattern match to extract its type
checkAssignRhs ( RhsNewPair efst esnd ) it ctxs types  =
    checkExpr efst it ctxs [ ftype ] ++
    checkExpr esnd it ctxs [ stype ]
  where
    PairType ( Just ( ftype , stype ) )  =  head types


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Here we are calling a function so we need to check that the name provided
-- refers to a function object that is in scope. Then check that the argument
-- list of expressions evaluates to a list of types that matches the number and
-- the types of the parameters of the function we are calling.
-- Note: this could use some improvements. There is a repeted call to findIndet.
-- So first we check that the identifier is in scope and that is is a function.
-- Once we are sure it is a Function we pattern match to expose the Func object.
checkAssignRhs rhs@( RhsCall fname args ) it@( ST encl _ ) ctxs types  =
  -- I look for fname in current table
  case findIdent' fname it of
    -- Not found!
    Nothing       -> [ "Function Not Found @" ++ fname ]

    Just identObj -> case objCtx identObj of
                        Function func -> proceed func
                        -- Keep looking one layer up
                        _ -> case findIdent' fname encl of
                               Nothing -> [ "Function Not Found @" ++ fname ]
                               Just identObj -> case objCtx identObj of
                                                   Function func -> proceed func
                                                   _             -> [ "Wrong Ctx @" ++ fname ]

  where

    proceed  ( Func ftype _ params _ _ )  =   retTypeErr ++ concat argsErrs ++ lengthErr
      where
          retTypeErr     =  checkType ftype types
          -- Args
          argsErrs       =  zipWith ( \e t -> checkExpr e it ctxs [ t ] ) args ( map ptypeOf params )
          -- Lenght
          lengthErrMsg   =  "Invalid Number Of Arguments In Function Call"
          lengthErr      =  toSemErr lengthErrMsg ( length params == length args )



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Expression Semantics :::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

checkExpr
  :: Expr            -- Given an expression
  -> It              -- And its identifier table
  -> [ Context ]     -- In case of an IdentExpr, check that the identifier
                     -- appears as one of the contexes provided, where empty
                     -- list means any Context
  -> [ Type ]        -- Check that the expression evaluates to one of the
                     -- expected types, where empty list means any Type
  -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Simple literals are only checked against the exptected types
checkExpr ( BoolLiterExpr _ ) _ _ types  =  checkType BoolType   types
checkExpr ( CharLiterExpr _ ) _ _ types  =  checkType CharType   types
checkExpr ( IntLiterExpr  _ ) _ _ types  =  checkType IntType    types
checkExpr ( StrLiterExpr  _ ) _ _ types  =  checkType StringType types
checkExpr   PairLiterExpr     _ _ types  =  checkType NullType   types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -e- -- --
-- For a ParenthesisedExpr we check the expression contained therein
checkExpr ( ParenthesisedExpr expr ) it ctxs types  =
    checkExpr expr it ctxs types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- For an IdentExpr we check that the identifier has beed declared and is
-- within scope. If it is we check for type and context as well.
checkExpr ( IdentExpr ident ) it ctxs types  =
    if   isJust identObj
    then checkType itype types ++
         checkCtx  ctx   ctxs
    else notFoundErr
  where
    identObj       =  findIdent' ident it
    notFoundErr    =  checkFound ident identObj
    IdentObj itype ctx =  fromJust identObj

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Let checkArrayElem do the job
checkExpr ( ArrayElemExpr arr ) it ctxs types  =
    checkArrayElem arr it ctxs types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- For a UnaryOperExpr we make sure the unary operation returns one of the
-- expected types and also that the expression argument for the operator is
-- of the expected type needed by the operator itself
checkExpr unop@( UnaryOperExpr op expr ) it ctxs types  =
  case op of
    NotUnOp -> checkUnOpExpr BoolType     BoolType
    LenUnOp -> checkUnOpExpr ArrayType {} IntType
    OrdUnOp -> checkUnOpExpr CharType     IntType
    ChrUnOp -> checkUnOpExpr IntType      CharType
    NegUnOp -> checkUnOpExpr IntType      IntType
  where
    checkUnOpExpr
      :: Type            -- The type expected by the unary operator
      -> Type            -- The type returned by the unary operator
      -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)
    checkUnOpExpr inType outType  =
        checkExpr expr it ctxs [ inType ] ++ checkType outType types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- For a BinaryOperExpr we make sure the binary operator returns one of the
-- expected types and also that the expression arguments for the operator
-- are of the expected type needed by the operator itself
checkExpr binop@( BinaryOperExpr op expr expr' ) it ctxs types  =
  case op of
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
      -> [ SemanticErr ] -- Return a bunch of SemanticErr's (or none)
    checkBinOpExpr inTypes outType  =
        checkExpr expr  it ctxs inTypes ++
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


-- Appends a statement to the error messages
onStat            :: Stat -> [ SemanticErr ] -> [ SemanticErr ]
onStat stat errs  =  map ( \err -> show' "" stat ++ " -> " ++ err ) errs


-- Appends an expression to the error messages
onExpr            :: Expr -> [ SemanticErr ] -> [ SemanticErr ]
onExpr expr errs  =  map ( \err -> show expr ++ " -> " ++ err ) errs


-- Given an input type and a list of expected types, will produce a single
-- semantic error in case of a type mismatch
checkType       :: Type -> [ Type ] -> [ SemanticErr ]
checkType t ts  =  toSemErr errMsg ( null ts || any ( ~== t ) ts )
  where
    errMsg  =  "Expecting Types: " ++ show ts ++ " | Found Type: " ++ show t


-- Given an input context and a list of expected contexts, will produce a single
-- semantic error in case of a context mismatch
checkCtx      :: Context -> [ Context ] -> [ SemanticErr ]
checkCtx c cs =  toSemErr errMsg ( null cs || any ( ~== c ) cs )
  where
    errMsg  =  "Expecting Contexts: " ++ concatMap str cs ++ " | Found Context: " ++ str c

    str   Variable      =  "Variable"  -- TODO eventually remove and derive Show
    str   Parameter     =  "Parameter"
    str ( Function _ )  =  "Function"


-- Given an identifier name and Maybe its associated identifier objcet, will
-- produce a single semantic error in case the identifier object is Nothing
checkFound           :: IdentName -> Maybe IdentObj -> [ SemanticErr ]
checkFound name obj  =  toSemErr errMsg ( isJust obj )
  where
    errMsg  =  "Identifier Not Found @" ++ name
