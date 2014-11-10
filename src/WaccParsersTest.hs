-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 3.2. WACC Parser "unit" tests ::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

module WaccParsersTest where

import WaccParser
import WaccDataTypes

import Text.Parsec.Token
import Text.ParserCombinators.Parsec

runAll = do 
    {-testPairLiter  = -} runTests "pPairLiter"  pPairLiter  tPairLiter
    {-testArrayLiter = -} runTests "pArrayLiter" pArrayLiter tArrayLiter
    {-testBoolLiter  = -} runTests "pBoolLiter"  pBoolLiter  tBoolLiter
    {-testIntLiter   = -} runTests "pIntLiter"   pIntLiter   tIntLiter
    {-testPairType   = -} runTests "pPairType"   pPairType   tPairType
    {-testArrayElem  = -} runTests "pArrayElem"  pArrayElem  tArrayElem 
    {-testType       = -} runTests "pType"       pType       tType 
    {-testBaseType   = -} runTests "pbaseType"   pBaseType   tBaseType
    {-testParam      = -} runTests "pParam"      pParam      tParam
    {-testParamList  = -} runTests "pParamList"  pParamList  tParamList 
    {-testCharLiter  = -} runTests "pParamList"  pParamList  tParamList 
    {-testStrLiter   = -} runTests "pStrLiter"   pStrLiter   tStrLiter
    {-testProgram    = -} runTests "pProgram"    pProgram    tProgram
    {-testStat       = -} runTests "pStat"       pStat       tStat
    {-testFunc       = -} runTests "pFunc"       pFunc       tFunc

tPairLiter 
  = [ ( "null"  , True  )
    , ( ""      , False )
    , ( " null" , False )
    , ( " Null" , False )
    , ( " null" , False )
    , ( "Null " , False ) ]

tArrayLiter 
  = [ ( ""    , False )
    , ( "["   , False )
    , ( "]"   , False )
    , ( "[]"  , True  )
    , ( "[,]" , False ) ]

tIntSign 
  = [ ( "+" , True ) 
    , ( "-" , True ) 
    , ( "a" , True ) 
    , ( "a" , True ) 
    , ( ""  , True ) ]

tIntLiter
  = [ ( "12345"   , True  ) 
    , ( "+12345"  , True  )
    , ( "-12345"  , True  )
    , ( "0"       , True  )
    , ( "+0"      , True  )
    , ( "-0"      , True  )
    , ( "000000"  , True  )
    , ( "-000111" , True  )
    , ( "+111100" , True  )
    , ( "+1x"     , True  )
    , ( ""        , False )
    , ( "+"       , False )
    , ( "-"       , False )
    , ( "a"       , False )
    , ( "+a"      , False )
    , ( "-a"      , False )
    , ( "hello"   , False ) ]

tComment 
  = [ ( ""                     , False ) 
    , ( "1"                    , False )
    , ( "a"                    , False )
    , ( "\n"                   , False )
    , ( "\t"                   , False )
    , ( "\""                   , False )
    , ( "#"                    , False )
    , ( "#hello"               , False )
    , ( "#"                    , False )
    , ( "#\n"                  , True  )
    , ( "#1\n"                 , True  )
    , ( "#a\n"                 , True  )
    , ( "#\"\'\t\b\f\0Allah\n" , True  )
    , ( "# Hello World\n"      , True  )
    , ( "########\n"           , True  )
    , ( "# \n"                 , True  )
    , ( "# ##\na"              , True  )
    , ( "# :@òasd%313412&&&\n" , True  ) ]


-- To create a test for a parsing function `pFuncName`
-- 1) Create test array `tFuncName` :: ( str::String , pass::Bool )
--    With input string to parse `str` and whether or not it should `pass`
-- 2) ghci> runTests "FuncName" pFuncName tFuncName
-- 3) add the line above to `runAll`

runAll = do
  runTests "pPairLiter"  pPairLiter  tPairLiter
  runTests "pArrayLiter" pArrayLiter tArrayLiter
  runTests "pIntSign"    pIntSign    tIntSign
  runTests "pIntLiter"   pIntLiter   tIntLiter
  runTests "pComment"    pComment    tComment


runTests fname parser tests = mapM_ ( runTest fname parser ) tests

runTest fname parser ( input , pass ) = case regularParse parser input of 
  Left  e -> log $ if not pass then "OK it failed!"
                   else "='( Failed but should've passed... ("  ++ show e ++ ")"
  Right r -> log $ if pass then "OK it succeeded! (" ++ show r ++ ")" 
                   else "='( Success but shoudl've failed... (" ++ show r ++ ")"
  where 
    log msg = putStrLn $ fname ++ ":\t\"" ++ input ++ "\"\t ... " ++ msg ++ "\n"


-- 3.2.2 Tests for statements
tStat
  = [ ( "skip",                             True  )
    , ( "int x = 10" ,                      True  )
    , ( "int[] a = [1,2]",                  True  )
    , ( "pair(int,int) = newpair(1,1)",     True  )
    , ( "x = a * 4 + 1 ",                   True  )
    , ( "read x"  ,                         True  )
    , ( "read 1"  ,                         False )
    , ( "free x",                           True  )
    , ( "free 1",                           False )
    , ( "exit 10",                          True  )
    , ( "print \"a\"",                      True  )
    , ( "print 1+1",                        True  )
    , ( "println w",                        True  )
    , ( "println w!",                       False )
    , ( "if (1 > 2) then skip else a=2",    False )
    , ( "if (1 > 2) then skip else a=2 fi", True  )
    , ( "while(true) do skip done",         True  )
    , ( "while(true) do done",              False )
    , ( "while() do done",                  False )
    , ( "begin skip end",                   True  )
    , ( "skip; a=2",                        True  )
    , ( "skip; skip",                       True  ) ]


-- 3.2.3 Tests for types
-- TODO:
tPairType
  = [ ( "pair(int, int)"           , True  )
    , ( "pair(pair, int)"          , True  )
    , ( "pair(int, bla)"           , False )
    , ( "pair(int, pair(int,int))" , False )
    , ( "pair(int, )"              , False )
    , ( "pair(,)"                  , False )
    , ( "pair()"                   , False )
    , ( "newpair(int, int)"        , False )
    , ( "pair(1,2)"                , False ) ]

-- 3.2.4 Tests for expressions

-- |For Int, Bool, Char, Str, Pair literals see Section 3.2.1 Literals
-- TODO:

-- 3.2.5 Tests for programs and function
-- TODO:



-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: Unit Test Framework ::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- |Runs all tests, grouped by parser and outputs for each whether all the tests
-- have passed. Will display details only for the failed tests.
runGroupAll = do
    runGroupTests "pBoolLiter"   pBoolLiter   tBoolLiter
    runGroupTests "pIntLiter"    pIntLiter    tIntLiter
    runGroupTests "pCharLiter"   pCharLiter   tCharLiter
    runGroupTests "pStrLiter"    pStrLiter    tStrLiter
    runGroupTests "pPairLiter"   pPairLiter   tPairLiter
    runGroupTests "pArrayLiter"  pArrayLiter  tArrayLiter
    runGroupTests "pStat"        pStat        tStat
    runGroupTests "pPairType"    pPairType    tPairType

runGroupTests fname parser tests 
    = if and $ map ( verifyTest fname parser ) tests
       then putStrLn $ fname ++ "\tPASSED!\n"
       else putStrLn $ fname ++ "\tFAILED!\n" 
            ++ ( concatMap (runTest fname parser) tests )

        where

          verifyTest fname parser ( input , pass ) 
              = case parseWithEof parser input of 
                  Left  e -> not pass
                  Right r -> pass 
 
runTest :: (Show a) => [Char] -> Parser a -> ([Char], Bool) -> [Char]
runTest fname parser ( input , pass ) 
    = case parseWithEof parser input of 
        Left  e -> log $ if not pass then ""
                         else "pass (" ++ show e ++ ")"
        Right r -> log $ if pass     then ""
                         else "fail (" ++ show r ++ ")"
      where 
          log msg 
            | msg == "" = "" 
            | otherwise = "FAIL!  " ++ fname ++ " \t" ++ input 
                             ++ " \t: should " ++ msg ++ "\n"


-- |Runs all tests for a parser, and displays individually whether each test tuple
-- has failed. Will display details for both the failed tests and passed tests.
runDetailTests fname parser tests = mapM_ ( putStr . ( run fname parser ) ) tests

run :: (Show a) => [Char] -> Parser a -> ([Char], Bool) -> [Char]
run fname parser ( input , pass ) 
    = case parseWithEof parser input of 
        Left  e ->  if not pass then "fail"
                    else "pass (" ++ show e ++ ")"
        Right r ->  if pass     then "pass (" ++ show r ++ ")"
                    else "fail (" ++ show r ++ ")"



 
