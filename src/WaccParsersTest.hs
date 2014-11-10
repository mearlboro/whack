-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 3.2. WACC Parser "unit" tests ::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

module WaccParsersTest where

import WaccParser
import WaccDataTypes

import Text.ParserCombinators.Parsec


-- |This testing library mostly contains edge-cases for the subcomponents of our
-- parser. The role of this library is to spot bugs at function level, and help
-- with detecting the reasons of failure when testing with complete programs.


-- 3.2.1 Tests for literals and identifiers 

-- |Note: the apparently redundant literal tests are design to check whether the
-- language specifications provided by the Parsec library correspond to the WACC
-- language definition in the given specification.

tBoolLiter
 = [ ( "true"        , True  )
   , ( "false"       , True  )
   , ( "False "      , False )
   , ( "0"           , False )
   , ( "trueeeeelol" , False ) ]

tIntLiter
  = [ ( "12345"   , True  ) 
    , ( "+12345"  , True  )
    , ( "-12345"  , True  )
    , ( "0"       , True  )
    , ( "+0"      , True  )
    , ( "-0"      , True  )
    , ( "000000"  , True  )
    , ( "---0111" , False )
    , ( "++11100" , False )
    , ( "+1x"     , False )
    , ( ""        , False )
    , ( "+"       , False )
    , ( "-"       , False )
    , ( "a"       , False )
    , ( "+a"      , False )
    , ( "-a"      , False )
    , ( "-12xx45" , False ) ]

tCharLiter
 = [ ( ""       , False )
   , ( " "      , False )
   , ( "a"      , False )
   , ( "\'a\'"  , True  )
   , ( "\' \'"  , True  )
   , ( "\'aa\'" , False )
   , ( "\'\'\'" , False )
   , ( "\'0\'"  , True  )
   , ( "\'.\'"  , True  )
   , ( "\'-1\'" , False ) 
   , ( "\'"     , False )
   , ( "\'\'"   , False )
   , ( "\"a\""  , False ) ]


tStrLiter
 = [ ( ""             , False )
   , ( " "            , False )
   , ( "\" \""        , True  )
   , ( "\"\"\""       , False )
   , ( "\"\"\"\""     , False ) 
   , ( "abc"          , False ) 
   , ( "\"\""         , True  )
   , ( "\"abc\""      , True  ) 
   , ( "\"a\\\"b\""   , True  ) 
   , ( "\"a\" \"a \"" , False ) ]


tArrayLiter 
  = [ ( "[]"              , True  )
    , ( "[,]"             , False )
    , ( "[1]"             , True  )
    , ( "[1, 2]"          , True  )
    , ( "[a + 1, 3]"      , True  )
    , ( "[a, b]"          , True  )
    , ( "[\"\"]"          , True  )
    , ( "[\"a\"]"         , True  )
    , ( "[\'a\',\'b\']"   , True  )
    , ( "[\"a\", \"\"]"   , True  )
    , ( "[\"a]"           , False )
    , ( "[bla]"           , True  ) ]


tPairLiter 
  = [ ( "null"  , True  )
    , ( ""      , False )
    , ( " null" , False )
    , ( "null a", False )
    , ( "nulla" , False )
    , ( "Null " , False ) ]


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



 
