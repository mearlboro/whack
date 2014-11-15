-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: 3.2.1 WACC Parser "unit" tester  :::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

module WaccTesting.WaccParsersTest where

import Wacc.WaccParser

import Text.ParserCombinators.Parsec


-- |This testing library mostly contains edge-cases for the subcomponents of our
-- parser. The role of this library is to spot bugs at function level, and help
-- with detecting the reasons of failure when testing with complete programs.


--------------------------------------------------------------------------------

-- Tests for literals and identifiers

-- |Note: the apparently redundant literal tests are design to check whether the
-- language specifications provided by the Parsec library correspond to the WACC
-- language definition in the given specification.

tBoolLiter
 = [ ( "true"        , True  )
   , ( "false"       , True  )
   , ( ""            , False )
   , ( "True "       , False )
   , ( ",true "      , False )
   , ( "falsetrue "  , False )
   , ( "truetrue "   , False )
   , ( "true false " , False )
   , ( "trueeeeelol" , False ) ]

tIntLiter
  = [ ( "12345"    , True  )
    , ( "0"        , True  )
    , ( "+0"       , True  )
    , ( "-0"       , True  )
    , ( "000000"   , True  )
    , ( "0011 "    , True  )
    , ( "+0011 "   , True  )
    , ( "-0011 "   , True  )
    , ( ""         , False )
    , ( " 10 a "   , False )
    , ( " -10 a "  , False )
    , ( " 10, "    , False )
    , ( " 10++1 "  , False )
    , ( "---0111"  , False )
    , ( "++11100"  , False )
    , ( "+1x"      , False )
    , ( "a"        , False )
    , ( "-a"       , False )
    , ( "2xx45"    , False ) ]

tCharLiter
  = [ ( "\'a\'"    , True  )
    , ( "\'0\'"    , True  )
    , ( "\'.\'"    , True  )
    , ( "\'\\\"\'" , True  )
    , ( "\'aa\'"   , False )
    , ( "\'-1\'"   , False )
    , ( "\'"       , False )
    , ( "\'\'"     , False )
    , ( "\'\'\'"   , False )
    , ( "\'\"\'"   , False )
    , ( "\""       , False )
    , ( "\'\'\'\'" , False )
    , ( "a"        , False )
    , ( "\"a\""    , False ) ]

tStrLiter
  = [ ( "\"\""         , True  )
    , ( "\"a\""        , True  )
    , ( "\"abc\""      , True  )
    , ( "\"a\\\"b\""   , True  )
    , ( ""             , False )
    , ( "abc"          , False )
    , ( "\""           , False )
    , ( "\"\"\""       , False )
    , ( "\"\"\"\""     , False )
    , ( "\"a\" \"a \"" , False ) ]

tArrayLiter
  = [ ( "[]"                  , True  )
    , ( "[1] "                , True  )
    , ( "[1]"                 , True  )
    , ( "[1, 2]"              , True  )
    , ( "[a + 1, 3]"          , True  )
    , ( "[a , b] "            , True  )
    , ( "[a,b, c,___1] "      , True  )
    , ( "[\"\"]"              , True  )
    , ( "[\"a\"]"             , True  )
    , ( "[\'a\'  ,\'b\']"     , True  )
    , ( "[\"a\", \"\"]"       , True  )
    , ( "['1', \" a\"] "      , True  )
    , ( " [  ,] "             , False )
    , ( " [ "                 , False )
    , ( " ] "                 , False )
    , ( "[]] "                , False )
    , ( "[,]"                 , False )
    , ( "[\"a]"               , False )
    , ( " [a,b,c,int ] "      , False )
    , ( " [ [ ] ] "           , False ) ]

tPairLiter
  = [ ( "null   "  , True  )
    , ( ""         , False )
    , ( "null   a" , False )
    , ( "nulla "   , False )
    , ( "Null "    , False ) ]

-- TODO: identifiers


-- Tests for statements
tStat
  = [ ( "skip"                             ,  True  )
    , ( "int x = 10"                       ,  True  )
    , ( "int[] a = [1,2]"                  ,  True  )
    , ( "pair(int,int) a = newpair(1,1)"   ,  True  )
    , ( "x = a * 4 + 1 "                   ,  True  )
    , ( "read x"                           ,  True  )
    , ( "free x"                           ,  True  )
    , ( "exit 10"                          ,  True  )
    , ( "print \"a\""                      ,  True  )
    , ( "print 1+1"                        ,  True  )
    , ( "println w"                        ,  True  )
    , ( "if (1 > 2) then skip else a=2 fi" ,  True  )
    , ( "while( true ) do   skip done"     ,  True  )
    , ( "begin   skip  end"                ,  True  )
    , ( "skip ;  a=2"                      ,  True  )
    , ( "skip ; skip ; skip ;   skip "     ,  True  )
    , ( "read 1"                           ,  False )
    , ( "println w!"                       ,  False )
    , ( "if (1 > 2) then skip else a=2"    ,  False )
    , ( "while(true) do done"              ,  False )
    , ( "while() do done"                  ,  False )
    , ( "pair(int,int) = newpair(1,1)"     ,  False )
    , ( "skip    skip "                    ,  False )
    , ( "skip;    skip; "                  ,  False )
    , ( "skip ; skip skip; "               ,  False )  ]


-- Tests for expressions

-- |For Int, Bool, Char, Str, Pair literals see Section 3.2.1 Literals
-- TODO:

-- Tests for programs and function
-- TODO:




-- :: Unit Test Framework ::::::::::::::::::::::::::::::::::::::::::::::::::: --

-- |Runs all tests, grouped by parser and outputs for each whether all the tests
-- have passed. Will display details only for the failed tests.
runAll = do
    runTests "pBoolLiter"  pBoolLiter  tBoolLiter
    runTests "pIntLiter"   pIntLiter   tIntLiter
    runTests "pCharLiter"  pCharLiter  tCharLiter
    runTests "pStrLiter"   pStrLiter   tStrLiter
    runTests "pPairLiter"  pPairLiter  tPairLiter
    runTests "pArrayLiter" pArrayLiter tArrayLiter
    
    runTests "pStat"       pStat       tStat
 
      where

          -- |Runs the parser test for all pairs in the list
          runTests fname parser tests = do
              -- number of passed tests
              let p = length $ filter (verifyTest fname parser) tests
              -- total number of tests
              let t = length tests

              -- if all tests pass
              if p == t
              -- will print the success message
                  then putStrLn $ fname ++ "\tSUCCESS! Passed " 
                      ++ show t ++ "/" ++ show t ++ "\n"

              -- will print extra details about each failed test
                  else putStrLn $ fname ++ "\tFAILURE! Passed " 
                      ++ show p ++ "/" ++ show t ++ "\n"
                      ++ ( concatMap ( runTest fname parser ) tests )

          -- |Verifies if an individual pair test passed
          verifyTest fname parser ( input , pass )
              = case parseWithEof parser input of
                  Left  e -> not pass
                  Right r -> pass

-- |Runs a test individually. To be used when testing individual pairs in the
-- command line.
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


