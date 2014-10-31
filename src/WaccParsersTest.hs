module WaccParsersTest where

import WaccParser

import Text.ParserCombinators.Parsec


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



   


 