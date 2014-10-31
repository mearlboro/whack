module WaccParsersTest where

import WaccParser

import Text.ParserCombinators.Parsec


tPairLiter 
  = [ ( "pPairLiter" , pPairLiter , "null"  , True  )
    , ( "pPairLiter" , pPairLiter , ""      , False )
    , ( "pPairLiter" , pPairLiter , " null" , False )
    , ( "pPairLiter" , pPairLiter , " Null" , False )
    , ( "pPairLiter" , pPairLiter , " null" , False )
    , ( "pPairLiter" , pPairLiter , "Null " , False ) ]

tArrayLiter 
  = [ ( "pArrayLiter" , pArrayLiter , "" ,   True  )
    , ( "pArrayLiter" , pArrayLiter , " " ,  True  )
    , ( "pArrayLiter" , pArrayLiter , "(" ,  True  ) ]

tIntSign 
  = [ ( "pIntSign" , pIntSign , "+" ,   True  ) 
    , ( "pIntSign" , pIntSign , "-" ,   True  ) 
    , ( "pIntSign" , pIntSign , "a" ,   True  ) 
    , ( "pIntSign" , pIntSign , "a" ,   True  ) 
    , ( "pIntSign" , pIntSign , "" ,    True  ) ]

tIntLiter
  = [ ( "pIntLiter" , pIntLiter , "12345" , True ) 
    , ( "pIntLiter" , pIntLiter , "+12345" , True )
    , ( "pIntLiter" , pIntLiter , "-12345" , True )
    , ( "pIntLiter" , pIntLiter , "0" , True )
    , ( "pIntLiter" , pIntLiter , "+0" , True )
    , ( "pIntLiter" , pIntLiter , "-0" , True )
    , ( "pIntLiter" , pIntLiter , "000000" , True )
    , ( "pIntLiter" , pIntLiter , "-000111" , True )
    , ( "pIntLiter" , pIntLiter , "+111100" , True )
    , ( "pIntLiter" , pIntLiter , "" , False )
    , ( "pIntLiter" , pIntLiter , "+" , False )
    , ( "pIntLiter" , pIntLiter , "-" , False )
    , ( "pIntLiter" , pIntLiter , "a" , False )
    , ( "pIntLiter" , pIntLiter , "+a" , False )
    , ( "pIntLiter" , pIntLiter , "-a" , False )
    , ( "pIntLiter" , pIntLiter , "hello" , False )
    , ( "pIntLiter" , pIntLiter , "+1x" , True ) ]


runAll = do
  runTests tPairLiter
  runTests tArrayLiter
  runTests tIntSign
  runTests tIntLiter
  runTests tArrayElem


runTest ( fname , parser , input , pass ) = case regularParse parser input of 
  Left  e -> log $ if not pass then "OK it failed!"
                   else "='( Failed but should've passed... ("  ++ show e ++ ")"
  Right r -> log $ if pass then "OK it succeeded! (" ++ show r ++ ")" 
                   else "='( Success but shoudl've failed... (" ++ show r ++ ")"
  where 
    log msg = putStrLn $ fname ++ ":\t\"" ++ input ++ "\"\t ... " ++ msg ++ "\n"



runTests tests = mapM_ runTest tests
   


 
