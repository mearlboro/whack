module WaccParsersTest where

import WaccParser

import Text.ParserCombinators.Parsec


testsPairLiter 
  = [ ( "pPairLiter" , pPairLiter , "null"  , True  )
    , ( "pPairLiter" , pPairLiter , ""      , False )
    , ( "pPairLiter" , pPairLiter , " null" , False )
    , ( "pPairLiter" , pPairLiter , " Null" , False )
    , ( "pPairLiter" , pPairLiter , " null" , False )
    , ( "pPairLiter" , pPairLiter , "Null " , False ) ]

testArrayLiter 
  = [ ( "pArrayLiter" , pArrayLiter , "" ,   True  )
    , ( "pArrayLiter" , pArrayLiter , " " ,  True  )
    , ( "pArrayLiter" , pArrayLiter , "(" ,  True  ) ]


runTest ( fname , parser , input , pass ) = case regularParse parser input of 
  Left  e -> reportTest $ if not pass then 
                            "OK  Failed as expected!"
                          else 
                            "='( Failed but was supposed to pass... ( " 
                            ++ show e ++ ")"

  Right r -> reportTest $ if pass then 
                            "OK  Success as expected!"
                          else 
                            "='( Success but was suppoed to fail... (" 
                            ++ show r ++ ")"
  where
    reportTest msg = putStrLn $ fname ++ ":\t\"" ++ input ++ "\"\t ... " ++ msg ++ "\n"




runTests tests = mapM_ runTest tests
   


 
