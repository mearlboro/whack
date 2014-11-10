module WaccParsersTest where

import WaccParser
import WaccLanguageDef

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Control.Monad.IO.Class ( liftIO )

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

-- :: <pair-liter> ::= 'null' ::::::::::::::::::::::::::::::::::::::::::::::: --
tPairLiter
  = [ ( ""        , False ) 
    , ( " null "  , True ) 
    , ( "null "   , True )
    , ( " null" , True )
    , ( " "       , False ) 
    , ( " Null "  , False )
    , ( "n"       , False ) 
    , ( " a "     , False ) 
    , ( " , "     , False )
    , ( "null"  , True  )
    , ( "null a", False )
    , ( "nulla" , False )
    , ( "Null " , False ) ]
 

    -- :: <pair-liter> ::= 'null' ::::::::::::::::::::::::::::::::::::::::::::::: --

-- :: <array-liter> ::= '[' ( <expr> (',' <expr>)* )? ']' ::::::::::::::::::: --
tArrayLiter
  = [ ( " [] "                                           , True  )
    , ( " [ 1] "                                         , True  )
    , ( " [ [ ] ] "                                      , False )
    , ( " [ true ,    false, '1', 'c',   \" Hello\"] "   , True  )
    , ( " [  ,] "                                        , False )
    , ( " [ "                                            , False )
    , ( " ] "                                            , False ) 
    , ( "[]] "                                           , False ) 
    , ( " [a,b,c,int ] "                                 , False )  
    , ( " [a,b,c,___1 ] "                                , True )
    , ( "[]"              , True  )
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

-- :: <bool-liter> ::= 'true' | 'false' ::::::::::::::::::::::::::::::::::::: --
tBoolLiter 
  = [ ( " true " , True )
    , ( " false " , True )
    , ( " True " , False ) 
    , ( " faltrue " , False )
    , ( " truetrue ", False )
    , ( " true false ", False )
    , ( ",true false ", False )
    , ( "true"        , True  )
   , ( "false"       , True  )
   , ( "False "      , False )
   , ( "0"           , False )
   , ( "trueeeeelol" , False )
   , ( "true"        , True  )
   , ( "false"       , True  )
   , ( "False "      , False )
   , ( "0"           , False )
   , ( "trueeeeelol" , False )]

-- :: <int-liter> ::= <int-sign>? <digit>+ :::::::::::::::::::::::::::::::::: --
tIntLiter
 = [ ( " 0000 "               , True )
   , ( " 1111 "               , True )
   , ( " 0011 "               , True )
   , ( " +0011 "               , True )
   , ( " +1100 "               , True )
   , ( " -99 "               , True )
   , ( " -100 "               , True )
   , ( " hello "               , False ) 
   , ( " 10 hello "               , False )
   , ( " -10 hello "               , False ) 
   , ( " 10, "               , False )
   , ( " 10++10 "               , False )
   ,  ( "12345"   , True  ) 
    , ( "+12345"  , True  )
    , ( "-12345"  , True  )
    , ( "0"       , True  )
    , ( "+0"      , True  )
    , ( "-0"      , True  )
    , ( "000000"  , True  )
    , ( "-000111" , True  )
    , ( "+111100" , True  )
    , ( "+1x"     , False  )
    , ( ""        , False )
    , ( "+"       , False )
    , ( "-"       , False )
    , ( "a"       , False )
    , ( "+a"      , False )
    , ( "-a"      , False )
    , ( "hello"   , False )
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

-- :: 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')' :::::::::::::::::: --
tPairType
  = [ ( "pair(int, int)"           , True  )
    , ( "pair(pair, int)"          , True  )
    , ( "pair(int, bla)"           , False )
    , ( "pair(int, pair(int,int))" , False )
    , ( "pair(int, )"              , False )
    , ( "pair(,)"                  , False )
    , ( "pair()"                   , False )
    , ( " pair (   int ,    bool  )", True)
    , ( "newpair(int, int)"        , False )
    , ( "pair(1,2)"                , False ) ]

-- :: <array-elem> ::= <ident> ('[' <expr> ']')+ ::::::::::::::::::::::::::::::: --
tArrayElem 
    = [ ( " hello[1] "   ,  True ) 
      , ( " a[] "   ,  False ) 
      , ( " a[1+1] "   ,  True) 
      , ( " b[a]"   ,  True ) 
      --, ( " b[a[a]]"   ,  True )  --
      , ( " b[1][]"   ,  False ) 
      , ( " b[1][a][b][c][d] "   ,  True ) 
      , ( " b[true]"   ,  True ) 
      , ( " b [true] "   ,  True )
      , ( " b [true] [a] [d]"   ,  True ) ]

-- :: <type> ::= <base-type> | <array-type> | <pair-type> ::::::::::::::::::: --
tType 
    = [ ( "  int    "                     ,  True ) 
      , ( "  bool   "                    ,  True )
      , ( "  string "                    ,  True ) 
      , ( "  char   "                    ,  True )
      , ( "  int[][]"                    ,  True )
      , ( "  bool[][]"                    ,  True ) 
      , ( "  pair(pair   ,   pair)[][]"                    ,  True ) 
      , ( "  pair(pair   ,   pair)[][]"                    ,  True )
      , ( "  pair(pair(char,int)[]   ,   pair)[][]"                    , False)  ]

-- :: <program> ::= 'begin' <func>* <stat> 'end' :::::::::::::::::::::::::::: --
tBaseType
  = [ ( "int    "   , True ) 
    , ( "bool   "   , True ) 
    , ( "string "   , True )
    , ( "char   "   , True )
    , ( ",char   "  , False ) 
    , ( "achar   "  , False ) 
    , ( ""          , False ) 
    , ( ""          , False ) ]

-- :: <param> ::= <type> <ident> :::::::::::::::::::::::::::::::::::::::::::: --
tParam
  = [ ( " int  g "              , True  ) 
    , ( " bool a "              , True  ) 
    , ( " int    "              , False ) 
    , ( " int a  "              , True  ) 
    , ( " int a  "              , True  ) 
    , ( " bool , "              , False )
    , ( " bool _ "              , True  )
    , ( " @bool _"              , False  )
    , ( " bool _ "              , True  )
    , ( " bool _ "              , True  ) ]

-- :: <param-list> ::= <param> (',' <param>)* ::::::::::::::::::::::::::::::: --
tParamList 
  = [ ( "int a , bool b, int z"   , True ) 
    , ( "int g"  , True )
    , ( "int g , bool zz,,"   , False )
    , ( "    int g , char xx    , bool e    "   , True )
    , ( "int g"   , True )
    , ( "int g"   , True ) ]

tCharLiter 
  = [ ("\'o\'"  ,      True  ) 
    , ("\'\'"   ,      False ) 
    , ("\'\'\'" ,      True  )
    , ("\'" ,          False )
    , ("\'\"\'" ,      True  )
    , ("\'\'\'" ,      True  ) 
    , ("\'.\'" ,       True  ) 
    , ("\'\'\'\'" ,    False )
    , ("\'\'\'\'" ,    False )
    , ("\'0\'" ,       True  ) 
    , ("a" ,           False ) 
    , ("\"a\"" ,       False )
    , ( "a"      , False )
    , ( "\'a\'"  , True  )
    , ( "\'aa\'" , False )
    , ( "\'\'\'" , False )
    , ( "\'0\'"  , True  )
    , ( "\'.\'"  , True  )
    , ( "\'-1\'" , False )
    , ( "\'"     , False )
    , ( "\'\'"   , False )
    , ( "\"a\""  , False ) ]
   --  ++ zip ( map (\c->"\'"++[c]++"\'") [ 'a' .. '\255' ] ) ( repeat True )

tStrLiter 
  = [ (""            , False )
    , ("\""          , False ) 
    , ("\"\""        , True  ) 
    , ("\"\"\""      , False  )
    , ("\"\"o"       , False )
    , ("\"o\""       , True  ) 
    , ("\"o\""       , True  ) -- 
    , ( "\"\"\""       , False )
    , ( "\"\"\"\""     , False )
    , ( "abc"          , False )
    , ( "\"\""         , True  )
    , ( "\"abc\""      , True  )
    , ( "\"a\\\"b\""   , True  )
    , ( "\"a\" \"a \"" , False ) ]
   -- ++ zip ( map (\c->"\""++[c]++"\"") [ 'a' .. '\255' ] ) ( repeat True )

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

-- :: <program> ::= 'begin' <func>* <stat> 'end' :::::::::::::::::::::::::::: --
tProgram 
  = [ ( ""               , False ) 
    , ( "begin skip end" , True ) 
    , ( "begin end" , False ) -- Infinite loop
    , ( "begin return true end" , True ) 
    , ( "end"            , False )
    , ( "beginend"       , False ) ]

-- STAT
tStat
  = [ ( "skip",                             True  )
    , ( "int x = 10" ,                      True  )
    , ( "int[] a = [1,2]",                  True  )
    , ( "pair(int,int) = newpair(1,1)",     False  )
    , ( "pair(int,int)  a= newpair(1,1)",   True  )
    , ( "x = a * 4 + 1 ",                   True  )
    , ( "read x"  ,                         True  )
    , ( "read 1"  ,                         False )
    , ( "free x",                           True  )
    , ( "free 1",                           True )
    , ( "exit 10",                          True  ) 
    , ( "print \"a\"",                      True  )
    , ( "print 1+1",                        True  )
    , ( "println w",                        True  )
    , ( "println w!",                       False )
    , ( "if (1 > 2) then skip else a=2",    False )
    , ( "if (1 > 2) then skip else a=2 fi", True  )
    , ( "while(true) do skip done",         True  )
    , ( "begin end",         False  )
    , ( "while(true) do done",              False ) -- Infinite loop
    , ( "while() do done",                  False )
    , ( "begin skip end",                   True  )
    , ( "skip ; a = 2",                        True  )
    , ( "skip; skip",                       True  ) 
    , ( "skip  ;  skip ",                       True  ) 
    , ( "skip  ;  skip ",                       True  ) 
    , ( "skip  ;  skip ",                       True  ) 
    , ( "skip ; skip ; skip ; skip ; skip ; skip ",                       True  ) 
    , ( "skip ; skip ; skip ; skip ; skip ; skip; ",                      False  ) 
    ]




tFunc -- <func> ::= <type> <ident> '(' <param-list>? ')' 'is' <stat> 'end'
  = [ ( "int f() is skip end" , True ) 
    , ( "asd f() is lol end " , False ) 
    , ( "" , False )
    , ( "" , False )
    , ( "" , False )
    , ( "" , False )
    , ( "" , False )
    , ( "" , False )
    , ( "" , False )
    , ( "" , False ) ]


runTests fname parser tests = mapM_ ( runTest fname parser ) tests
    where
        runTest fname parser ( input , pass ) = case parseWithEof (waccWhiteSpace >> parser) input of 
          Left  e -> if pass     then log $ "='( Failed but should've passed... ("  ++ show e ++ ")" else return ()
          Right r -> if not pass then log $ "='( Success but shoudl've failed... (" ++ show r ++ ")" else return ()
          where 
            log msg = putStrLn $ fname ++ ":\t\"" ++ input ++ "\"\t ... " ++ msg ++ "\n"



   


 
