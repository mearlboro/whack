-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: WACC file tester :::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

module WaccTesting.ExamplesTester where

import Wacc.Data.DataTypes
import Wacc.Semantics.Augmenter
import Wacc.Semantics.Checker
import Wacc.Syntax.LanguageDef
import Wacc.Syntax.Parser
import Wacc.CodeGeneration.ARM11Instructions

import Control.Applicative
import Control.Monad.IO.Class ( liftIO )
import Control.Monad
import Data.List 
import System.Directory
import System.FilePath ( (</>) )
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

--------------------------------------------------------------------------------

type File = ( FilePath , FilePath ) -- ( file name , absolute path )

type Filter = FilePath -> Bool

-- | Is the file at file path a hidden file?
isHidden  :: Filter 
isHidden  =  not . isPrefixOf "."

-- | Returns all non-hidden files in dir and in all its sub dirs if any
getRecursiveContents :: FilePath -> IO [ File ] 
getRecursiveContents dir = do
    children <- filter isHidden <$> getDirectoryContents dir
    let extract name = do 
        let path = dir </> name 
        isDirectory <- doesDirectoryExist path
        if isDirectory 
            then getRecursiveContents path 
            else return [( name , path )]
    concat <$> forM children extract

--------------------------------------------------------------------------------

translateOne :: FilePath -> IO (FilePath, Bool, [ SemanticErr ], [ Instr ])
translateOne path = do 
  source <- readFile path
  let parseFail src = return ()
  let prog = parseWithEof pProgram source
  case prog of 
    Left  e -> parseFail e >> return (path, False, [], [])
    Right p -> do
      let (p', errors) = checkProgram p
      --if null errors then 
        -- assemble
      return (path, True, errors, [])

translateAll :: FilePath -> IO (Int, [(FilePath, Bool, [ SemanticErr ], [ Instr ])])
translateAll dir = do 
  -- Is the file at file path a Wacc file?
  let isWacc = isSuffixOf ".wacc"
  -- All valid or invalid wacc programs
  programs <- filter ( isWacc . snd ) <$> getRecursiveContents dir 
  -- Translate all programs
  out <- forM (map snd programs) translateOne 
  -- The total number of programs parsed
  let total = length programs

  return (total, out)


main = do
  -- ../WaccCompiler.hs_directory/wacc_examples
  pwd <- flip (++) "/WaccTesting/wacc_examples/" <$> getCurrentDirectory

  asd <- translateAll ( pwd ++ "valid" )
  checkProgs False False asd  
  
  --asd' <- translateAll ( pwd ++ "invalid/syntaxErr" )
  --checkProgs True  False asd'

  --asd'' <- translateAll ( pwd ++ "invalid/semanticErr" )  
  --checkProgs False True  asd''


checkProgs :: Bool -> Bool -> (Int, [(FilePath, Bool, [ SemanticErr ], [ Instr ])]) -> IO ()
checkProgs haveSynErr haveSemErr (tot, res) = do 

  let passed (_, syn, sem, _) = (haveSynErr == not syn) && (haveSemErr == not (null sem))
  mapM_ dumpOutput res
  let oks = length $ filter passed res

  putStrLn $ show oks ++ "/" ++ show tot ++ ": " ++ show (tot - oks) ++ " behaved badly"


dumpOutput :: (FilePath, Bool, [ SemanticErr ], [ Instr ]) -> IO ()
dumpOutput (path, syntaxOk, semErrs, _) = do 
  putStrLn $ replicate 80 '~'
  putStrLn $ "Translating " ++ show path ++ "..."
  putStrLn $ if syntaxOk then "Syntax OK" else "Syntax ERRORS"
  putStrLn $ if null semErrs then "Semantics Ok" else show semErrs

  ---- Print outcome
  --putStrLn $ replicate 80 '*'
  --putStr   $ show behavedWell ++ "/" ++ show total ++ " :: " 
  --putStrLn $ show ( total - behavedWell ) ++ " behaved badly!"  
--------------------------------------------------------------------------------


--testOne :: FilePath -> IO ()
--testOne path = do 
--  -- Read source file 
--  source <- readFile $ "WaccTesting/wacc_examples/" ++ path -- putStrLn $ source
--  -- Parse source file
--  let result = parseWithEof pProgram source

--  putStrLn $ replicate 80 '@'
--  putStrLn $ "Compiling " ++ show path ++ "\n"

--  case result of 
--    Right r -> do 
--        putStrLn $ show r 
--        putStrLn ( show $ checkProgram r )
--    Left  e -> error $ "Not parsed: " ++ show e 


---- | Parses one wacc file and returns true if it was parsed correctly
--parseOne :: Bool -> Bool 
--         -> File    -- * Is it supposed to pass (True) or fail (False) ?
--         -> IO Bool
--parseOne shouldPassSyntax shouldPassSem ( name , path ) = do 
--    -- Read source file 
--    source <- readFile path -- putStrLn $ source
    
--    -- Parse source file
--    let result = parseWithEof pProgram source
    
--    -- Prints filename and separators 
--    let putTestDetails name = putStrLn ( replicate 80 '-' ++ "\n" ++ name )

--    -- Invalid program yet the parser parsed something... 
--    let handlePhantomParse r = do 
--        --putTestDetails name
--        putStrLn $ "FAILED!\n" ++ show r 
    
--    -- Parser failed to parse something it was supposed to be able to parse
--    let handleFail e = do 
--        --putTestDetails name
--        putStrLn $ "FAILED!\n" ++ show e 

--    -- Get the result and act accordingly 
--    case result of 
--        -- Parsed correctly
--        Right r -> do 
--          if   not shouldPassSyntax then return False 
--          else return True 
--               --do -- it parsed but it shouldn't have
--               --let errs = (unlines . snd) (checkProgram r)
----
--               --if shouldPassSem && null errs then return True 
--               --else if shouldPassSem && (not $ null errs) then return False 
--               --else if (not shouldPassSem) && ( not $ null errs) then return True
--               --else if (not shouldPassSem) && ( null errs) then return False 
--               --else error "I don't know what's going on!"
--          --if shouldPass
--          --  then do 
--          --      let errs = (unlines . snd) (checkProgram r)
--          --      if ( length errs > 0 ) then do  
--          --           --putTestDetails name
--          --           --putStrLn ("Exit: 200\nSemantic Errors: " )
--          --           --putStrLn ( errs ) 
--          --           return False
--          --      else return True 
--          --  else handlePhantomParse r >> return False

--        Left e -> if   shouldPassSyntax 
--                  then handleFail e >> return False 
--                  else return True
--                 --putTestDetails name
--                 --putStrLn $ show e
--                 --putStr "Exit: 100\n"
                 


--------------------------------------------------------------------------------


--parseGroup syntaxOk semanticOk dir = do 
--    -- Is the file at file path a Wacc file?
--    let isWacc = isSuffixOf ".wacc"
  
--    -- All valid or invalid wacc programs
--    programs <- filter ( isWacc . snd ) <$> getRecursiveContents dir 
  
--    -- Parse all and find out how many passed or failed when they should have
--    behavedWell <- length . filter id <$> forM programs ( parseOne syntaxOk semanticOk )
  
--    -- The total number of programs parsed
--    let total = length programs
  
--    -- Print outcome
--    putStrLn $ replicate 80 '*'
--    putStr   $ show behavedWell ++ "/" ++ show total ++ " :: " 
--    putStrLn $ show ( total - behavedWell ) ++ " behaved badly!"  

--------------------------------------------------------------------------------
