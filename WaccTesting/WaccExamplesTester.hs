-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :: WACC file tester :::::::::::::::::::::::::::::::::::::::::::::::::::::: --
-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: --

module WaccTesting.WaccExamplesTester where

import Wacc.WaccDataTypes
import Wacc.WaccLanguageDef
import Wacc.WaccParser
import Wacc.WaccSemAugmenter
import Wacc.WaccSemChecker

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Control.Applicative
import Control.Monad.IO.Class ( liftIO )
import Control.Monad
import Data.List 
import System.Directory
import System.FilePath ( (</>) )

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

testOne :: FilePath -> IO ()
testOne path = do 
  -- Read source file 
  source <- readFile $ "wacc_examples/" ++ path -- putStrLn $ source
  -- Parse source file
  let result = parseWithEof pProgram source

  putStrLn $ replicate 80 '@'
  putStrLn $ "Compiling " ++ show path ++ "\n"

  case result of 
    Right r -> do 
        putStrLn $ show r 
        putStrLn ( show $ checkProgram r )
    Left  e -> error $ "Not parsed: " ++ show e 


-- | Parses one wacc file and returns true if it was parsed correctly
parseOne :: Bool 
         -> File    -- * Is it supposed to pass (True) or fail (False) ?
         -> IO Bool
parseOne shouldPass ( name , path ) = do 
    -- Read source file 
    source <- readFile path -- putStrLn $ source
    
    -- Parse source file
    let result = parseWithEof pProgram source
    
    -- Invalid program yet the parser parsed something... 
    let handlePhantomParse r = do 
        putStrLn $ replicate 80 '~' ++ "\nFAILED (" ++ path ++ ")\n" ++ show r 
    
    -- Parser failed to parse something it was supposed to be able to parse
    let handleFail e = do 
        putStrLn $ replicate 80 '~' ++ "\nFAILED (" ++ path ++ ")\n" ++ show e

    -- Get the result and act accordingly 
    case result of 
        -- Performs semantic check
        Right r -> if shouldPass
            then do 
                let errs = unlines ( checkProgram r )
                when ( length errs > 0 ) ( do  
                --    putStrLn ( replicate 80 '*' ++ "\n*..." ++ 
                --               drop ( length path - 74 ) path ++ " *" )
                --    putStrLn $ replicate 80 '*'
  
                --    putStrLn $ show r ++ "\n"
                --    putStrLn $ concat ( replicate 10 "~@" )
                    putStrLn ("Exit: 200\nSemantic Errors: " )
                    putStrLn ( errs ) )

                return True 
            else handlePhantomParse r >> return False

        Left e -> if shouldPass 
            then handleFail e >> return False 
            else putStr "Exit: 100\n" >> return True

--------------------------------------------------------------------------------

parseGroup shouldPass dir = do 
    -- Is the file at file path a Wacc file?
    let isWacc = isSuffixOf ".wacc"
  
    -- All valid or invalid wacc programs
    programs <- filter ( isWacc . snd ) <$> getRecursiveContents dir 
  
    -- Parse all and find out how many passed or failed when they should have
    behavedWell <- length . filter id <$> forM programs ( parseOne shouldPass )
  
    -- The total number of programs parsed
    let total = length programs
  
    -- Print outcome
    putStrLn $ replicate 80 '*'
    putStr   $ show behavedWell ++ "/" ++ show total ++ " :: " 
    putStrLn $ show ( total - behavedWell ) ++ " behaved badly!"  

--------------------------------------------------------------------------------

main = do
  -- ../WaccCompiler.hs_directory/wacc_examples
  pwd <- flip (++) "/wacc_examples/" <$> getCurrentDirectory 
  -- Check valid programs
  parseGroup True  ( pwd ++ "valid"       ) 

  -- Check invalid programs
  parseGroup False ( pwd ++ "syntaxErr"   ) 
  parseGroup True  ( pwd ++ "semanticErr" ) 
