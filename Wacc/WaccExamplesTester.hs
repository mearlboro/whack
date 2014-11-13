module Wacc.WaccExamplesTester where

import Wacc.WaccParser
import Wacc.WaccLanguageDef

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Control.Monad.IO.Class ( liftIO )
import System.Directory
import Data.List 
import Control.Applicative
import Control.Monad
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
    let extract name = do {
      let path = dir </> name 
    ; isDirectory <- doesDirectoryExist path
    ; if   isDirectory 
      then getRecursiveContents path 
      else return [( name , path )]
    }
    concat <$> forM children extract

--------------------------------------------------------------------------------

-- | Parses one wacc file and returns true if it was parsed correctly
parseOne :: Bool 
         -> File    -- * Is it supposed to pass (True) or fail (False) ?
         -> IO Bool
parseOne shouldPass ( name , path ) = do 
  -- Read source file 
  source <- readFile path -- putStrLn $ source
  
  -- Parse source file
  let result = parseWithEof pProgram source
  
  -- Error message expected by LabTS
  let errorMessage = "" -- "#syntax_error#\nexit:\n100\n"

  -- Invalid program yet the parser parsed something... but what? 
  let handlePhantomParse r = do 
      putStrLn $ "FAILED (" ++ path ++ ")\n" ++ show r
      -- "Parser was supposed to fail but it parsed this: " ++ show r 
  
  -- Parser failed to parse something it was supposed to be able to parse
  let handleFail e = do 
      putStrLn $ replicate 80 '~' ++ "\nFAILED (" ++ path ++ ")\n" ++ show e

  -- Get the result and act accordingly
  case result of -- putStrLn $ show result
      Right r -> if   shouldPass 
                 then return True 
                 else handlePhantomParse r >> return False

      Left  e -> if   shouldPass 
                 then handleFail e >> return False 
                 else putStr errorMessage >> return True

--------------------------------------------------------------------------------

parseBunch shouldPass dir = do 
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
  parseBunch True ( pwd ++ "valid" ) 
  -- Check invalid programs
  parseBunch False ( pwd ++ "invalid" ) 

 
