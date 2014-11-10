module WaccCompiler where

import WaccParser
import WaccLanguageDef

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

-- | Is the file at file path a Wacc file?
isWacc  :: Filter
isWacc  =  isSuffixOf ".wacc"

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
parseOne :: File -> IO Bool
parseOne ( name , path ) = do 

  source <- readFile path -- putStrLn $ source

  let result = parseWithEof pProgram source
  
  let handleFail e = do 
      putStrLn $ replicate 80 '~' 
      putStrLn $ "FAILED (" ++ name ++ ")\n" ++ show e

  case result of -- putStrLn $ show result
      Right _ -> return True
      Left  e -> handleFail e >> return False 

--------------------------------------------------------------------------------

main = do

  -- WaccCompiler.hs directory ../wacc_examples
  pwd <- flip (++) "/wacc_examples/" <$> getCurrentDirectory 

  -- All valid wacc programs
  valids <- filter ( isWacc . snd ) <$> getRecursiveContents ( pwd ++ "valid" ) 

  -- All valid wacc programs
  invalids <- filter ( isWacc . snd ) <$> getRecursiveContents ( pwd ++ "invalid" ) 

  -- Compile all valid programs and find out how many of them passed
  passed <- length . filter id <$> forM valids parseOne

  -- The total number of programs parsed
  let total = length valids

  -- Print outcome
  putStrLn $ replicate 80 '*'
  putStr   $ show passed ++ "/" ++ show total ++ " :: " 
  putStrLn $ show ( total - passed ) ++ " failed!"  

 
