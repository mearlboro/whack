module Main where

import System.Directory
import System.Environment
import System.FilePath.Posix
import Control.Monad ( liftM )

import Wacc.WaccParser

main = do
  args <- getArgs
  if length args < 1 
  then putStrLn "Expecting a .wacc file."
  else do
    let file = head args
    exists <- isFile file
    if not $ isWacc file && exists
       then putStrLn "Expecting a .wacc file."
       else do
         program <- readFile $ file
         parse program


-- TODO: make a file management and compiler module

isFile :: FilePath -> IO Bool
isFile file = do 
   exists <- doesFileExist file
   return $ exists

-- | Is the file at the file path a .wacc file?
isWacc :: FilePath -> Bool
isWacc = ( (==) ".wacc" ) . takeExtension

-- | Parses the contents of the input .wacc file and returns true if it was parsed correctly
parse :: FilePath -> IO ()
parse source = do 
  
  -- Parse source file
  let result = parseWithEof pProgram source
  
  -- Error message expected by LabTS
  let errorMessage = "#syntax_error#\nexit:\n100\n"

  -- Get the result and act accordingly
  case result of
      Right r -> putStrLn "The exit code is 0."

      Left  e -> putStr errorMessage

