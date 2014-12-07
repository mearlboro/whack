module Main where

import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix 

import Wacc.Data.DataTypes
import Wacc.Syntax.Parser
import Wacc.Semantics.Checker
import Wacc.CodeGeneration.TransProgram
import Wacc.CodeGeneration.TransCommon (makePretty)

-------------------------------------------------------------------------------

-- |Runs the wacc compiler after checking argument validity.
main = do
    args       <- getArgs
    (valid, f) <- verifyArgs args
    if valid
        then do
            program <- readFile f
            parse program f
        else putStrLn f

-------------------------------------------------------------------------------

-- |Verifies whether the program arguments are compatible with the compiler and
-- returns True or False, and the file name or an error message respectively.
verifyArgs :: [String] -> IO (Bool, String)
verifyArgs args = do
    -- error message to print to the console.
    let msg = "Expecting a .wacc file."

    if length args < 1
        then return (False, msg )
        else do
            -- .wacc file is first argument.
            let file = head args
            exists <- doesFileExist file

            if not $ isWacc file && exists
               then return (False, msg )
               else return (True , file)

        where

            -- | Is the file at the file path a .wacc file?
            isWacc :: FilePath -> Bool
            isWacc = ( (==) ".wacc" ) . takeExtension

-------------------------------------------------------------------------------

-- | Parses the contents of the input .wacc file and exits accordingly.
-- exit   0 : success
-- exit 100 : #syntax_error#
-- exit 200 : #semantic_error#

parse :: String -> FilePath -> IO ()
parse source path = do

  -- Parse source file
  let result = parseWithEof pProgram source

  -- Get the result and act accordingly
  case result of
      Right r -> check r path
      Left  e -> do
        putStrLn $ show e
        exitWith $ ExitFailure 100

-- TODO: type signature
check :: Program -> FilePath -> IO ()
check program path = do
  -- Takes a program AST and gets a list of error
  let (program', errs) = checkProgram program

  -- If list is empty, exit with success
  if ( length errs > 0 )
    then do
        putStrLn $ unlines errs
        exitWith $ ExitFailure 200
    else do
        let assembled = makePretty (evaluateProgram program')
        -- Write the assembly file
        --putStrLn assembled 
        saveAssembled path assembled
        -- putStrLn assemble
        exitWith   ExitSuccess


saveAssembled :: FilePath -> String -> IO ()
saveAssembled path src = do 
  pwd <- getCurrentDirectory 
  writeFile (pwd ++ "/" ++ takeBaseName path ++ ".s") src 


