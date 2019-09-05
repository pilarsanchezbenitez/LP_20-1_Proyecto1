import System.Environment
import ParseBAE
import BAE
import Data.List

main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- parseFile file
      putStrLn $ show $ eval $ transform x
    _ -> putStrLn "Error: Only put the name of the file."