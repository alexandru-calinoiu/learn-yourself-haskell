import           Data.Char (toUpper)
import           System.IO (IOMode (ReadMode), hClose, hGetContents, openFile,
                            withFile)

main :: IO ()
main = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriendcaps.txt" $ map toUpper contents

main'' = withFile "girlfriend.txt" ReadMode (\handle -> do
  contents <- hGetContents handle
  putStr contents)

main' = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
