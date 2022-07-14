import           Control.Exception    (bracketOnError)
import qualified Data.ByteString.Lazy as B
import           System.Directory     (removeFile, renameFile)
import           System.Environment   (getArgs)
import           System.IO            (hClose, hPutStr, openTempFile)

main = do
  (filename1:filename2:_) <- getArgs
  copy filename1 filename2

copy :: String -> String -> IO ()
copy sourceFileName destFileName = do
  contents <- readFile sourceFileName
  bracketOnError
    (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle contents
      hClose tempHandle
      renameFile tempName destFileName)
