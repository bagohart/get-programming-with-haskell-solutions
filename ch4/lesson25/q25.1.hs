import System.Environment
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad

-- difference between the number of characters in a file and the number of bytes
main :: IO ()
main = do
    fileName <- head <$> getArgs
    byteLength <- BC.length <$> BC.readFile fileName
    charNum <- T.length <$> TIO.readFile fileName
    putStrLn ("Number of bytes: " ++ show byteLength)
    putStrLn ("Number of characters: " ++ show charNum)
    putStrLn ("Difference: " ++ show (byteLength - charNum))

