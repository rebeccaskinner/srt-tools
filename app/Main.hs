module Main where
import Data.Text.IO qualified as Text
import System.Environment
import Subrip

main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs
  inputText <- Text.readFile inputFile
  subs <- case parseSubrip inputText of
            Left err -> ioError $ userError err
            Right subs -> pure subs
  Text.writeFile outputFile . renderSubtitles . compressSubtitles $ subs
