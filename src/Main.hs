module Main where

import qualified Tokenizer
import qualified Parser
import qualified Data.Text                     as T

main :: IO ()
main = do
  input <- T.pack <$> getContents
  let tokens = Tokenizer.tokenize "test.tcl" input
  putStrLn "tokenize results:"
  Tokenizer.printResult tokens
  putStrLn "parseTokens results:"
  mapM_ (Parser.printResult . Parser.parseTokens) tokens
