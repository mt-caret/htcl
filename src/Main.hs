module Main where

import qualified Tokenizer
import qualified Data.Text                     as T

main :: IO ()
main = do
  input <- T.pack <$> getContents
  Tokenizer.tokenize input
