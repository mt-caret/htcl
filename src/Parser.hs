module Parser where

import qualified Tokenizer
import qualified Data.List.NonEmpty            as NE
import qualified Data.List.Split               as S
import qualified Data.Maybe                    as DM
import qualified Data.Text                     as T

data AstToken
  = Word T.Text
  | BraceWord T.Text
  | BracketWord Ast
  deriving (Eq, Show)

type Command = NE.NonEmpty AstToken

type Ast = NE.NonEmpty Command

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

toAstToken :: Tokenizer.Token -> Either String (Maybe AstToken)
toAstToken Tokenizer.Separator     = Right Nothing
toAstToken (Tokenizer.Comment   _) = Right Nothing
toAstToken (Tokenizer.Word      x) = Right . Just $ Word x
toAstToken (Tokenizer.BraceWord x) = Right . Just $ BraceWord x
toAstToken (Tokenizer.BracketWord tokens) =
  fmap BracketWord . NE.nonEmpty <$> parseTokens tokens

parseTokens :: [Tokenizer.Token] -> Either String [Command]
parseTokens = mapM toAst . S.endBy [Tokenizer.Separator]
 where
  toAst :: [Tokenizer.Token] -> Either String Command
  toAst tokens = do
    astTokens <- NE.nonEmpty . DM.catMaybes <$> mapM toAstToken tokens
    maybeToEither "empty command" astTokens

printResult :: Either String [Command] -> IO ()
printResult (Left  err     ) = putStrLn err
printResult (Right commands) = mapM_ print commands
