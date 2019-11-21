{-# Language OverloadedStrings #-}
{-# Language ApplicativeDo #-}
module Tokenizer where

import           Data.Functor
import qualified Data.Void                     as V
import qualified Data.Text                     as T
import           Text.Megaparsec                ( (<|>)
                                                , (<?>)
                                                )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP

type Parser = MP.Parsec V.Void T.Text

data Token
  = Separator
  | Comment T.Text
  | Word T.Text
  | BraceWord T.Text
  | BracketWord [Token]
  deriving (Eq, Show)

seperator :: Parser Token
seperator =
  (<?> "command seperator")
    . ($> Separator)
    . MP.some
    . MP.choice
    $ [MP.eol $> '\r', MP.char ';']

comment :: Parser Token
comment =
  Comment <$> MP.between (MP.char '#') MP.eol untilNewline <?> "comment"
  where untilNewline = T.pack <$> MP.many (MP.noneOf ['\n', '\r'])

stringWord :: Parser T.Text
stringWord = MP.between quote quote nonquote <?> "string"
 where
  quote    = MP.char '"'
  nonquote = T.pack <$> MP.many (MP.anySingleBut '"')

wordChar :: Parser Char
wordChar =
  MP.choice
      [ MP.alphaNumChar
      , MP.char ':'
      , MP.char '/'
      , MP.char '-'
      , MP.char '.'
      , MP.char '$'
      , MP.char '\''
      , MP.char '>'
      , MP.char '<'
      , MP.char '+'
      , MP.char '*'
      , MP.char '?'
      ]
    <?> "valid word character"

regularWord :: Parser T.Text
regularWord = T.pack <$> MP.some wordChar

word :: Parser Token
word = Word <$> MP.choice [stringWord, regularWord]

matchedBraces :: Parser String
matchedBraces = do
  void $ MP.char '{'
  l    <- nonbrace
  rest <- concat <$> MP.many
    (do
      inner <- matchedBraces
      rest  <- nonbrace
      return $ "{" ++ inner ++ "}" ++ rest
    )
  void $ MP.char '}'
  return $ l ++ rest
  where nonbrace = MP.many (MP.noneOf ['{', '}'])

braceWord :: Parser Token
braceWord = BraceWord . T.pack <$> matchedBraces

bracketWord :: Parser Token
bracketWord = BracketWord <$> MP.between (MP.char '[') (MP.char ']') tcl

-- TODO: does this cover all whitespace characters?
whitespace :: Parser ()
whitespace = MP.hidden . ($> ()) . MP.many . MP.oneOf $ [' ', '\t']
--whitespace :: Parser ()
--whitespace = MP.hidden MP.space

tcl :: Parser [Token]
tcl = MP.choice [seperator, comment, word, braceWord] `MP.sepEndBy` whitespace

tclProgram :: Parser [Token]
tclProgram = tcl <* MP.eof

tokenize
  :: String -> T.Text -> Either (MP.ParseErrorBundle T.Text V.Void) [Token]
tokenize = MP.parse tclProgram

printResult :: Either (MP.ParseErrorBundle T.Text V.Void) [Token] -> IO ()
printResult (Left  error ) = putStrLn $ MP.errorBundlePretty error
printResult (Right tokens) = mapM_ print tokens

isWord :: Token -> Bool
isWord Separator       = False
isWord (Comment     _) = False
isWord (Word        _) = True
isWord (BraceWord   _) = True
isWord (BracketWord _) = True
