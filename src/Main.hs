{-# Language OverloadedStrings #-}
{-# Language ApplicativeDo #-}
module Main where

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
  MP.choice [MP.eol $> '\r', MP.char ';'] $> Separator <?> "command seperator"

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
      ]
    <?> "valid word character"

regularWord :: Parser T.Text
regularWord = T.pack <$> MP.some wordChar

word :: Parser Token
word = Word <$> MP.choice [stringWord, regularWord]

matchedBraces :: Parser String
matchedBraces = do
  _    <- MP.char '{'
  l    <- nonbrace
  rest <- concat <$> MP.many
    (do
      inner <- matchedBraces
      rest  <- nonbrace
      return $ "{" ++ inner ++ "}" ++ rest
    )
  _ <- MP.char '}'
  return $ l ++ rest
  where nonbrace = MP.many (MP.noneOf ['{', '}'])

braceWord :: Parser Token
braceWord = BraceWord . T.pack <$> matchedBraces

bracketWord :: Parser Token
bracketWord = BracketWord <$> MP.between (MP.char '[') (MP.char ']') tcl

tcl :: Parser [Token]
tcl = MP.choice [seperator, comment, word, braceWord] `MP.sepEndBy` MP.space

tclProgram :: Parser [Token]
tclProgram = tcl <* MP.eof

main :: IO ()
main = do
  input <- T.pack <$> getContents
  MP.parseTest tclProgram input
