module Language.Lambda.Expr (
    Statement (..),
    Expr (..),
    expr,
    lambdaFile,
    lambdaLine,
) where

import Data.Functor (void, ($>))
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (abs)

type Parser = P.Parsec Void T.Text

data Statement
    = Assign T.Text Expr
    | Effect Expr
    deriving (Show, Eq)

data Expr
    = Ident T.Text
    | Z Integer
    | R Double
    | String String
    | Bool Bool
    | Abs T.Text Expr
    | App Expr Expr
    | Op T.Text Expr Expr
    | Unit
    deriving (Show, Eq, Ord)

{- Helpers -}
skip :: Parser ()
skip = L.space P.space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip

symbol :: T.Text -> Parser T.Text
symbol = L.symbol skip

chainl1 :: (P.MonadParsec e s m) => m a -> m (a -> a -> a) -> m a
chainl1 p o =
    p >>= rest
  where
    rest x = P.try (o <*> pure x <*> p >>= rest) <|> pure x

{- Syntax -}
lineComment :: Parser a
lineComment = P.empty

blockComment :: Parser a
blockComment = P.empty

absOpen :: Parser T.Text
absOpen = lexeme (P.string "\\")

absClose :: Parser T.Text
absClose = lexeme (P.string "->")

optionalParen :: Parser a -> Parser a
optionalParen p = P.try (paren p) <|> p

paren :: Parser a -> Parser a
paren = lexeme . P.between (symbol "(") (symbol ")")

optionalParenRec :: Parser a -> Parser a
optionalParenRec p = P.try (parenRec p) <|> p

parenRec :: Parser a -> Parser a
parenRec p =
    lexeme go
  where
    go = do
        void $ symbol "("
        x <- P.try go <|> p
        void $ symbol ")"
        pure x

strIdent :: Parser T.Text
strIdent =
    lexeme $ standard <|> infixToPrefix
  where
    followChar = P.alphaNumChar <|> P.choice (P.char <$> ['\'', '_'])
    standard =
        T.cons
            <$> P.letterChar
            <*> (T.pack <$> P.many followChar)
    infixToPrefix = P.between (symbol "(") (symbol ")") oper

oper :: Parser T.Text
oper =
    P.label "operator" $
        lexeme . fmap T.concat . P.some $
            P.choice
                ["!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "^", "|", "-", "~"]

{- Atoms -}
atom :: Parser Expr
atom =
    optionalParenRec $
        P.choice
            [ P.try bool
            , ident
            , string
            , unit
            , P.try r
            , z
            ]

bool :: Parser Expr
bool =
    P.label "Bool" $
        lexeme $
            Bool
                <$> P.choice
                    [ P.try (P.string "True" $> True)
                    , P.string "False" $> False
                    ]

unit :: Parser Expr
unit = P.label "Unit" $ lexeme (P.string "Unit") $> Unit

string :: Parser Expr
string =
    P.label "String" $
        lexeme $
            P.between (P.char '"') (P.char '"') $
                String <$> innerString
  where
    innerString = P.many oneChar
    oneChar = P.try (P.string "\\\"" $> '"') <|> P.anySingleBut '"'

ident :: Parser Expr
ident = P.label "Identifier" $ Ident <$> strIdent

z :: Parser Expr
z =
    P.label "Z" $
        Z <$> lexeme (L.signed P.empty L.decimal)

r :: Parser Expr
r =
    P.label "R" $
        R <$> lexeme (L.signed P.empty L.float)

{- Structures -}
expr :: Parser Expr
expr = P.choice [app, abs]

args :: Parser (Expr -> Expr)
args =
    P.label "arguments" $
        foldl (\f x -> f . Abs x) id
            <$> P.many (lexeme strIdent)

abs :: Parser Expr
abs =
    (absOpen *> args <* absClose)
        <*> expr

app :: Parser Expr
app =
    term
        `chainl1` pure App
        `chainl1` (Op <$> oper)
  where
    term =
        P.choice
            [ P.try $ paren (P.choice [app, abs, term])
            , atom
            ]

{- Statements -}
-- TODO: Error messages
lambdaFile :: Parser [Statement]
lambdaFile = P.many assign <* P.eof

lambdaLine :: Parser Statement
lambdaLine = P.try assign <|> (Effect <$> expr)

assign :: Parser Statement
assign =
    Assign
        <$> P.try strIdent
        <*> do
            a <- args
            void $ lexeme (P.string "=")
            a <$> expr <* symbol ";"
