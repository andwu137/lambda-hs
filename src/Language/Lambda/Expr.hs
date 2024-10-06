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
strIdent = lexeme (T.cons <$> P.letterChar <*> (T.pack <$> P.many P.alphaNumChar))

operator :: Parser T.Text
operator =
    lexeme . fmap T.concat . P.some $
        P.choice $
            symbol
                <$> ["!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "^", "|", "-", "~"]

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
expr = P.label "Expression" $ P.choice [oper, abs, app]

abs :: Parser Expr
abs =
    P.label "Abstraction" $
        Abs
            <$> (absOpen *> lexeme strIdent <* absClose)
            <*> expr

oper :: Parser Expr
oper =
    P.label "Operator" $ term `chainl1` (Op <$> operator)
  where
    term =
        P.choice
            [ paren (P.choice [oper, abs, term])
            , app
            , atom
            ]

app :: Parser Expr
app =
    P.label "Application" $ term `chainl1` pure App
  where
    term =
        P.choice
            [ paren (P.choice [oper, app, abs, term])
            , atom
            ]

{- Statements -}
-- TODO: Error messages
lambdaFile :: Parser [Statement]
lambdaFile = P.many lambdaLine <* P.eof

lambdaLine :: Parser Statement
lambdaLine = P.try assign <|> (Effect <$> expr)

assign :: Parser Statement
assign =
    P.label "assignment" $
        liftA2
            Assign
            (P.try strIdent <* lexeme (P.string "="))
            (expr <* lexeme (P.char ';'))
