module Language.Lambda.Expr (
    Statement (..),
    Expr (..),
    expr,
    lambdaFile,
    lambdaLine,
) where

import Data.Foldable (Foldable (..))
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
    = Unit
    | Undefined
    | Ident T.Text
    | Z Integer
    | R Double
    | String String
    | Bool Bool
    | Abs T.Text Expr
    | App Expr Expr
    | Op T.Text Expr Expr
    | Strict Expr
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

chainr1 :: (P.MonadParsec e s m) => m a -> m (a -> a -> a) -> m a
chainr1 p o =
    p >>= rest
  where
    rest x = P.try (o <*> pure x <*> (p >>= rest)) <|> pure x

beta :: T.Text -> Expr -> Expr -> Expr
beta inpName inp = \case
    i@(Ident idt)
        | idt == inpName -> inp
        | otherwise -> i
    a@(Abs n b)
        | n == inpName -> a
        | otherwise -> Abs n (next b)
    App x y -> App (next x) (next y)
    Op o x y -> Op o (next x) (next y)
    x -> x
  where
    next = beta inpName inp

{- Syntax -}
lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

absOpen :: Parser T.Text
absOpen = symbol "\\"

absClose :: Parser T.Text
absClose = symbol "->"

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
            <$> P.lowerChar
            <*> (T.pack <$> P.many followChar)
    infixToPrefix = P.between (symbol "(") (symbol ")") (operL <|> operR)

oper :: Parser T.Text
oper = operL <|> operR

operComb :: (Foldable f) => f (Parser T.Text) -> Parser T.Text
operComb ps =
    lexeme . fmap T.concat . P.some $
        P.choice ps

operL :: Parser T.Text
operL =
    P.label "operatorL" $
        operComb ["!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "^", "|", "-"]

operR :: Parser T.Text
operR =
    P.label "operatorR" $
        T.cons <$> P.char '~' <*> operComb ["!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "^", "|", "-"]

{- Atoms -}
atom :: Parser Expr
atom =
    optionalParenRec $
        P.choice
            [ P.try bool
            , undef
            , unit
            , ident
            , string
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
unit = P.label "Unit" $ symbol "Unit" $> Unit

undef :: Parser Expr
undef = P.label "Undefined" $ symbol "Undefined" $> Undefined

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
ident =
    P.label "Identifier" $ do
        i <- strIdent
        if i `notElem` reserved
            then pure $ Ident i
            else P.label "identifier" P.empty

z :: Parser Expr
z =
    P.label "Z" $
        Z <$> lexeme (L.signed P.empty L.decimal)

r :: Parser Expr
r =
    P.label "R" $
        R <$> lexeme (L.signed P.empty L.float)

reserved :: [T.Text]
reserved = ["let"]

tryStrict :: Parser Expr -> Parser Expr
tryStrict p = (Strict <$ P.char '~' <*> p) <|> p

{- Structures -}
expr :: Parser Expr
expr = P.choice [abs, letExpr, app]

letExpr :: Parser Expr
letExpr = do
    P.try (symbol "let") *> lambdaFile' <* symbol ";" >>= \case
        x : xs -> do
            let func = foldl' (\acc a -> applyAssign a . acc) (applyAssign x) xs
                applyAssign = \case
                    Assign n v -> beta n v
                    Effect{} -> id
            func <$> expr
        [] -> P.label "assignment" P.empty

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
        `chainl1` (Op <$> operL)
        `chainr1` (Op <$> operR)
  where
    oper' = lexeme $ Op <$> oper
    opL = do
        t <- term
        o <- oper'
        pure $ Abs "x" (o t (Ident "x"))
    opR = do
        o <- oper'
        Abs "x" . o (Ident "x") <$> term
    op = paren (P.try opL <|> opR)
    term = do
        P.choice
            [ P.try op
            , P.try $ tryStrict $ paren (P.choice [app, abs, term])
            , atom
            , abs
            ]

{- Statements -}
-- TODO: Error messages
lambdaFile :: Parser [Statement]
lambdaFile = lambdaFile' <* P.eof

lambdaFile' :: Parser [Statement]
lambdaFile' = P.many assign

lambdaLine :: Parser Statement
lambdaLine = P.try (assign <* P.eof) <|> (Effect <$> expr)

assign :: Parser Statement
assign =
    Assign
        <$> P.try strIdent
        <*> do
            a <- args
            void $ symbol "="
            a <$> expr <* symbol ";"
