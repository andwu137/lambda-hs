module Language.Lambda.Parser.Expr (
    Statement (..),
    Expr (..),
    expr,
    oper',
    skip,
    lambdaFile,
    lambdaLine,
) where

import Data.Foldable (Foldable (..))
import Data.Functor (void, ($>))
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (abs)

type Parser = P.Parsec Void Text.Text

data Statement
    = Assign !Text.Text !Expr
    | Effect !Expr
    deriving (Show, Eq)

data Expr
    = Unit
    | Undefined
    | Ident !Text.Text
    | Z !Integer
    | R !Double
    | String !String
    | Bool !Bool
    | Abs !Text.Text !Expr
    | App !Expr !Expr
    | Op !Text.Text !Expr !Expr
    | Strict !Expr
    deriving (Show, Eq, Ord)

{- Helpers -}
skip :: Parser ()
skip = L.space P.space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip

symbol :: Text.Text -> Parser Text.Text
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

beta :: Text.Text -> Expr -> Expr -> Expr
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

absOpen :: Parser Text.Text
absOpen = symbol "\\"

absClose :: Parser Text.Text
absClose = symbol "->"

optionalParen :: Parser a -> Parser a
optionalParen p = paren p <|> p

paren :: Parser a -> Parser a
paren = lexeme . P.between (symbol "(") (symbol ")")

optionalParenRec :: Parser a -> Parser a
optionalParenRec p = parenRec p <|> p

parenRec :: Parser a -> Parser a
parenRec p =
    lexeme go
  where
    go = do
        void $ symbol "("
        x <- go <|> p
        void $ symbol ")"
        pure x

strIdent :: Parser Text.Text
strIdent =
    lexeme $ standard <|> infixToPrefix
  where
    followChar = P.alphaNumChar <|> P.choice (P.char <$> ['\'', '_'])
    standard =
        Text.cons
            <$> P.lowerChar
            <*> (Text.pack <$> P.many followChar)
    infixToPrefix = P.between (symbol "(") (symbol ")") (operL <|> operR)

oper :: Parser (Expr -> Expr -> Expr)
oper = lexeme $ Op <$> oper'

oper' :: Parser Text.Text
oper' = operL <|> operR

operComb :: (Foldable f) => f (Parser Text.Text) -> Parser Text.Text
operComb ps = do
    o <-
        lexeme . fmap Text.concat . P.some $
            P.choice ps
    if o `notElem` reservedOper
        then pure o
        else P.label "operator" P.empty

operL :: Parser Text.Text
operL =
    P.label "operatorL" $
        operComb ["!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "^", "|", "-"]

operR :: Parser Text.Text
operR =
    P.label "operatorR" $
        Text.cons <$> P.char '~' <*> operComb ["!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "^", "|", "-"]

{- Atoms -}
atom :: Parser Expr
atom =
    P.choice
        [ bool
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
                    [ P.string "True" $> True
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
    oneChar = (P.string "\\\"" $> '"') <|> P.anySingleBut '"'

ident :: Parser Expr
ident =
    P.label "Identifier" $ do
        i <- strIdent
        if i `notElem` reservedName
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

reservedName :: [Text.Text]
reservedName = ["let"]

reservedOper :: [Text.Text]
reservedOper = ["="]

tryStrict :: Parser Expr -> Parser Expr
tryStrict p = (Strict <$ P.char '~' <*> p) <|> p

{- Structures -}
expr :: Parser Expr
expr = P.try letExpr <|> app

letExpr :: Parser Expr
letExpr = do
    symbol "let" *> lambdaFile' <* symbol ";" >>= \case
        x : xs -> do
            let func = foldl' (\acc a -> applyAssign a . acc) (applyAssign x) xs
                applyAssign = \case
                    Assign n v -> beta n v
                    Effect{} -> id
            func <$> expr
        [] -> P.label "assignment" P.empty

app :: Parser Expr
app =
    term
        `chainl1` pure App
        `chainl1` (Op <$> operL)
        `chainr1` (Op <$> operR)
  where
    term = do
        P.choice
            [ P.try $ paren (sidedOper <|> abs)
            , P.try $ tryStrict $ paren (app <|> term)
            , atom
            , abs
            ]

    sidedOper = P.try sidedOperL <|> sidedOperR
    sidedOperL = do
        t <- app
        o <- oper
        pure $ Abs "x" (o t (Ident "x"))
    sidedOperR = do
        o <- oper
        Abs "x" . o (Ident "x") <$> app

abs :: Parser Expr
abs =
    (absOpen *> args <* absClose)
        <*> expr

args :: Parser (Expr -> Expr)
args =
    P.label "arguments" $
        foldl (\f x -> f . Abs x) id
            <$> P.many (lexeme strIdent)

{- Statements -}
lambdaFile :: Parser [(P.SourcePos, Statement)]
lambdaFile = P.many ((,) <$> P.getSourcePos <*> assign) <* P.eof

lambdaFile' :: Parser [Statement]
lambdaFile' = P.many assign

lambdaLine :: Parser Statement
lambdaLine = P.try (assign' <* P.optional (symbol ";") <* P.eof) <|> (Effect <$> expr)

assign :: Parser Statement
assign = assign' <* symbol ";"

assign' :: Parser Statement
assign' =
    Assign
        <$> strIdent
        <*> do
            a <- args
            void $ symbol "="
            a <$> expr
