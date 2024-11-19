module Language.Lambda.Targets.Interpreter.SymbolTable (
    defaultSymbolTable,
    if',
    printId,
    print',
    putStrLn',
    show',
    showAbs,
    add,
    mul,
    div,
    sub,
) where

import Control.Monad (join, (<=<))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor (($>), (<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.Lambda.Parser as Parser
import qualified Language.Lambda.Targets.Interpreter.Core as I
import qualified Language.Lambda.Targets.Interpreter.Reduction as I
import Language.Lambda.Utils
import Prelude hiding (div, lookup)
import qualified Prelude hiding (lookup)

defaultSymbolTable :: I.SymbolTable IO
defaultSymbolTable =
    I.SymbolTable $
        Map.fromList
            [ ("print", I.Builtin print')
            , ("putStrLn", I.Builtin putStrLn')
            , ("show", I.Builtin $ fmap (I.Const . Parser.String) . show')
            , ("printId", I.Builtin printId)
            , ("+", I.Builtin $ curry2 add)
            , ("-", I.Builtin $ curry2 sub)
            , ("*", I.Builtin $ curry2 mul)
            , ("/", I.Builtin $ curry2 div)
            , ("if", I.Builtin if')
            , ("<", I.Builtin $ curry2 $ cmp (<))
            , ("<=", I.Builtin $ curry2 $ cmp (<=))
            , (">", I.Builtin $ curry2 $ cmp (>))
            , (">=", I.Builtin $ curry2 $ cmp (>=))
            , ("==", I.Builtin $ curry2 $ cmp (==))
            , ("/=", I.Builtin $ curry2 $ cmp (/=))
            ]
  where
    cmp f x y = do
        x' <- I.eval x
        y' <- I.eval y
        pure $ I.Const $ Parser.Bool (f x' y')

curry2 :: (Monad m) => (Parser.Expr -> Parser.Expr -> I.InterT m (I.Output m)) -> Parser.Expr -> I.InterT m (I.Output m)
curry2 f = pure . I.Builtin . f

curry3 :: (Monad m) => (Parser.Expr -> Parser.Expr -> Parser.Expr -> I.InterT m (I.Output m)) -> Parser.Expr -> I.InterT m (I.Output m)
curry3 f x = pure $ I.Builtin $ \y -> pure $ I.Builtin $ \z -> f x y z

cast = undefined -- TODO: cast function

if' :: (Monad m) => Parser.Expr -> I.InterT m (I.Output m)
if' b = do
    b' <- I.eval b
    case b' of
        (Parser.Bool p) -> do
            let (t, f) = ("t", "f")
                bool = Parser.Abs t . Parser.Abs f
                true = bool $ Parser.Ident t
                false = bool $ Parser.Ident f
            pure . I.Const $ if p then true else false
        _ -> I.throwE "if: Expected boolean for condition"

printId :: Parser.Expr -> I.InterT IO (I.Output IO)
printId x = print' x $> I.Const x

print' :: Parser.Expr -> I.InterT IO (I.Output IO)
print' = putStrLn' . Parser.String <=< show'

putStrLn' :: Parser.Expr -> I.InterT IO (I.Output IO)
putStrLn' = \case
    Parser.String s -> lift (Text.putStrLn s) $> I.Const Parser.Unit
    x -> I.throwE $ "Input was not a string: " <> tshow x

show' :: (Monad m) => Parser.Expr -> I.InterT m Text.Text
show' =
    \case
        Parser.Undefined -> I.eval Parser.Undefined >>= show'
        Parser.Strict e -> show' e <&> \e' -> "~(" <> e' <> ")"
        Parser.Bool b -> pure $ tshow b
        Parser.Unit -> pure "Unit"
        Parser.Z x -> pure $ tshow x
        Parser.R x -> pure $ tshow x
        Parser.String x -> pure x
        Parser.Ident i ->
            I.lookup i >>= \case
                I.Builtin _ -> showAbs $ Parser.Ident i
                I.Const x -> show' x
        a@(Parser.Abs{}) -> showAbs a
        a@(Parser.App{}) -> show' =<< I.eval a
        o@(Parser.Op{}) -> show' =<< I.eval o

showAbs :: (Monad m) => Parser.Expr -> I.InterT m Text.Text
showAbs = \case
    Parser.Undefined -> I.eval Parser.Undefined >>= showAbs
    Parser.Strict e -> showAbs e <&> \e' -> "~(" <> e' <> ")"
    Parser.Bool b -> pure $ tshow b
    Parser.Unit -> pure "Unit"
    Parser.Z x -> pure $ tshow x
    Parser.R x -> pure $ tshow x
    Parser.String x -> pure x
    Parser.Ident i ->
        pure $
            case Parser.parse (Parser.oper' <* Parser.eof) "lambda-interpreter" i of
                Left _ -> i
                Right _ -> "(" <> i <> ")"
    Parser.Abs f b -> showAbs b <&> \x -> Text.concat ["λ", f, ". ", x]
    Parser.App ml mr -> do
        (\l r -> Text.concat [showParens ml l, " ", showParens mr r])
            <$> showAbs ml
            <*> showAbs mr
    Parser.Op o ml mr ->
        (\l r -> Text.concat [showParens ml l, " ", o, " ", showParens mr r])
            <$> showAbs ml
            <*> showAbs mr

parens :: Text.Text -> Text.Text
parens x = Text.concat ["(", x, ")"]

showParens :: Parser.Expr -> Text.Text -> Text.Text
showParens = \case
    Parser.Strict e -> showParens e
    Parser.Op{} -> parens
    Parser.App{} -> parens
    Parser.Abs{} -> parens
    Parser.Undefined -> id
    Parser.Bool{} -> id
    Parser.Unit -> id
    Parser.Z{} -> id
    Parser.R{} -> id
    Parser.String{} -> id
    Parser.Ident{} -> id

add :: (Monad m) => Parser.Expr -> Parser.Expr -> I.InterT m (I.Output m)
add x y =
    join $ add' <$> I.eval x <*> I.eval y
  where
    add' = \cases
        (Parser.Z a) (Parser.Z b) -> pure $ I.Const $ Parser.Z (a + b)
        (Parser.Z a) (Parser.R b) -> pure $ I.Const $ Parser.R (fromInteger a + b)
        (Parser.R a) (Parser.Z b) -> pure $ I.Const $ Parser.R (a + fromInteger b)
        (Parser.R a) (Parser.R b) -> pure $ I.Const $ Parser.R (a + b)
        _ _ -> I.throwE "+: No matching function"

mul :: (Monad m) => Parser.Expr -> Parser.Expr -> I.InterT m (I.Output m)
mul x y =
    join $
        mul' <$> I.eval x <*> I.eval y
  where
    mul' = \cases
        (Parser.Z a) (Parser.Z b) -> pure $ I.Const $ Parser.Z (a * b)
        (Parser.Z a) (Parser.R b) -> pure $ I.Const $ Parser.R (fromInteger a * b)
        (Parser.R a) (Parser.Z b) -> pure $ I.Const $ Parser.R (a * fromInteger b)
        (Parser.R a) (Parser.R b) -> pure $ I.Const $ Parser.R (a * b)
        _ _ -> I.throwE "*: No matching function "

div :: (Monad m) => Parser.Expr -> Parser.Expr -> I.InterT m (I.Output m)
div x y =
    join $ div' <$> I.eval x <*> I.eval y
  where
    div' = \cases
        (Parser.Z a) (Parser.Z b) -> pure $ I.Const $ Parser.Z (a `Prelude.div` b)
        (Parser.Z a) (Parser.R b) -> pure $ I.Const $ Parser.R (fromInteger a / b)
        (Parser.R a) (Parser.Z b) -> pure $ I.Const $ Parser.R (a / fromInteger b)
        (Parser.R a) (Parser.R b) -> pure $ I.Const $ Parser.R (a / b)
        _ _ -> I.throwE "/: No matching function"

sub :: (Monad m) => Parser.Expr -> Parser.Expr -> I.InterT m (I.Output m)
sub x y =
    join $ sub' <$> I.eval x <*> I.eval y
  where
    sub' = \cases
        (Parser.Z a) (Parser.Z b) -> pure $ I.Const $ Parser.Z (a - b)
        (Parser.Z a) (Parser.R b) -> pure $ I.Const $ Parser.R (fromInteger a - b)
        (Parser.R a) (Parser.Z b) -> pure $ I.Const $ Parser.R (a - fromInteger b)
        (Parser.R a) (Parser.R b) -> pure $ I.Const $ Parser.R (a - b)
        _ _ -> I.throwE "-: No matching function"
