module Language.Lambda.Targets.Interpreter.SymbolTable (
    defaultSymbolTable,
) where

import Control.Applicative (Alternative (..))
import Control.Monad (join, (<=<))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor (($>), (<&>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Lambda.Expr
import Language.Lambda.Targets.Interpreter.Core
import Language.Lambda.Targets.Interpreter.Reduction
import Prelude hiding (div, lookup)
import qualified Prelude hiding (lookup)

defaultSymbolTable :: SymbolTable IO
defaultSymbolTable =
    M.fromList
        [ ("print", Builtin myPrint)
        , ("putStrLn", Builtin myPutStrLn)
        , ("show", Builtin $ fmap (Const . String) . myShow)
        , ("+", Builtin $ curry2 add)
        , ("-", Builtin $ curry2 sub)
        , ("*", Builtin $ curry2 mul)
        , ("/", Builtin $ curry2 div)
        , ("if", Builtin $ curry3 if')
        , ("<", Builtin $ curry2 $ cmp (<))
        , ("<=", Builtin $ curry2 $ cmp (<=))
        , (">", Builtin $ curry2 $ cmp (>))
        , (">=", Builtin $ curry2 $ cmp (>=))
        , ("==", Builtin $ curry2 $ cmp (==))
        , ("/=", Builtin $ curry2 $ cmp (/=))
        ]
  where
    cmp f x y = do
        x' <- eval x
        y' <- eval y
        pure $ Const $ Bool (f x' y')

curry2 :: (Monad m) => (Expr -> Expr -> InterT m (Output m)) -> Expr -> InterT m (Output m)
curry2 f = pure . Builtin . f

curry3 :: (Monad m) => (Expr -> Expr -> Expr -> InterT m (Output m)) -> Expr -> InterT m (Output m)
curry3 f x = pure $ Builtin $ \y -> pure $ Builtin $ \z -> f x y z

cast = undefined -- TODO: cast function

if' :: Expr -> Expr -> Expr -> InterT IO (Output IO)
if' b x y = do
    b' <- eval b
    case b' of
        (Bool p) -> pure $ if p then Const x else Const y
        _ -> throwE "if: Expected boolean for condition"

myPrint :: Expr -> InterT IO (Output IO)
myPrint = myPutStrLn <=< fmap String . myShow

myPutStrLn :: Expr -> InterT IO (Output IO)
myPutStrLn = \case
    String s -> lift (putStrLn s) $> Const Unit
    x -> throwE $ "Input was not a string: " <> T.pack (show x)

myShow :: Expr -> InterT IO String
myShow =
    \case
        Bool b -> pure $ show b
        Unit -> pure $ show ()
        Z x -> pure $ show x
        R x -> pure $ show x
        String x -> pure x
        Ident i ->
            lookup i >>= \case
                Builtin _ -> pure (T.unpack i)
                Const x -> myShow x
        a@(Abs{}) -> showAbs a
        a@(App{}) -> myShow =<< eval a
        o@(Op{}) -> myShow =<< eval o

showAbs :: Expr -> InterT IO [Char]
showAbs = \case
    Bool b -> pure $ show b
    Unit -> pure $ show ()
    Z x -> pure $ show x
    R x -> pure $ show x
    String x -> pure x
    Ident i -> pure $ T.unpack i
    Abs f b -> showAbs b <&> \x -> concat ["(λ", T.unpack f, ". ", x, ")"]
    App ml mr -> do
        (\l r -> concat [tryParen ml l, " ", tryParen mr r])
            <$> showAbs ml
            <*> showAbs mr
    Op o ml mr ->
        (\l r -> concat [tryParen ml l, " ", T.unpack o, " ", tryParen mr r])
            <$> showAbs ml
            <*> showAbs mr
  where
    parens x = concat ["(", x, ")"]
    tryParen = \case
        Abs{} -> parens
        Op{} -> parens
        -- App{} -> parens
        _ -> id

add :: Expr -> Expr -> InterT IO (Output IO)
add x y =
    join $ add' <$> eval x <*> eval y
  where
    add' = \cases
        (Z a) (Z b) -> pure $ Const $ Z (a + b)
        (R a) (R b) -> pure $ Const $ R (a + b)
        _ _ -> throwE "+: No matching function"

mul :: Expr -> Expr -> InterT IO (Output IO)
mul x y =
    join $
        mul' <$> eval x <*> eval y
  where
    mul' = \cases
        (Z a) (Z b) -> pure $ Const $ Z (a * b)
        (R a) (R b) -> pure $ Const $ R (a * b)
        _ _ -> throwE "*: No matching function "

div :: Expr -> Expr -> InterT IO (Output IO)
div x y =
    join $ div' <$> eval x <*> eval y
  where
    div' = \cases
        (Z a) (Z b) -> pure $ Const $ Z (a `Prelude.div` b)
        (R a) (R b) -> pure $ Const $ R (a / b)
        _ _ -> throwE "/: No matching function"

sub :: Expr -> Expr -> InterT IO (Output IO)
sub x y =
    join $ sub' <$> eval x <*> eval y
  where
    sub' = \cases
        (Z a) (Z b) -> pure $ Const $ Z (a - b)
        (R a) (R b) -> pure $ Const $ R (a - b)
        _ _ -> throwE "-: No matching function"
