module Language.Lambda.Targets.Interpreter.SymbolTable (
    defaultSymbolTable,
) where

import Control.Monad (join, (<=<))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Lambda.Expr
import Language.Lambda.Targets.Interpreter.Core
import Language.Lambda.Targets.Interpreter.Reduction
import Prelude hiding (div)
import qualified Prelude

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
        x' <- evalNoBuiltin x
        y' <- evalNoBuiltin y
        pure $ Const $ Bool (f x' y')

curry2 :: (Monad m) => (Expr -> Expr -> InterT m (Output m)) -> Expr -> InterT m (Output m)
curry2 f = pure . Builtin . f

curry3 :: (Monad m) => (Expr -> Expr -> Expr -> InterT m (Output m)) -> Expr -> InterT m (Output m)
curry3 f x = pure $ Builtin $ \y -> pure $ Builtin $ \z -> f x y z

cast = undefined -- TODO: cast function

if' :: Expr -> Expr -> Expr -> InterT IO (Output IO)
if' b x y = do
    b' <- evalNoBuiltin b
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
        i@(Ident{}) -> pure $ show i
        a@(Abs{}) -> pure $ show a
        a@(App{}) -> myShow =<< evalNoBuiltin a
        o@(Op{}) -> myShow =<< evalNoBuiltin o

add :: Expr -> Expr -> InterT IO (Output IO)
add x y =
    join $ add' <$> evalNoBuiltin x <*> evalNoBuiltin y
  where
    add' = \cases
        (Z a) (Z b) -> pure $ Const $ Z (a + b)
        (R a) (R b) -> pure $ Const $ R (a + b)
        _ _ -> throwE "+: No matching function"

mul :: Expr -> Expr -> InterT IO (Output IO)
mul x y =
    join $
        mul' <$> evalNoBuiltin x <*> evalNoBuiltin y
  where
    mul' = \cases
        (Z a) (Z b) -> pure $ Const $ Z (a * b)
        (R a) (R b) -> pure $ Const $ R (a * b)
        _ _ -> throwE "*: No matching function "

div :: Expr -> Expr -> InterT IO (Output IO)
div x y =
    join $ div' <$> evalNoBuiltin x <*> evalNoBuiltin y
  where
    div' = \cases
        (Z a) (Z b) -> pure $ Const $ Z (a `Prelude.div` b)
        (R a) (R b) -> pure $ Const $ R (a / b)
        _ _ -> throwE "/: No matching function"

sub :: Expr -> Expr -> InterT IO (Output IO)
sub x y =
    join $ sub' <$> evalNoBuiltin x <*> evalNoBuiltin y
  where
    sub' = \cases
        (Z a) (Z b) -> pure $ Const $ Z (a - b)
        (R a) (R b) -> pure $ Const $ R (a - b)
        _ _ -> throwE "-: No matching function"
