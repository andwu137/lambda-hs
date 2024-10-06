module Language.Lambda.Targets.Interpreter.Reduction (
    beta,
    tryBeta,
    lookupApply,
    eval,
    evalConst,
    evalConstDef,
    evalNoBuiltin,
) where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as T
import Language.Lambda.Expr
import Language.Lambda.Targets.Interpreter.Core
import Prelude hiding (lookup)

-- >>> let body = Abs "print" (App (Ident "print") (Ident "x"))
-- >>> let inp = Z 5
-- >>> beta body "x" inp
-- Abs "print" (App (Ident "print") (Z 5))
beta :: Expr -> T.Text -> Expr -> Expr
beta b sym inp = case b of
    p@(Bool _) -> p
    i@(Ident sym')
        | sym == sym' -> inp
        | otherwise -> i
    z@(Z{}) -> z
    r@(R{}) -> r
    s@(String{}) -> s
    Unit -> Unit
    a@(Abs sym' b')
        | sym == sym' -> a
        | otherwise -> Abs sym' (beta b' sym inp)
    App f x -> App (beta f sym inp) (beta x sym inp)
    Op o x y -> Op o (beta x sym inp) (beta y sym inp)

-- >>> runInter (tryBeta (Abs "x" (Abs "f" (App (Ident "f") (Ident "x")))) (Z 5)) def
-- >>> runInter (tryBeta (Abs "f" (App (Ident "f") (Z 5))) (Ident "print")) def
-- >>> runInter (tryBeta (Ident "print") (Z 5)) def
-- Right (Abs "f" (App (Ident "f") (Z 5)))
-- Right (App (Ident "print") (Z 5))
-- Right (Z 5)
tryBeta :: Expr -> Expr -> InterT IO (Output IO)
tryBeta = \cases
    (Ident sym) inp -> lookupApply sym inp
    (Abs x b) inp -> pure . Const $ beta b x inp
    _ _ -> throwE "Left hand side of application is not an abstraction nor Identifier"

eval :: Expr -> InterT IO (Output IO)
eval =
    \case
        App f x ->
            eval f >>= \case
                Builtin f' -> f' x
                Const e -> tryBeta e x >>= tryEval
        Op o x y ->
            tryBeta (Ident o) x >>= \case
                Builtin f' -> f' y
                Const e -> tryBeta e y >>= tryEval
        x -> pure $ Const x
  where
    tryEval = \case
        f@(Builtin _) -> pure f
        Const c -> eval c

lookupApply :: T.Text -> Expr -> InterT IO (Output IO)
lookupApply sym x =
    lookup sym >>= \case
        Const e -> tryBeta e x
        Builtin f -> f x

evalConst :: Expr -> (Expr -> InterT IO ()) -> InterT IO ()
evalConst x f =
    eval x >>= \case
        Builtin _ -> liftIO $ putStrLn "Unable to print built-in"
        Const x' -> f x'

evalConstDef :: Expr -> InterT IO a -> (Expr -> InterT IO a) -> InterT IO a
evalConstDef x d f =
    eval x >>= \case
        Builtin _ -> d
        Const x'@(App{}) -> evalConstDef x' d f
        Const x'@(Op{}) -> evalConstDef x' d f
        Const x' -> f x'

evalNoBuiltin :: Expr -> InterT IO Expr
evalNoBuiltin x = evalConstDef x (throwE "expected non builtin") pure
