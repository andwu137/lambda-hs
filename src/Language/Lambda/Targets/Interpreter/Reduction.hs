module Language.Lambda.Targets.Interpreter.Reduction (
    beta,
    tryBeta,
    lookupApply,
    eval,
    whnf,
    whnfConst,
    whnfConstDef,
) where

import Control.Monad ((>=>))
import qualified Data.Text as T
import Language.Lambda.Expr
import Language.Lambda.Targets.Interpreter.Core
import Prelude hiding (lookup)

lookupApply :: (Monad m) => T.Text -> Expr -> InterT m (Output m)
lookupApply sym x =
    lookup sym >>= \case
        Const e -> tryBeta e x
        Builtin f -> f x

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

tryBeta :: (Monad m) => Expr -> Expr -> InterT m (Output m)
tryBeta = \cases
    (Ident sym) inp -> lookupApply sym inp
    (Abs x b) inp -> pure . Const $ beta b x inp
    x@(App{}) y -> whnfFirst x y
    x@(Op{}) y -> whnfFirst x y
    _ _ -> throwE "Left hand side of application is not an abstraction nor identifier"
  where
    whnfFirst x y =
        whnf x >>= \case
            Builtin f -> f y
            Const e -> tryBeta e y

whnf :: (Monad m) => Expr -> InterT m (Output m)
whnf =
    \case
        App f x ->
            whnf f >>= \case
                Builtin f' -> f' x
                Const e -> tryBeta e x >>= tryWhnf
        Op o x y ->
            tryBeta (Ident o) x >>= \case
                Builtin f' -> f' y
                Const e -> tryBeta e y >>= tryWhnf
        x -> pure $ Const x
  where
    tryWhnf = \case
        f@(Builtin _) -> pure f
        Const c -> whnf c

whnfConst :: (Monad m) => Expr -> (Expr -> InterT m ()) -> InterT m ()
whnfConst x f =
    whnf x >>= \case
        Builtin _ -> throwE "Unable to print built-in"
        Const x' -> f x'

whnfConstDef :: (Monad m) => Expr -> InterT m a -> (Expr -> InterT m a) -> InterT m a
whnfConstDef x d f =
    whnf x >>= \case
        Builtin _ -> d
        Const x'@(App{}) -> whnfConstDef x' d f
        Const x'@(Op{}) -> whnfConstDef x' d f
        Const x' -> f x'

eval :: (Monad m) => Expr -> InterT m Expr
eval =
    \case
        b@(Bool{}) -> pure b
        u@Unit -> pure u
        z@(Z{}) -> pure z
        r@(R{}) -> pure r
        s@(String{}) -> pure s
        a@(Abs _ _) -> pure a
        Ident i ->
            lookup i >>= \case
                Builtin _ -> throwE "Unable to eval builtin"
                Const c -> eval c
        a@(App{}) -> evalAgain a
        o@(Op{}) -> evalAgain o
  where
    evalAgain =
        whnf >=> \case
            Builtin _ -> throwE "Unable to eval builtin"
            Const c -> eval c
