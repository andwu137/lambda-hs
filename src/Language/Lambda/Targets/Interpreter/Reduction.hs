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
import qualified Language.Lambda.Expr as E
import qualified Language.Lambda.Targets.Interpreter.Core as I
import Prelude hiding (lookup)

lookupApply :: (Monad m) => T.Text -> E.Expr -> I.InterT m (I.Output m)
lookupApply sym x =
    I.lookup sym >>= \case
        I.Const e -> tryBeta e x
        I.Builtin f -> f x

beta :: E.Expr -> T.Text -> E.Expr -> E.Expr
beta b sym inp = case b of
    p@(E.Bool _) -> p
    i@(E.Ident sym')
        | sym == sym' -> inp
        | otherwise -> i
    z@(E.Z{}) -> z
    r@(E.R{}) -> r
    s@(E.String{}) -> s
    E.Unit -> E.Unit
    a@(E.Abs sym' b')
        | sym == sym' -> a
        | otherwise -> E.Abs sym' (beta b' sym inp)
    E.App f x -> E.App (beta f sym inp) (beta x sym inp)
    E.Op o x y -> E.Op o (beta x sym inp) (beta y sym inp)

tryBeta :: (Monad m) => E.Expr -> E.Expr -> I.InterT m (I.Output m)
tryBeta = \cases
    (E.Ident sym) inp -> lookupApply sym inp
    (E.Abs x b) inp -> pure . I.Const $ beta b x inp
    x@(E.App{}) y -> whnfFirst x y
    x@(E.Op{}) y -> whnfFirst x y
    _ _ -> I.throwE "Left hand side of application is not an abstraction nor identifier"
  where
    whnfFirst x y =
        whnf x >>= \case
            I.Builtin f -> f y
            I.Const e -> tryBeta e y

whnf :: (Monad m) => E.Expr -> I.InterT m (I.Output m)
whnf =
    \case
        E.App f x ->
            whnf f >>= \case
                I.Builtin f' -> f' x
                I.Const e -> tryBeta e x >>= tryWhnf
        E.Op o x y ->
            tryBeta (E.Ident o) x >>= \case
                I.Builtin f' -> f' y
                I.Const e -> tryBeta e y >>= tryWhnf
        x -> pure $ I.Const x
  where
    tryWhnf = \case
        f@(I.Builtin _) -> pure f
        I.Const c -> whnf c

whnfConst :: (Monad m) => E.Expr -> (E.Expr -> I.InterT m ()) -> I.InterT m ()
whnfConst x f =
    whnf x >>= \case
        I.Builtin _ -> I.throwE "Unable to print built-in"
        I.Const x' -> f x'

whnfConstDef :: (Monad m) => E.Expr -> I.InterT m a -> (E.Expr -> I.InterT m a) -> I.InterT m a
whnfConstDef x d f =
    whnf x >>= \case
        I.Builtin _ -> d
        I.Const x'@(E.App{}) -> whnfConstDef x' d f
        I.Const x'@(E.Op{}) -> whnfConstDef x' d f
        I.Const x' -> f x'

eval :: (Monad m) => E.Expr -> I.InterT m E.Expr
eval = \case
    b@(E.Bool{}) -> pure b
    u@E.Unit -> pure u
    z@(E.Z{}) -> pure z
    r@(E.R{}) -> pure r
    s@(E.String{}) -> pure s
    a@(E.Abs{}) -> pure a
    E.Ident i ->
        I.lookup i >>= \case
            I.Builtin _ -> I.throwE "Unable to eval builtin"
            I.Const c -> eval c
    a@(E.App{}) -> evalAgain a
    o@(E.Op{}) -> evalAgain o
  where
    evalAgain =
        whnf >=> \case
            I.Builtin _ -> I.throwE "Unable to eval builtin"
            I.Const c -> eval c
