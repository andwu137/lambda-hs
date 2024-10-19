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
import qualified Data.Text as Text
import qualified Language.Lambda.Parser as Parser
import qualified Language.Lambda.Targets.Interpreter.Core as I
import Prelude hiding (lookup)

lookupApply :: (Monad m) => Text.Text -> Parser.Expr -> I.InterT m (I.Output m)
lookupApply sym x =
    I.lookup sym >>= \case
        I.Const e -> tryBeta e x
        I.Builtin f -> f x

beta :: Parser.Expr -> Text.Text -> Parser.Expr -> Parser.Expr
beta b sym inp = case b of
    Parser.Undefined -> Parser.Undefined
    Parser.Strict e -> Parser.Strict $ beta e sym inp
    p@(Parser.Bool _) -> p
    i@(Parser.Ident sym')
        | sym == sym' -> inp
        | otherwise -> i
    z@(Parser.Z{}) -> z
    r@(Parser.R{}) -> r
    s@(Parser.String{}) -> s
    Parser.Unit -> Parser.Unit
    a@(Parser.Abs sym' b')
        | sym == sym' -> a
        | otherwise -> Parser.Abs sym' (beta b' sym inp)
    Parser.App f x -> Parser.App (beta f sym inp) (beta x sym inp)
    Parser.Op o x y -> Parser.Op o (beta x sym inp) (beta y sym inp)

tryBeta :: (Monad m) => Parser.Expr -> Parser.Expr -> I.InterT m (I.Output m)
tryBeta = \cases
    (Parser.Ident sym) inp -> lookupApply sym inp
    (Parser.Abs x b) inp -> pure . I.Const $ beta b x inp
    x@(Parser.App{}) y -> whnfFirst x y
    x@(Parser.Op{}) y -> whnfFirst x y
    Parser.Undefined _ -> I.throwE "Left hand side of application is Undefined"
    _ _ -> I.throwE "Left hand side of application is not an abstraction nor identifier"
  where
    whnfFirst x y =
        whnf x >>= \case
            I.Builtin f -> f y
            I.Const e -> tryBeta e y

whnf :: (Monad m) => Parser.Expr -> I.InterT m (I.Output m)
whnf = \case
    Parser.Strict e -> I.Const <$> eval e
    Parser.App f (Parser.Strict x) -> eval x >>= whnf . Parser.App f
    Parser.App f x ->
        whnf f >>= \case
            I.Builtin f' -> f' x
            I.Const e -> tryBeta e x >>= tryWhnf
    Parser.Op o (Parser.Strict x) y -> eval x >>= \x' -> whnf (Parser.Op o x' y)
    Parser.Op o x (Parser.Strict y) -> eval y >>= \y' -> whnf (Parser.Op o x y')
    Parser.Op o x y ->
        tryBeta (Parser.Ident o) x >>= \case
            I.Builtin f' -> f' y
            I.Const e -> tryBeta e y >>= tryWhnf
    x -> pure $ I.Const x
  where
    tryWhnf = \case
        f@(I.Builtin _) -> pure f
        I.Const c -> whnf c

whnfConst :: (Monad m) => Parser.Expr -> (Parser.Expr -> I.InterT m ()) -> I.InterT m ()
whnfConst x f =
    whnf x >>= \case
        I.Builtin _ -> I.throwE "Unable to print built-in"
        I.Const x' -> f x'

whnfConstDef :: (Monad m) => Parser.Expr -> I.InterT m a -> (Parser.Expr -> I.InterT m a) -> I.InterT m a
whnfConstDef x d f =
    whnf x >>= \case
        I.Builtin _ -> d
        I.Const x'@(Parser.App{}) -> whnfConstDef x' d f
        I.Const x'@(Parser.Op{}) -> whnfConstDef x' d f
        I.Const x' -> f x'

eval :: (Monad m) => Parser.Expr -> I.InterT m Parser.Expr
eval = \case
    Parser.Undefined -> I.throwE "Unable to evaluate undefined"
    Parser.Strict e -> eval e
    b@(Parser.Bool{}) -> pure b
    u@Parser.Unit -> pure u
    z@(Parser.Z{}) -> pure z
    r@(Parser.R{}) -> pure r
    s@(Parser.String{}) -> pure s
    a@(Parser.Abs{}) -> pure a
    Parser.Ident i ->
        I.lookup i >>= \case
            I.Builtin _ -> I.throwE "Unable to evaluate builtin"
            I.Const c -> eval c
    a@(Parser.App{}) -> whnfEvalAgain a
    o@(Parser.Op{}) -> whnfEvalAgain o
  where
    whnfEvalAgain = whnf >=> evalAgain
    evalAgain =
        \case
            I.Builtin _ -> I.throwE "Unable to eval builtin"
            I.Const c -> eval c
