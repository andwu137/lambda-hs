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
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as T
import Language.Lambda.Expr
import Language.Lambda.Targets.Interpreter.Core
import Prelude hiding (lookup)

lookupApply :: T.Text -> Expr -> InterT IO (Output IO)
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

tryBeta :: Expr -> Expr -> InterT IO (Output IO)
tryBeta = \cases
    (Ident sym) inp -> lookupApply sym inp
    (Abs x b) inp -> pure . Const $ beta b x inp
    x@(App{}) y -> whnfFirst x y
    x@(Op{}) y -> whnfFirst x y
    _ _ -> throwE "Left hand side of application is not an abstraction nor Identifier"
  where
    whnfFirst x y =
        whnf x >>= \case
            Builtin f -> f y
            Const e -> tryBeta e y

whnf :: Expr -> InterT IO (Output IO)
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

whnfConst :: Expr -> (Expr -> InterT IO ()) -> InterT IO ()
whnfConst x f =
    whnf x >>= \case
        Builtin _ -> liftIO $ putStrLn "Unable to print built-in"
        Const x' -> f x'

whnfConstDef :: Expr -> InterT IO a -> (Expr -> InterT IO a) -> InterT IO a
whnfConstDef x d f =
    whnf x >>= \case
        Builtin _ -> d
        Const x'@(App{}) -> whnfConstDef x' d f
        Const x'@(Op{}) -> whnfConstDef x' d f
        Const x' -> f x'

eval :: Expr -> InterT IO Expr
eval e =
    case e of
        b@(Bool{}) -> pure b
        u@Unit -> pure u
        z@(Z{}) -> pure z
        r@(R{}) -> pure r
        s@(String{}) -> pure s
        Ident i ->
            lookup i >>= \case
                Builtin _ -> throwE "Unable to eval builtin"
                Const c -> eval c
        Abs _ _ -> throwE "Unable to eval abstraction"
        a@(App{}) -> evalAgain a
        o@(Op{}) -> evalAgain o
  where
    evalAgain =
        whnf >=> \case
            Builtin _ -> throwE "Unable to eval builtin"
            Const c -> eval c
