module Language.Lambda.Targets.Interpreter.Core (
    SymbolTable,
    Output (..),
    InterT (..),
    runInterT,
    evalInterT,
    get,
    put,
    modify,
    union,
    fromList,
    throwE,
    catchE,
    lookup,
    debugSymbolTable,
) where

import Control.Applicative (Alternative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State as S
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Lambda.Expr
import Prelude hiding (lookup)

{- SymbolTable -}
type SymbolTable m = M.Map T.Text (Output m)

fromList :: (Monad m) => [(T.Text, Output m)] -> InterT m (M.Map T.Text (Output m))
fromList st =
    sequence $
        M.fromListWithKey
            errorDuplicateSymbolBound
            (fmap (InterT . pure) <$> st)

errorDuplicateSymbolBound :: (Monad m) => T.Text -> p1 -> p2 -> InterT m a
errorDuplicateSymbolBound k _ _ = throwE $ "Unexpected duplicate symbol bound: " <> k

data Output m
    = Builtin (Expr -> InterT m (Output m))
    | Const Expr

debugSymbolTable :: M.Map T.Text (Output m) -> M.Map T.Text Expr
debugSymbolTable = fmap $ \case
    Builtin _ -> Ident "Built-In"
    Const c -> c

{- Inter -}
newtype InterT m a
    = InterT {unInterT :: S.StateT (SymbolTable m) (E.ExceptT T.Text m) a}
    deriving (Functor, Applicative, Monad, Alternative, MonadIO)

instance MonadTrans InterT where
    lift mx = InterT $ S.StateT $ \y -> do
        E.ExceptT $ Right . (,y) <$> mx

get :: (Monad m) => InterT m (SymbolTable m)
get = InterT S.get

put :: (Monad m) => SymbolTable m -> InterT m ()
put = InterT . S.put

modify :: (Monad m) => (SymbolTable m -> SymbolTable m) -> InterT m ()
modify = InterT . S.modify

union :: (Monad m) => SymbolTable m -> InterT m ()
union st2 = do
    st1 <- get
    st' <-
        sequence $
            M.unionWithKey
                errorDuplicateSymbolBound
                (InterT . pure <$> st1)
                (InterT . pure <$> st2)
    put st'

throwE :: (Monad m) => T.Text -> InterT m a
throwE = InterT . lift . E.throwE

catchE :: (Monad m) => InterT m a -> (T.Text -> InterT m a) -> InterT m a
catchE x f = do
    res <- lift . runInterT x =<< get
    case res of
        Left e -> f e
        Right (a, st) -> put st $> a

runInterT :: InterT m a -> SymbolTable m -> m (Either T.Text (a, SymbolTable m))
runInterT i st = E.runExceptT . flip S.runStateT st . unInterT $ i

evalInterT :: (Monad m) => InterT m a -> SymbolTable m -> m (Either T.Text a)
evalInterT i st = E.runExceptT . flip S.evalStateT st . unInterT $ i

lookup :: (Monad m) => T.Text -> InterT m (Output m)
lookup sym = do
    get >>= \st -> case sym `M.lookup` st of
        Nothing -> throwE $ "Symbol does not exist: " <> sym
        Just x -> pure x
