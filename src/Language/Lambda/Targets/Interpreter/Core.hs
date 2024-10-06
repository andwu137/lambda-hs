module Language.Lambda.Targets.Interpreter.Core (
    SymbolTable,
    Output (..),
    InterT (..),
    runInterT,
    evalInterT,
    get,
    put,
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
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Lambda.Expr
import Prelude hiding (lookup)

{- SymbolTable -}
type SymbolTable m = M.Map T.Text (Output m)

data Output m
    = Builtin (Expr -> InterT m (Output m))
    | Const Expr

debugSymbolTable :: M.Map T.Text (Output m) -> M.Map T.Text Expr
debugSymbolTable = fmap $ \case
    Builtin _ -> String "Built-In"
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

throwE :: (Monad m) => T.Text -> InterT m a
throwE = InterT . lift . E.throwE

catchE :: (Monad m) => InterT m a -> (T.Text -> InterT m a) -> InterT m a
catchE x f = either f pure =<< lift . evalInterT x =<< get

runInterT :: InterT m a -> SymbolTable m -> m (Either T.Text (a, SymbolTable m))
runInterT i st = E.runExceptT . flip S.runStateT st . unInterT $ i

evalInterT :: (Monad m) => InterT m a -> SymbolTable m -> m (Either T.Text a)
evalInterT i st = E.runExceptT . flip S.evalStateT st . unInterT $ i

lookup :: (Monad m) => T.Text -> InterT m (Output m)
lookup sym = do
    get >>= \st -> case sym `M.lookup` st of
        Nothing -> throwE $ "Symbol does not exist: " <> sym
        Just x -> pure x
