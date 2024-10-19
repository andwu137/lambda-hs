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
    unionReplace,
    insert,
    insertReplace,
    fromList,
    throwE,
    catchE,
    lookup,
    debugSymbolTable,
) where

import Control.Applicative (Alternative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State as State
import Data.Functor (($>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Language.Lambda.Parser as Parser
import Prelude hiding (lookup)

{- SymbolTable -}
type SymbolTable m = Map.Map Text.Text (Output m)

fromList :: (Monad m) => [(Text.Text, Output m)] -> InterT m (Map.Map Text.Text (Output m))
fromList st =
    sequence $
        Map.fromListWithKey
            errorDuplicateSymbolBound
            (fmap (InterT . pure) <$> st)

errorDuplicateSymbolBound :: (Monad m) => Text.Text -> p1 -> p2 -> InterT m a
errorDuplicateSymbolBound k _ _ = throwE $ "Unexpected duplicate symbol bound: " <> k

data Output m
    = Builtin (Parser.Expr -> InterT m (Output m))
    | Const Parser.Expr

debugSymbolTable :: SymbolTable m -> Map.Map Text.Text Parser.Expr
debugSymbolTable = fmap $ \case
    Builtin _ -> Parser.String "Built-In"
    Const c -> c

{- Inter -}
newtype InterT m a
    = InterT {unInterT :: State.StateT (SymbolTable m) (Except.ExceptT Text.Text m) a}
    deriving (Functor, Applicative, Monad, Alternative, MonadIO)

instance MonadTrans InterT where
    lift mx = InterT $ State.StateT $ \y -> do
        Except.ExceptT $ Right . (,y) <$> mx

get :: (Monad m) => InterT m (SymbolTable m)
get = InterT State.get

put :: (Monad m) => SymbolTable m -> InterT m ()
put = InterT . State.put

modify :: (Monad m) => (SymbolTable m -> SymbolTable m) -> InterT m ()
modify = InterT . State.modify

union :: (Monad m) => SymbolTable m -> InterT m ()
union st2 = do
    st1 <- get
    st' <-
        sequence $
            Map.unionWithKey
                errorDuplicateSymbolBound
                (InterT . pure <$> st1)
                (InterT . pure <$> st2)
    put st'

insert :: (Monad m) => Text.Text -> Output m -> InterT m ()
insert k x = union =<< fromList [(k, x)]

unionReplace :: (Monad m) => SymbolTable m -> InterT m ()
unionReplace = modify . Map.union

insertReplace :: (Monad m) => Text.Text -> Output m -> InterT m ()
insertReplace k x = modify $ Map.insert k x

throwE :: (Monad m) => Text.Text -> InterT m a
throwE = InterT . lift . Except.throwE

catchE :: (Monad m) => InterT m a -> (Text.Text -> InterT m a) -> InterT m a
catchE x f = do
    res <- lift . runInterT x =<< get
    case res of
        Left e -> f e
        Right (a, st) -> put st $> a

runInterT :: InterT m a -> SymbolTable m -> m (Either Text.Text (a, SymbolTable m))
runInterT i st = Except.runExceptT . flip State.runStateT st . unInterT $ i

evalInterT :: (Monad m) => InterT m a -> SymbolTable m -> m (Either Text.Text a)
evalInterT i st = Except.runExceptT . flip State.evalStateT st . unInterT $ i

lookup :: (Monad m) => Text.Text -> InterT m (Output m)
lookup sym = do
    get >>= \st -> case sym `Map.lookup` st of
        Nothing -> throwE $ "Symbol does not exist: " <> sym
        Just x -> pure x
