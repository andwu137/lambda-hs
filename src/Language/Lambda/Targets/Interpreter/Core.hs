module Language.Lambda.Targets.Interpreter.Core (
    SymbolTable (..),
    InterConfig (..),
    Output (..),
    InterT (..),
    runInterT,
    evalInterT,
    get,
    gets,
    put,
    modify,
    ask,
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
import qualified Control.Monad.Trans.RWS.Strict as RWST
import Data.Functor (($>), (<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Language.Lambda.Parser as Parser
import Prelude hiding (lookup)

{- SymbolTable -}
newtype SymbolTable m
    = SymbolTable
    { getSymbolTable :: Map.Map Text.Text (Output m)
    }

fromList :: (Monad m) => [(Text.Text, Output m)] -> InterT m (SymbolTable m)
fromList st =
    SymbolTable
        <$> sequence
            ( Map.fromListWithKey
                errorDuplicateSymbolBound
                (fmap (InterT . pure) <$> st)
            )

errorDuplicateSymbolBound :: (Monad m) => Text.Text -> p1 -> p2 -> InterT m a
errorDuplicateSymbolBound k _ _ = throwE $ "unexpected duplicate symbol bound: " <> k

data Output m
    = Builtin !(Parser.Expr -> InterT m (Output m))
    | Const !Parser.Expr

debugSymbolTable :: SymbolTable m -> Map.Map Text.Text Parser.Expr
debugSymbolTable (SymbolTable st) =
    st <&> \case
        Builtin _ -> Parser.String "built-in"
        Const c -> c

data InterConfig
    = InterConfig
    { replName :: !String
    , inputPrefix :: !String
    , inputPostfix :: !String
    , returnPrefix :: !String
    , returnPostfix :: !String
    }
    deriving (Show, Eq)

{- Inter -}
newtype InterT m a
    = InterT
    { unInterT ::
        RWST.RWST
            InterConfig
            ()
            (SymbolTable m)
            (Except.ExceptT Text.Text m)
            a
    }
    deriving (Functor, Applicative, Monad, Alternative, MonadIO)

instance MonadTrans InterT where
    lift = InterT . lift . lift

gets :: (Monad m) => (SymbolTable m -> b) -> InterT m b
gets f = f <$> get

get :: (Monad m) => InterT m (SymbolTable m)
get = InterT RWST.get

put :: (Monad m) => SymbolTable m -> InterT m ()
put = InterT . RWST.put

modify :: (Monad m) => (SymbolTable m -> SymbolTable m) -> InterT m ()
modify = InterT . RWST.modify

ask :: (Monad m) => InterT m InterConfig
ask = InterT RWST.ask

union :: (Monad m) => SymbolTable m -> InterT m ()
union (SymbolTable st2) = do
    st1 <- gets getSymbolTable
    st' <-
        sequence $
            Map.unionWithKey
                errorDuplicateSymbolBound
                (InterT . pure <$> st1)
                (InterT . pure <$> st2)
    put (SymbolTable st')

insert :: (Monad m) => Text.Text -> Output m -> InterT m ()
insert k x = union =<< fromList [(k, x)]

unionReplace :: (Monad m) => SymbolTable m -> InterT m ()
unionReplace (SymbolTable st) =
    modify $ SymbolTable . Map.union st . getSymbolTable

insertReplace :: (Monad m) => Text.Text -> Output m -> InterT m ()
insertReplace k x =
    modify $ SymbolTable . Map.insert k x . getSymbolTable

throwE :: (Monad m) => Text.Text -> InterT m a
throwE = InterT . lift . Except.throwE

catchE :: (Monad m) => InterT m a -> (Text.Text -> InterT m a) -> InterT m a
catchE x f = do
    conf <- ask
    st <- get
    res <- lift $ runInterT x conf st
    case res of
        Left e -> f e
        Right (a, st') -> put st' $> a

runInterT ::
    (Monad m) =>
    InterT m a ->
    InterConfig ->
    SymbolTable m ->
    m (Either Text.Text (a, SymbolTable m))
runInterT i conf st =
    Except.runExceptT $ dropWriter <$> RWST.runRWST (unInterT i) conf st
  where
    dropWriter (r, s, _) = (r, s)

evalInterT ::
    (Monad m) =>
    InterT m a ->
    InterConfig ->
    SymbolTable m ->
    m (Either Text.Text a)
evalInterT i conf st =
    Except.runExceptT $ dropWriter <$> RWST.evalRWST (unInterT i) conf st
  where
    dropWriter (s, _) = s

lookup :: (Monad m) => Text.Text -> InterT m (Output m)
lookup sym = do
    gets (Map.lookup sym . getSymbolTable) >>= \case
        Nothing -> throwE $ "symbol does not exist: " <> sym
        Just x -> pure x
