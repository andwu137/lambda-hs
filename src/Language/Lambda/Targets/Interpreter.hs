module Language.Lambda.Targets.Interpreter (
    InterConfig (..),
    runInterpreterSingle,
    runInterpreter,
    interpreter,
    runFile,
    loop,
) where

import Control.Monad (forM, void, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Lambda.Expr
import Language.Lambda.Targets.Interpreter.Core
import Language.Lambda.Targets.Interpreter.Reduction
import Language.Lambda.Targets.Interpreter.SymbolTable
import System.IO
import qualified Text.Megaparsec as P
import Prelude hiding (div, lookup)

printError :: T.Text -> IO ()
printError = putStrLn . T.unpack

data InterConfig = InterConfig {prefix :: String}
    deriving (Show, Eq)

runInterpreterSingle :: T.Text -> IO ()
runInterpreterSingle s = do
    res <- evalInterT (handleLine s) defaultSymbolTable
    either printError pure res

runInterpreter :: InterConfig -> String -> IO ()
runInterpreter conf filename = do
    res <- evalInterT (interpreter conf filename) defaultSymbolTable
    either printError pure res

interpreter :: InterConfig -> String -> InterT IO ()
interpreter conf filename = do
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ hSetBuffering stdin LineBuffering
    runFile filename . T.pack =<< liftIO (readFile filename)
    loop conf

runFile :: String -> T.Text -> InterT IO ()
runFile filename inp = do
    case P.parse lambdaFile filename inp of
        Left e -> throwE $ T.pack $ P.errorBundlePretty e
        Right r -> do
            st <- forM r $ \case
                Assign x y -> pure (x, Const y)
                Effect x -> do
                    x' <- display x
                    throwE $ "Unexpected: " <> x'
            modify (<> M.fromList st) -- TODO: Error on duplicates

loop :: InterConfig -> InterT IO ()
loop (InterConfig{prefix}) =
    go
  where
    go = do
        -- liftIO . print . debugSymbolTable =<< get
        liftIO $ putStr prefix
        liftIO (T.pack <$> getLine) >>= \line ->
            catchE (handleLine line) $ liftIO . printError
        go

handleLine :: T.Text -> InterT IO ()
handleLine s =
    case P.parse (lambdaLine <* P.eof) "lambda-interpreter" s of
        Left e -> throwE . T.pack $ P.errorBundlePretty e
        Right x -> handleStatement x

handleStatement :: Statement -> InterT IO ()
handleStatement = \case
    Assign x y -> modify (M.insert x (Const y))
    Effect x ->
        whnf x >>= \case
            Builtin _ -> throwE "Unable to print built-in"
            Const e -> liftIO . putStrLn . T.unpack =<< display e

display :: Expr -> InterT IO T.Text
display =
    eval >=> \case
        Bool b -> pure $ T.pack $ show b
        Unit -> pure $ T.pack $ show ()
        Z x -> pure $ T.pack $ show x
        R x -> pure $ T.pack $ show x
        String x -> pure $ T.pack x
        Abs{} -> throwE "Unable to print abstraction"
        e -> eval e >>= display
