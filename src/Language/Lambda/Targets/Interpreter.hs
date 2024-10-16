module Language.Lambda.Targets.Interpreter (
    InterConfig (..),
    runInterpreterSingle,
    runInterpreter,
    runloopInterpreter,
    runFile,
    fileInterpreter,
    loopInterpreter,
) where

import Control.Monad (forM, (>=>))
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

runInterpreterDefault :: InterT IO () -> IO ()
runInterpreterDefault i = do
    res <- evalInterT i defaultSymbolTable
    either printError pure res

runInterpreterSingle :: T.Text -> IO ()
runInterpreterSingle s = runInterpreterDefault $ handleLine s

runInterpreter :: InterConfig -> String -> IO ()
runInterpreter conf filename =
    runInterpreterDefault $ fileInterpreter conf filename

runloopInterpreter :: InterConfig -> IO ()
runloopInterpreter conf = runInterpreterDefault $ loopInterpreter conf

fileInterpreter :: InterConfig -> String -> InterT IO ()
fileInterpreter conf filename = do
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ hSetBuffering stdin LineBuffering
    runFile filename . T.pack =<< liftIO (readFile filename)
    loopInterpreter conf

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
            union =<< fromList st

loopInterpreter :: InterConfig -> InterT IO ()
loopInterpreter (InterConfig{prefix}) =
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
display x = T.pack <$> myShow x
