module Language.Lambda.Targets.Interpreter (
    InterConfig (..),
    runInterpreterSingle,
    runInterpreter,
    runloopInterpreter,
    runFile,
    fileInterpreter,
    loopInterpreter,
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as T
import qualified Language.Lambda.Expr as E
import qualified Language.Lambda.Targets.Interpreter.Core as I
import qualified Language.Lambda.Targets.Interpreter.Reduction as I
import qualified Language.Lambda.Targets.Interpreter.SymbolTable as I
import System.IO
import qualified Text.Megaparsec as P
import Prelude hiding (div, lookup)

printError :: T.Text -> IO ()
printError = putStrLn . unlines . fmap (">>> " <>) . lines . T.unpack

data InterConfig = InterConfig {prefix :: String}
    deriving (Show, Eq)

runInterpreterDefault :: I.InterT IO () -> IO ()
runInterpreterDefault i = do
    res <- I.evalInterT i I.defaultSymbolTable
    either printError pure res

runInterpreterSingle :: T.Text -> IO ()
runInterpreterSingle s = runInterpreterDefault $ handleLine s

runInterpreter :: InterConfig -> String -> IO ()
runInterpreter conf filename =
    runInterpreterDefault $ fileInterpreter conf filename

runloopInterpreter :: InterConfig -> IO ()
runloopInterpreter conf = runInterpreterDefault $ loopInterpreter conf

fileInterpreter :: InterConfig -> String -> I.InterT IO ()
fileInterpreter conf filename = do
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ hSetBuffering stdin LineBuffering
    runFile filename . T.pack =<< liftIO (readFile filename)
    loopInterpreter conf

runFile :: String -> T.Text -> I.InterT IO ()
runFile filename inp = do
    case P.parse E.lambdaFile filename inp of
        Left e -> I.throwE $ T.pack $ P.errorBundlePretty e
        Right r -> do
            st <- forM r $ \case
                E.Assign x y -> pure (x, I.Const y)
                E.Effect x -> do
                    x' <- display x
                    I.throwE $ "Unexpected: " <> x'
            I.union =<< I.fromList st

loopInterpreter :: InterConfig -> I.InterT IO ()
loopInterpreter (InterConfig{prefix}) =
    go
  where
    go = do
        liftIO $ putStr prefix
        liftIO (T.pack <$> getLine) >>= \line ->
            I.catchE (handleLine line) $ liftIO . printError
        go

handleLine :: T.Text -> I.InterT IO ()
handleLine s =
    case P.parse (E.lambdaLine <* P.eof) "lambda-interpreter" s of
        Left e -> I.throwE . T.pack $ P.errorBundlePretty e
        Right x -> handleStatement x

handleStatement :: E.Statement -> I.InterT IO ()
handleStatement = \case
    E.Assign x y -> I.insertReplace x (I.Const y)
    E.Effect x ->
        I.whnf x >>= \case
            I.Builtin _ -> I.throwE "Unable to print built-in"
            I.Const e -> liftIO . putStrLn . T.unpack =<< display e

display :: E.Expr -> I.InterT IO T.Text
display x = T.pack <$> I.myShow x
