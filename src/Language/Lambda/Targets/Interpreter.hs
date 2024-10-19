module Language.Lambda.Targets.Interpreter (
    InterConfig (..),
    runInterpreterSingle,
    runInterpreter,
    runloopInterpreter,
    runFile,
    fileInterpreter,
    loopInterpreter,
) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map as M
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

prettyDebugSymbolTable :: I.InterT IO ()
prettyDebugSymbolTable = do
    st <- I.get
    forM_
        (M.toList $ I.debugSymbolTable st)
        ( \(k, e) -> do
            e' <- I.myShow e
            liftIO . putStrLn $ ">>> " <> T.unpack k <> ": " <> e'
        )

data InterConfig
    = InterConfig
    { inputPrefix :: String
    , inputPostfix :: String
    , returnPrefix :: String
    , returnPostfix :: String
    }
    deriving (Show, Eq)

runInterpreterDefault :: I.InterT IO () -> IO ()
runInterpreterDefault i = do
    res <- I.evalInterT i I.defaultSymbolTable
    either printError pure res

runInterpreterSingle :: InterConfig -> T.Text -> IO ()
runInterpreterSingle conf s = runInterpreterDefault $ handleLine conf s

runInterpreter :: InterConfig -> String -> IO ()
runInterpreter conf filename =
    runInterpreterDefault $ fileInterpreter conf filename

runloopInterpreter :: InterConfig -> IO ()
runloopInterpreter conf = runInterpreterDefault $ loopInterpreter conf

fileInterpreter :: InterConfig -> String -> I.InterT IO ()
fileInterpreter conf filename = do
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ hSetBuffering stdin LineBuffering
    runFile conf filename . T.pack =<< liftIO (readFile filename)
    loopInterpreter conf

runFile :: InterConfig -> String -> T.Text -> I.InterT IO ()
runFile conf filename inp = do
    case P.parse E.lambdaFile filename inp of
        Left e -> I.throwE $ T.pack $ P.errorBundlePretty e
        Right r -> do
            st <- forM r $ \case
                E.Assign x y -> pure (x, I.Const y)
                E.Effect x -> do
                    x' <- T.pack <$> display conf x
                    I.throwE $ "Unexpected: " <> x'
            I.union =<< I.fromList st

loopInterpreter :: InterConfig -> I.InterT IO ()
loopInterpreter conf@(InterConfig{inputPrefix, inputPostfix}) =
    go
  where
    go = do
        let wrapInput m = putStr inputPrefix *> m <* putStr inputPostfix
        line <- liftIO . wrapInput $ T.pack <$> getLine
        I.catchE (handleLine conf line) $ liftIO . printError
        go

handleLine :: InterConfig -> T.Text -> I.InterT IO ()
handleLine conf s =
    case P.parse (E.lambdaLine <* P.eof) "lambda-interpreter" s of
        Left e -> I.throwE . T.pack $ P.errorBundlePretty e
        Right x -> handleStatement conf x

handleStatement :: InterConfig -> E.Statement -> I.InterT IO ()
handleStatement c = \case
    E.Assign x y -> I.insertReplace x (I.Const y)
    E.Effect x ->
        I.whnf x >>= \case
            I.Builtin _ -> I.throwE "Unable to print built-in"
            I.Const e -> liftIO . putStr =<< display c e

display :: InterConfig -> E.Expr -> I.InterT IO String
display (InterConfig{returnPrefix, returnPostfix}) x =
    (\s -> returnPrefix <> s <> returnPostfix) <$> I.myShow x
