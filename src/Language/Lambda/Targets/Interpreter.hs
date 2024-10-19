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
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Language.Lambda.Parser as Parser
import qualified Language.Lambda.Targets.Interpreter.Core as I
import qualified Language.Lambda.Targets.Interpreter.Reduction as I
import qualified Language.Lambda.Targets.Interpreter.SymbolTable as I
import System.IO as SysIO
import Prelude hiding (div, lookup)

printError :: Text.Text -> IO ()
printError = putStrLn . unlines . fmap (">>> " <>) . lines . Text.unpack

prettyDebugSymbolTable :: I.InterT IO ()
prettyDebugSymbolTable = do
    st <- I.get
    forM_
        (Map.toList $ I.debugSymbolTable st)
        ( \(k, e) -> do
            e' <- I.myShow e
            liftIO . putStrLn $ ">>> " <> Text.unpack k <> ": " <> e'
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

runInterpreterSingle :: InterConfig -> Text.Text -> IO ()
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
    runFile conf filename . Text.pack =<< liftIO (readFile filename)
    loopInterpreter conf

runFile :: InterConfig -> String -> Text.Text -> I.InterT IO ()
runFile conf filename inp = do
    case Parser.parse Parser.lambdaFile filename inp of
        Left e -> I.throwE $ Text.pack $ Parser.errorBundlePretty e
        Right r -> do
            st <- forM r $ \case
                Parser.Assign x y -> pure (x, I.Const y)
                Parser.Effect x -> do
                    x' <- Text.pack <$> display conf x
                    I.throwE $ "Unexpected: " <> x'
            I.union =<< I.fromList st

loopInterpreter :: InterConfig -> I.InterT IO ()
loopInterpreter conf@(InterConfig{inputPrefix, inputPostfix}) =
    go
  where
    go = do
        let wrapInput m = putStr inputPrefix *> m <* putStr inputPostfix
        line <- liftIO . wrapInput $ Text.pack <$> getLine
        I.catchE (handleLine conf line) $ liftIO . printError
        go

handleLine :: InterConfig -> Text.Text -> I.InterT IO ()
handleLine conf s =
    case Parser.parse (Parser.skip *> Parser.lambdaLine <* Parser.eof) "lambda-interpreter" s of
        Left e -> do
            case Parser.parse (Parser.skip <* Parser.eof) "lambda-interpreter" s of
                Left _ -> I.throwE . Text.pack $ Parser.errorBundlePretty e
                Right _ -> pure ()
        Right x -> handleStatement conf x

handleStatement :: InterConfig -> Parser.Statement -> I.InterT IO ()
handleStatement c = \case
    Parser.Assign x y -> I.insertReplace x (I.Const y)
    Parser.Effect x ->
        I.whnf x >>= \case
            I.Builtin _ -> I.throwE "Unable to print built-in"
            I.Const e -> liftIO . putStr =<< display c e

display :: InterConfig -> Parser.Expr -> I.InterT IO String
display (InterConfig{returnPrefix, returnPostfix}) x =
    (\s -> returnPrefix <> s <> returnPostfix) <$> I.myShow x
