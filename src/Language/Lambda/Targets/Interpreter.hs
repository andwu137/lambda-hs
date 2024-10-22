module Language.Lambda.Targets.Interpreter (
    I.InterConfig (..),
    I.InterT (..),
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
            e' <- I.show' e
            liftIO . putStrLn $ ">>> " <> Text.unpack k <> ": " <> e'
        )

runInterpreterDefault :: I.InterConfig -> I.InterT IO () -> IO ()
runInterpreterDefault conf i = do
    res <- I.evalInterT i conf I.defaultSymbolTable
    either printError pure res

runInterpreterSingle :: I.InterConfig -> Text.Text -> IO ()
runInterpreterSingle conf s = runInterpreterDefault conf $ handleLine s

runInterpreter :: I.InterConfig -> String -> IO ()
runInterpreter conf filename =
    runInterpreterDefault conf $ fileInterpreter filename

runloopInterpreter :: I.InterConfig -> IO ()
runloopInterpreter conf = runInterpreterDefault conf loopInterpreter

fileInterpreter :: String -> I.InterT IO ()
fileInterpreter filename = do
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ hSetBuffering stdin LineBuffering
    runFile filename . Text.pack =<< liftIO (readFile filename)
    loopInterpreter

runFile :: String -> Text.Text -> I.InterT IO ()
runFile filename inp = do
    case Parser.parse Parser.lambdaFile filename inp of
        Left e -> I.throwE $ Text.pack $ Parser.errorBundlePretty e
        Right r -> do
            st <- forM r $ \(sp, s) -> case s of
                Parser.Assign x y -> pure (x, I.Const y)
                Parser.Effect x -> do
                    x' <- Text.pack <$> display x
                    I.throwE $ err sp <> "unexpected: " <> x'
            I.union =<< I.fromList st
  where
    err (Parser.SourcePos{..}) =
        Text.unlines
            [ Text.pack sourceName
            , Text.pack $
                (show sourceLine <> ":")
                    <> (show sourceColumn <> ":")
            ]

loopInterpreter :: I.InterT IO ()
loopInterpreter =
    go
  where
    go = do
        (I.InterConfig{inputPrefix, inputPostfix}) <- I.ask
        let wrapInput m = putStr inputPrefix *> m <* putStr inputPostfix
        line <- liftIO . wrapInput $ Text.pack <$> getLine
        I.catchE (handleLine line) $ liftIO . printError
        go

handleLine :: Text.Text -> I.InterT IO ()
handleLine s = do
    (I.InterConfig{replName}) <- I.ask
    case Parser.parse (Parser.skip *> Parser.lambdaLine <* Parser.eof) replName s of
        Left e -> do
            case Parser.parse (Parser.skip <* Parser.eof) replName s of
                Left _ -> I.throwE . Text.pack $ Parser.errorBundlePretty e
                Right _ -> pure ()
        Right x -> handleStatement x

handleStatement :: Parser.Statement -> I.InterT IO ()
handleStatement = \case
    Parser.Assign x y -> I.insertReplace x (I.Const y)
    Parser.Effect x ->
        I.whnf x >>= \case
            I.Builtin _ -> I.throwE "unable to print built-in"
            I.Const e -> liftIO . putStr =<< display e

display :: Parser.Expr -> I.InterT IO String
display x = do
    (I.InterConfig{returnPrefix, returnPostfix}) <- I.ask
    (\s -> returnPrefix <> s <> returnPostfix) <$> I.show' x
