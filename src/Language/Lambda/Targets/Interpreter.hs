module Language.Lambda.Targets.Interpreter (
    I.InterConfig (..),
    I.InterT (..),
    runInterpreterSingle,
    runloopInterpreter,
    runFileInterpreterMain,
    runFileInterpreterLoop,
    loadFile,
    fileInterpreter,
    loopInterpreter,
) where

import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.Lambda.Parser as Parser
import qualified Language.Lambda.Targets.Interpreter.Core as I
import qualified Language.Lambda.Targets.Interpreter.Reduction as I
import qualified Language.Lambda.Targets.Interpreter.SymbolTable as I
import System.IO as SysIO
import Prelude hiding (div, lookup)

printError :: Text.Text -> Text.Text -> IO ()
printError errPrefix =
    Text.putStrLn
        . Text.unlines
        . fmap (errPrefix <>)
        . Text.lines

prettyDebugSymbolTable :: I.InterT IO ()
prettyDebugSymbolTable = do
    st <- I.get
    forM_
        (Map.toList $ I.debugSymbolTable st)
        ( \(k, e) -> do
            e' <- I.show' e
            liftIO . Text.putStrLn $ ">>> " <> k <> ": " <> e'
        )

runInterpreterDefault :: I.InterConfig -> I.InterT IO () -> IO ()
runInterpreterDefault conf i = do
    res <- I.evalInterT i conf I.defaultSymbolTable
    either (printError $ I.errorPrefix conf) pure res

runInterpreterSingle :: I.InterConfig -> Text.Text -> IO ()
runInterpreterSingle conf s =
    runInterpreterDefault conf $
        handleLine s

runFileInterpreterMain :: I.InterConfig -> String -> IO ()
runFileInterpreterMain conf filename =
    runInterpreterDefault conf $ do
        fileInterpreter filename
        mainInterpreter

runFileInterpreterLoop :: I.InterConfig -> String -> IO ()
runFileInterpreterLoop conf filename =
    runInterpreterDefault conf $ do
        fileInterpreter filename
        loopInterpreter

runloopInterpreter :: I.InterConfig -> IO ()
runloopInterpreter conf = runInterpreterDefault conf loopInterpreter

mainInterpreter :: I.InterT IO ()
mainInterpreter = do
    (I.InterConfig{moduleMain}) <- I.ask
    I.lookup moduleMain >>= \case
        I.Builtin _ ->
            liftIO $ Text.putStrLn "main resolved to a builtin function"
        I.Const e -> void $ handleExpr e

fileInterpreter :: String -> I.InterT IO ()
fileInterpreter filename = do
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ hSetBuffering stdin LineBuffering
    loadFile filename

loadFile :: String -> I.InterT IO ()
loadFile filename =
    loadFileSymbols filename
        . Text.pack
        =<< liftIO (readFile filename)

loadFileSymbols :: String -> Text.Text -> I.InterT IO ()
loadFileSymbols filename inp = do
    case Parser.parse Parser.lambdaFile filename inp of
        Left e -> I.throwE $ Text.pack $ Parser.errorBundlePretty e
        Right r -> do
            st <- forM r $ \(sp, s) -> case s of
                Parser.Assign x y -> pure (x, I.Const y)
                Parser.Effect x -> do
                    x' <- display x
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
        ( I.InterConfig
                { inputPrefix
                , inputPostfix
                , errorPrefix
                }
            ) <-
            I.ask
        let wrapInput m = Text.putStr inputPrefix *> m <* Text.putStr inputPostfix
        line <- liftIO . wrapInput $ Text.pack <$> getLine
        I.catchE (handleLine line) $ liftIO . printError errorPrefix
        go

handleLine :: Text.Text -> I.InterT IO ()
handleLine s = do
    (I.InterConfig{replName}) <- I.ask
    let rname = Text.unpack replName
    case Parser.parse (Parser.lambdaLine <* Parser.eof) rname s of
        Left e -> do
            case Parser.parse (Parser.skip <* Parser.eof) rname s of
                Left _ -> I.throwE . Text.pack $ Parser.errorBundlePretty e
                Right _ -> pure ()
        Right x -> handleStatement x

handleStatement :: Parser.Statement -> I.InterT IO ()
handleStatement = \case
    Parser.Assign x y -> I.insertReplace x (I.Const y)
    Parser.Effect x -> do
        r <- handleExpr x
        liftIO . Text.putStr =<< display r

handleExpr :: Parser.Expr -> I.InterT IO Parser.Expr
handleExpr x =
    I.whnf x >>= \case
        I.Builtin _ -> I.throwE "unable to print built-in"
        I.Const e -> pure e

display :: Parser.Expr -> I.InterT IO Text.Text
display x = do
    (I.InterConfig{returnPrefix, returnPostfix}) <- I.ask
    (\s -> returnPrefix <> s <> returnPostfix) <$> I.show' x
