module Language.Lambda.Targets.Interpreter (
    runInterpreterSingle,
    runInterpreter,
    interpreter,
    runFile,
    loop,
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Monad.Trans.State as S
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Lambda.Expr
import Language.Lambda.Targets.Interpreter.Core
import Language.Lambda.Targets.Interpreter.Reduction
import Language.Lambda.Targets.Interpreter.SymbolTable
import System.IO
import qualified Text.Megaparsec as P
import Prelude hiding (div, lookup)

runInterpreterSingle :: T.Text -> IO ()
runInterpreterSingle s = do
    res <- runInterT (handleLine s) defaultSymbolTable
    either print pure res

runInterpreter :: String -> IO ()
runInterpreter filename = do
    res <- runInterT (interpreter filename) defaultSymbolTable
    either print pure res

interpreter :: String -> InterT IO ()
interpreter filename = do
    runFile filename . T.pack =<< liftIO (readFile filename)
    loop

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
            InterT $ S.modify (<> M.fromList st)

loop :: InterT IO ()
loop = do
    liftIO $ hSetBuffering stdin LineBuffering
    -- liftIO . print . debugSymbolTable =<< get
    liftIO $ putStr ">>> "
    liftIO (T.pack <$> getLine) >>= handleLine
    loop

handleLine :: T.Text -> InterT IO ()
handleLine s =
    case P.parse (lambdaLine <* P.eof) "lambda-interpreter" s of
        Left e -> liftIO $ putStrLn $ P.errorBundlePretty e
        Right x -> handleStatement x

handleStatement :: Statement -> InterT IO ()
handleStatement = \case
    Assign x y -> InterT $ S.modify (M.insert x (Const y))
    Effect x ->
        eval x >>= \case
            Builtin _ -> liftIO $ putStrLn "Unable to print built-in"
            Const e -> liftIO . putStrLn . T.unpack =<< display e

display :: Expr -> InterT IO T.Text
display e =
    case e of
        Bool b -> pure $ T.pack $ show b
        Unit -> pure $ T.pack $ show ()
        Z x -> pure $ T.pack $ show x
        R x -> pure $ T.pack $ show x
        String x -> pure $ T.pack x
        Ident i -> do
            let unresolved = show $ Ident i
            out <-
                catchE (lookup i) $
                    pure . const (Const $ String unresolved)
            case out of
                Builtin _ -> pure $ T.concat ["Function(", i, ")"]
                Const c -> display c
        Abs x b -> do
            b' <- T.pack . show <$> evalNoBuiltin b
            pure $ T.concat ["(\\", x, " -> ", b', ")"]
        App f x ->
            eval (App f x) >>= \case
                Builtin _ -> pure "Unable to print builtin"
                Const c -> display c
        Op o x y -> do
            eval (Op o x y) >>= \case
                Builtin _ -> pure "Unable to print builtin"
                Const c -> display c
