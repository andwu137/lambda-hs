{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import qualified Language.Lambda.Targets.Interpreter as I
import qualified System.Environment as SysEnv
import qualified System.IO as SysIO

loopConf :: I.InterConfig
loopConf =
    I.InterConfig
        { I.replName = "lambda-interpreter"
        , I.inputPrefix = "λ> "
        , I.returnPrefix = "β> "
        , I.inputPostfix = ""
        , I.returnPostfix = "\n\n"
        , I.errorPrefix = "Error> "
        , I.moduleMain = "module_main"
        }

singleConf :: I.InterConfig
singleConf =
    I.InterConfig
        { I.replName = "lambda-interpreter"
        , I.inputPrefix = ""
        , I.returnPrefix = ""
        , I.inputPostfix = ""
        , I.returnPostfix = ""
        , I.errorPrefix = "Error: "
        , I.moduleMain = "module_main"
        }

main :: IO ()
main = do
    SysIO.hSetBuffering SysIO.stdout SysIO.NoBuffering

    args <- SysEnv.getArgs
    case args of
        [] -> I.runloopInterpreter loopConf
        ["-i"] -> I.runloopInterpreter loopConf
        ["-i", filename] -> I.runFileInterpreterLoop loopConf filename
        [filename] -> I.runFileInterpreterMain singleConf filename
        _ -> putStrLn "currently is garbage, only does '-i filename'"
