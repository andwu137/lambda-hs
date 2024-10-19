module Main (main) where

import qualified Language.Lambda.Targets.Interpreter as I
import qualified System.Environment as SysEnv
import qualified System.IO as SysIO

conf :: I.InterConfig
conf =
    I.InterConfig
        { I.replName = "lambda-interpreter"
        , I.inputPrefix = "λ> "
        , I.returnPrefix = "β> "
        , I.inputPostfix = ""
        , I.returnPostfix = "\n\n"
        }

main :: IO ()
main = do
    SysIO.hSetBuffering SysIO.stdout SysIO.NoBuffering

    args <- SysEnv.getArgs
    case args of
        [] -> I.runloopInterpreter conf
        ["-i"] -> I.runloopInterpreter conf
        ["-i", filename] -> I.runInterpreter conf filename
        _ -> putStrLn "currently is garbage, only does '-i filename'"
