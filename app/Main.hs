module Main (main) where

import Language.Lambda.Targets.Interpreter (InterConfig (..), runInterpreter, runloopInterpreter)
import qualified System.Environment as SysEnv
import qualified System.IO as SysIO

conf :: InterConfig
conf =
    InterConfig
        { inputPrefix = "λ> "
        , returnPrefix = "β> "
        , inputPostfix = ""
        , returnPostfix = "\n\n"
        }

main :: IO ()
main = do
    SysIO.hSetBuffering SysIO.stdout SysIO.NoBuffering

    args <- SysEnv.getArgs
    case args of
        [] -> runloopInterpreter conf
        ["-i"] -> runloopInterpreter conf
        ["-i", filename] -> runInterpreter conf filename
        _ -> putStrLn "currently is garbage, only does '-i filename'"
