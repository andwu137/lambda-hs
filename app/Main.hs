module Main (main) where

import Language.Lambda.Targets.Interpreter (InterConfig (..), runInterpreter, runloopInterpreter)
import qualified System.Environment as SE
import System.IO

conf :: InterConfig
conf =
    InterConfig
        { prefix = "λ> "
        }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    args <- SE.getArgs
    case args of
        [] -> runloopInterpreter conf
        ["-i"] -> runloopInterpreter conf
        ["-i", filename] -> runInterpreter conf filename
        _ -> putStrLn "currently is garbage, only does '-i filename'"
