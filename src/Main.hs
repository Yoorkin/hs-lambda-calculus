module Main (main) where

import Parser
import Lambda
import Onebot
import REPL
import Data.Map
import Control.Monad.State

main :: IO ()
main = do
    putStrLn "choose interactive mode REPL[y] OneBot[n]:"
    ans <- getLine
    case head ans of 
        'y' -> evalStateT repl (empty,empty)
        'n' -> onebot




