module REPL(
    repl
) where

import Parser
import Lambda
import Data.Map as Map
import Control.Monad.State

execute :: String -> StateT Pairs IO ()
execute text = do
    case parseLambda text of
        Left err -> lift $ print err
        Right x -> do
            pairs <- get
            let eval = simplify (fst pairs)
                fill = replace (fst pairs)
             in case x of
                Let n e -> do put (insertPair n e pairs)
                Dsp e -> lift $ print $ eval $ fill $ eval e
                Eval e -> lift $ putStrLn (case alias (eval e) pairs of 
                                                            Just n -> n 
                                                            Nothing -> show $ eval e)

repl :: StateT Pairs IO ()
repl = do
    text <- lift getLine
    execute text
    repl