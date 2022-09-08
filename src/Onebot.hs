{-# LANGUAGE OverloadedStrings #-}
module Onebot(
    onebot
) where

import Parser
import Lambda
import Control.Monad.State
import Data.Map

import           Control.Monad       (forever, void)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import Data.List.Split(splitOn)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as Data.List.Utils



getInputFromJson :: Aeson.Value -> Maybe (String,Int)
getInputFromJson (Aeson.Object obj) = let
        msg_type = KeyMap.lookup "message_type" obj
        group_id = KeyMap.lookup "group_id" obj
        message = KeyMap.lookup "message" obj
      in case (msg_type,group_id,message) of
        (Just (Aeson.String "group"), Just (Aeson.Number id), Just (Aeson.String msg)) ->
            case T.unpack msg of
                ('>':code) -> Just (code,fromMaybe 0 (toBoundedInteger id))
                _ -> Nothing
        _ -> Nothing
getInputFromJson _ = Nothing

textToJson :: Text -> Maybe Aeson.Value
textToJson x = Aeson.decode (BLU.fromString $ T.unpack x)

reply :: String -> Int -> UTF8.ByteString
reply s i = UTF8.fromString ("{\"action\": \"send_msg\", \"params\": {\"group_id\":" ++
                    show i ++ ",\"message\":\"" ++ T.unpack (T.replace "\"" "'" (T.pack s)) ++ "\"}}")



app :: WS.Connection -> StateT Pairs IO ()
app conn = do text <- lift $ readFile "prelude.txt"
              let lines = splitOn "\n" text
              forM_ lines (\x -> do pairs <- get
                                    case parseLambda x of
                                        Right (Let n e) -> do put(insertPair n e pairs)
                                        _ -> return ()
                          )
              lift $ putStrLn text
              let loop = forever $ do
                    msg <- lift $ WS.receiveData conn
                    lift $ putStrLn $ T.unpack msg
                    case textToJson msg >>= getInputFromJson of
                        Just (code,id) ->  do
                            times <- get
                            case parseLambda code of
                                Left err -> lift $ WS.sendTextData conn $ reply (show err) id
                                Right x -> do
                                    pairs <- get
                                    let eval = simplify (fst pairs)
                                        fill = replace (fst pairs)
                                     in case x of
                                        Let n e -> do put (insertPair n e pairs)
                                        Dsp e -> do lift $ WS.sendTextData conn $ reply (show $ eval $ fill $ eval e) id
                                        Eval e -> do lift $ WS.sendTextData conn $ reply (case alias (eval e) pairs of
                                                        Just n -> n
                                                        Nothing -> show $ eval e) id
                        _ -> return ()
              loop

app' :: WS.Connection -> IO ()
app' conn = Control.Monad.void (execStateT (app conn) (empty,empty))
--------------------------------------------------------------------------------
onebot :: IO ()
onebot = withSocketsDo $ WS.runClient "127.0.0.1" 8080 "/" app'
