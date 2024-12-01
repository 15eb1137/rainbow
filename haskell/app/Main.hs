{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import SampleProblems (shuffleSampleProblems)
import Web.Scotty
import Network.HTTP.Types.Status (status200)
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header (hContentType)
import Control.Monad.IO.Class (MonadIO(liftIO))

main :: IO ()
main = do
    putStrLn "Starting server on port 3000..."
    scotty 3000 $ do
        -- CORSミドルウェアの設定
        middleware $ cors $ const $ Just simpleCorsResourcePolicy
            { corsRequestHeaders = [hContentType]
            , corsMethods = [methodGet, methodOptions]
            }

        -- GETエンドポイント
        get (literal "/problem") $ do
            status status200
            problems <- liftIO shuffleSampleProblems
            json problems

        -- ヘルスチェック用エンドポイント
        get (literal "/health") $ do
            text "OK"  -- 明示的にText型に変換