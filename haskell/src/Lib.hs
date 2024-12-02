{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( Problem (..),
    Card (..),
  )
where

import Data.Aeson (ToJSON)
import GHC.Generics

-- データ型の定義
data Card = Card
  { suit :: String,
    rank :: String
  }
  deriving (Generic, Show)

data Problem = Problem
  { communityCards :: [Card],
    playerHands :: [([Card])],
    correctAnswer :: Int
  }
  deriving (Generic, Show)

-- JSONシリアライズのためのインスタンス
instance ToJSON Card

instance ToJSON Problem
