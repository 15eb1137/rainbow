{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( sampleProblem,
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

-- サンプル問題の定義
sampleProblem :: [Problem]
sampleProblem =
  [ Problem
      { communityCards =
          [ Card "♠" "K",
            Card "♥" "Q",
            Card "♦" "J",
            Card "♣" "T",
            Card "♠" "9"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "K"], -- プレイヤー1の手札
            [Card "♣" "K", Card "♠" "Q"] -- プレイヤー2の手札
          ],
        correctAnswer = 1 -- プレイヤー1が勝利
      },
    Problem
      { communityCards =
          [ Card "♠" "A",
            Card "♥" "A",
            Card "♦" "A",
            Card "♣" "K",
            Card "♥" "K"
          ],
        playerHands =
          [ [Card "♠" "K", Card "♦" "K"], -- プレイヤー1の手札
            [Card "♣" "A", Card "♠" "Q"] -- プレイヤー2の手札
          ],
        correctAnswer = 2 -- プレイヤー2が勝利
      }
  ]