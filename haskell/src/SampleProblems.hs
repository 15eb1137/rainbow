module SampleProblems
  ( shuffleSampleProblems,
  )
where

import Lib
import System.Random
import Data.List (sortBy)
import Control.Monad (replicateM)

shuffleSampleProblems :: IO [Problem]
shuffleSampleProblems = do
    let len = length sampleProblems
    indices <- replicateM len (randomRIO (0, maxBound :: Int))
    return $ map snd $ sortBy (\(a,_) (b,_) -> compare a b) $ zip indices sampleProblems

-- サンプル問題の定義
sampleProblems :: [Problem]
sampleProblems =
  [ Problem
      { communityCards =
          [ Card "♠" "K",
            Card "♥" "Q",
            Card "♦" "J",
            Card "♣" "T",
            Card "♠" "9"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "K"],
            [Card "♣" "K", Card "♠" "Q"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（エースハイストレート vs キングハイストレート）
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
          [ [Card "♠" "K", Card "♦" "K"],
            [Card "♣" "A", Card "♠" "Q"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（キングフォーカードvsエースハイフォーカード）
      },
    Problem
      { communityCards =
          [ Card "♥" "8",
            Card "♥" "6",
            Card "♥" "4",
            Card "♣" "3",
            Card "♦" "2"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♥" "K"],
            [Card "♠" "5", Card "♣" "4"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（フラッシュvsストレート）
      },
    Problem
      { communityCards =
          [ Card "♠" "J",
            Card "♥" "J",
            Card "♦" "7",
            Card "♣" "7",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♦" "J", Card "♥" "7"],
            [Card "♣" "J", Card "♠" "K"]
          ],
        correctAnswer = 3 -- チョップ（JJJ77）
      },
    Problem
      { communityCards =
          [ Card "♠" "Q",
            Card "♥" "Q",
            Card "♦" "8",
            Card "♣" "8",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♦" "K", Card "♥" "2"],
            [Card "♣" "A", Card "♠" "4"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（エースハイ）
      },
    Problem
      { communityCards =
          [ Card "♦" "9",
            Card "♦" "8",
            Card "♦" "7",
            Card "♦" "6",
            Card "♥" "9"
          ],
        playerHands =
          [ [Card "♦" "T", Card "♦" "5"],
            [Card "♠" "9", Card "♣" "9"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（ストレートフラッシュ）
      },
    Problem
      { communityCards =
          [ Card "♥" "A",
            Card "♥" "K",
            Card "♥" "Q",
            Card "♥" "J",
            Card "♦" "T"
          ],
        playerHands =
          [ [Card "♥" "T", Card "♣" "2"],
            [Card "♠" "9", Card "♠" "8"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（ロイヤルフラッシュ）
      },
    Problem
      { communityCards =
          [ Card "♠" "T",
            Card "♥" "T",
            Card "♦" "T",
            Card "♣" "5",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♦" "K", Card "♥" "3"],
            [Card "♣" "A", Card "♠" "4"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♣" "8",
            Card "♣" "6",
            Card "♣" "4",
            Card "♣" "2",
            Card "♥" "8"
          ],
        playerHands =
          [ [Card "♦" "8", Card "♠" "8"],
            [Card "♣" "A", Card "♣" "K"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（８のフォーカード）
      },
    Problem
      { communityCards =
          [ Card "♠" "9",
            Card "♥" "8",
            Card "♦" "7",
            Card "♣" "6",
            Card "♠" "5"
          ],
        playerHands =
          [ [Card "♦" "T", Card "♥" "4"],
            [Card "♣" "4", Card "♠" "3"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（10ハイストレート）
      },
    Problem
      { communityCards =
          [ Card "♠" "K",
            Card "♥" "T",
            Card "♦" "7",
            Card "♣" "4",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♦" "K", Card "♥" "3"],
            [Card "♣" "A", Card "♠" "Q"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（キングペア）
      },
    Problem
      { communityCards =
          [ Card "♠" "Q",
            Card "♥" "Q",
            Card "♦" "Q",
            Card "♣" "Q",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♦" "K", Card "♥" "2"],
            [Card "♣" "A", Card "♠" "4"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♥" "J",
            Card "♥" "9",
            Card "♥" "6",
            Card "♥" "4",
            Card "♦" "2"
          ],
        playerHands =
          [ [Card "♥" "K", Card "♥" "3"],
            [Card "♥" "A", Card "♥" "2"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（エースハイフラッシュ）
      },
    Problem
      { communityCards =
          [ Card "♠" "8",
            Card "♥" "7",
            Card "♦" "6",
            Card "♣" "5",
            Card "♠" "K"
          ],
        playerHands =
          [ [Card "♦" "9", Card "♥" "4"],
            [Card "♣" "K", Card "♠" "8"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（9ハイストレート）
      },
    Problem
      { communityCards =
          [ Card "♠" "Q",
            Card "♥" "Q",
            Card "♦" "7",
            Card "♣" "7",
            Card "♠" "7"
          ],
        playerHands =
          [ [Card "♦" "A", Card "♥" "A"],
            [Card "♣" "K", Card "♠" "K"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（エースフルハウス）
      },
    Problem
      { communityCards =
          [ Card "♠" "T",
            Card "♥" "T",
            Card "♦" "5",
            Card "♣" "5",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♦" "T", Card "♥" "2"],
            [Card "♣" "A", Card "♠" "A"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（フルハウス）
      },
    Problem
      { communityCards =
          [ Card "♦" "8",
            Card "♦" "7",
            Card "♦" "6",
            Card "♦" "5",
            Card "♥" "4"
          ],
        playerHands =
          [ [Card "♦" "9", Card "♦" "4"],
            [Card "♦" "T", Card "♦" "3"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（9ハイストレートフラッシュ）
      },
    Problem
      { communityCards =
          [ Card "♠" "A",
            Card "♥" "A",
            Card "♦" "K",
            Card "♣" "K",
            Card "♠" "Q"
          ],
        playerHands =
          [ [Card "♦" "Q", Card "♥" "J"],
            [Card "♣" "Q", Card "♠" "K"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（キングフルハウス）
      },
    Problem
      { communityCards =
          [ Card "♠" "J",
            Card "♥" "J",
            Card "♦" "J",
            Card "♣" "J",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♦" "K", Card "♥" "2"],
            [Card "♣" "A", Card "♠" "4"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "Q",
            Card "♥" "J",
            Card "♦" "T",
            Card "♣" "9",
            Card "♠" "8"
          ],
        playerHands =
          [ [Card "♦" "K", Card "♥" "7"],
            [Card "♣" "A", Card "♠" "K"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（エースハイストレート）
      },
    Problem
      { communityCards =
          [ Card "♠" "T",
            Card "♥" "T",
            Card "♦" "3",
            Card "♣" "4",
            Card "♠" "5"
          ],
        playerHands =
          [ [Card "♦" "A", Card "♥" "K"],
            [Card "♣" "A", Card "♠" "Q"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（エース・キングキッカー）
      },
    Problem
      { communityCards =
          [ Card "♥" "A",
            Card "♥" "K",
            Card "♥" "Q",
            Card "♥" "J",
            Card "♦" "T"
          ],
        playerHands =
          [ [Card "♥" "9", Card "♣" "8"],
            [Card "♠" "T", Card "♦" "9"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（エースハイフラッシュ）
      },
    Problem
      { communityCards =
          [ Card "♠" "A",
            Card "♥" "K",
            Card "♦" "K",
            Card "♣" "2",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♦" "A", Card "♥" "Q"],
            [Card "♣" "A", Card "♠" "J"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（2ペア + クイーンキッカー）
      },
    Problem
      { communityCards =
          [ Card "♦" "9",
            Card "♦" "7",
            Card "♦" "5",
            Card "♦" "3",
            Card "♥" "K"
          ],
        playerHands =
          [ [Card "♣" "8", Card "♠" "6"],
            [Card "♦" "A", Card "♦" "2"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（エースハイフラッシュ）
      },
    Problem
      { communityCards =
          [ Card "♠" "9",
            Card "♥" "9",
            Card "♦" "9",
            Card "♣" "K",
            Card "♠" "K"
          ],
        playerHands =
          [ [Card "♦" "K", Card "♥" "A"],
            [Card "♣" "Q", Card "♠" "Q"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（キングスフルハウス）
      },
    Problem
      { communityCards =
          [ Card "♠" "5",
            Card "♥" "4",
            Card "♦" "3",
            Card "♣" "2",
            Card "♠" "K"
          ],
        playerHands =
          [ [Card "♦" "A", Card "♥" "6"],
            [Card "♣" "A", Card "♠" "7"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（6ハイストレート）
      },
    Problem
      { communityCards =
          [ Card "♥" "T",
            Card "♥" "8",
            Card "♥" "6",
            Card "♥" "4",
            Card "♦" "K"
          ],
        playerHands =
          [ [Card "♦" "K", Card "♠" "K"],
            [Card "♥" "A", Card "♥" "2"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（エースハイフラッシュ）
      },
    Problem
      { communityCards =
          [ Card "♠" "Q",
            Card "♥" "Q",
            Card "♦" "J",
            Card "♣" "J",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♦" "K", Card "♥" "2"],
            [Card "♣" "A", Card "♠" "4"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "K",
            Card "♥" "K",
            Card "♦" "7",
            Card "♣" "5",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♦" "A", Card "♥" "Q"],
            [Card "♣" "A", Card "♠" "J"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（クイーンキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "J",
            Card "♥" "9",
            Card "♦" "7",
            Card "♣" "5",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♦" "A", Card "♥" "K"],
            [Card "♣" "A", Card "♠" "Q"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（キングキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "8",
            Card "♥" "8",
            Card "♦" "8",
            Card "♣" "K",
            Card "♠" "K"
          ],
        playerHands =
          [ [Card "♦" "8", Card "♥" "2"],
            [Card "♣" "K", Card "♠" "A"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（エイトのフォーカード）
      },
      Problem
      { communityCards =
          [ Card "♠" "8",
            Card "♥" "6",
            Card "♦" "4",
            Card "♣" "3",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "K"],
            [Card "♣" "A", Card "♠" "Q"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（エース・キングハイ）
      },
    Problem
      { communityCards =
          [ Card "♠" "J",
            Card "♥" "9",
            Card "♦" "7",
            Card "♣" "4",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♥" "K", Card "♦" "5"],
            [Card "♣" "K", Card "♠" "6"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（キングハイ、6キッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "T",
            Card "♥" "8",
            Card "♦" "6",
            Card "♣" "4",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♥" "Q", Card "♦" "J"],
            [Card "♣" "K", Card "♠" "2"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（キングハイ）
      },
    Problem
      { communityCards =
          [ Card "♠" "K",
            Card "♥" "7",
            Card "♦" "5",
            Card "♣" "4",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♥" "T", Card "♦" "8"],
            [Card "♣" "J", Card "♠" "9"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（キング・ジャックハイ）
      },
    Problem
      { communityCards =
          [ Card "♠" "Q",
            Card "♥" "J",
            Card "♦" "8",
            Card "♣" "5",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "2"],
            [Card "♣" "K", Card "♠" "4"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（エースハイ）
      },
    Problem
      { communityCards =
          [ Card "♠" "K",
            Card "♥" "K",
            Card "♦" "6",
            Card "♣" "4",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♥" "T", Card "♦" "8"],
            [Card "♣" "Q", Card "♠" "J"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（キングペア、クイーンキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "Q",
            Card "♥" "Q",
            Card "♦" "7",
            Card "♣" "5",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♥" "J", Card "♦" "T"],
            [Card "♣" "K", Card "♠" "8"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（クイーンペア、キングキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "J",
            Card "♥" "J",
            Card "♦" "8",
            Card "♣" "6",
            Card "♠" "4"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "3"],
            [Card "♣" "K", Card "♠" "2"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（ジャックペア、エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "T",
            Card "♥" "T",
            Card "♦" "9",
            Card "♣" "7",
            Card "♠" "5"
          ],
        playerHands =
          [ [Card "♥" "Q", Card "♦" "4"],
            [Card "♣" "K", Card "♠" "3"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（テンペア、キングキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "9",
            Card "♥" "9",
            Card "♦" "7",
            Card "♣" "5",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "4"],
            [Card "♣" "K", Card "♠" "6"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（ナインペア、エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "K",
            Card "♥" "K",
            Card "♦" "Q",
            Card "♣" "Q",
            Card "♠" "4"
          ],
        playerHands =
          [ [Card "♥" "J", Card "♦" "T"],
            [Card "♣" "A", Card "♠" "2"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（ツーペア K-Q、エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "J",
            Card "♥" "J",
            Card "♦" "T",
            Card "♣" "T",
            Card "♠" "5"
          ],
        playerHands =
          [ [Card "♥" "K", Card "♦" "3"],
            [Card "♣" "Q", Card "♠" "4"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（ツーペア J-T、キングキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "9",
            Card "♥" "9",
            Card "♦" "8",
            Card "♣" "8",
            Card "♠" "6"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "2"],
            [Card "♣" "K", Card "♠" "3"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（ツーペア 9-8、エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "7",
            Card "♥" "7",
            Card "♦" "6",
            Card "♣" "6",
            Card "♠" "4"
          ],
        playerHands =
          [ [Card "♥" "J", Card "♦" "3"],
            [Card "♣" "Q", Card "♠" "2"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（ツーペア 7-6、クイーンキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "5",
            Card "♥" "5",
            Card "♦" "4",
            Card "♣" "4",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♥" "K", Card "♦" "2"],
            [Card "♣" "A", Card "♠" "2"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（5ハイストレート）
      },
    Problem
      { communityCards =
          [ Card "♠" "K",
            Card "♥" "K",
            Card "♦" "K",
            Card "♣" "5",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♥" "J", Card "♦" "T"],
            [Card "♣" "Q", Card "♠" "8"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（スリーカード、クイーンキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "Q",
            Card "♥" "Q",
            Card "♦" "Q",
            Card "♣" "7",
            Card "♠" "4"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "2"],
            [Card "♣" "K", Card "♠" "3"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（スリーカード、エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "J",
            Card "♥" "J",
            Card "♦" "J",
            Card "♣" "8",
            Card "♠" "5"
          ],
        playerHands =
          [ [Card "♥" "K", Card "♦" "3"],
            [Card "♣" "Q", Card "♠" "4"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（スリーカード、キングキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "T",
            Card "♥" "T",
            Card "♦" "T",
            Card "♣" "9",
            Card "♠" "6"
          ],
        playerHands =
          [ [Card "♥" "Q", Card "♦" "4"],
            [Card "♣" "K", Card "♠" "3"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（スリーカード、キングキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "9",
            Card "♥" "9",
            Card "♦" "9",
            Card "♣" "7",
            Card "♠" "4"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "2"],
            [Card "♣" "K", Card "♠" "3"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（スリーカード、エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "8",
            Card "♥" "8",
            Card "♦" "8",
            Card "♣" "6",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♥" "J", Card "♦" "2"],
            [Card "♣" "Q", Card "♠" "4"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（スリーカード、クイーンキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "4",
            Card "♥" "4",
            Card "♦" "4",
            Card "♣" "3",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♥" "K", Card "♦" "5"],
            [Card "♣" "A", Card "♠" "6"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（スリーカード、エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "A",
            Card "♥" "Q",
            Card "♦" "J",
            Card "♣" "8",
            Card "♠" "4"
          ],
        playerHands =
          [ [Card "♥" "K", Card "♦" "7"],
            [Card "♣" "T", Card "♠" "9"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（エース・キングハイ）
      },
    Problem
      { communityCards =
          [ Card "♠" "K",
            Card "♥" "K",
            Card "♦" "5",
            Card "♣" "3",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♥" "9", Card "♦" "7"],
            [Card "♣" "T", Card "♠" "8"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（キングペア、テンキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "Q",
            Card "♥" "Q",
            Card "♦" "6",
            Card "♣" "4",
            Card "♠" "2"
          ],
        playerHands =
          [ [Card "♥" "A", Card "♦" "3"],
            [Card "♣" "K", Card "♠" "5"]
          ],
        correctAnswer = 1 -- プレイヤー1が勝利（クイーンペア、エースキッカー）
      },
    Problem
      { communityCards =
          [ Card "♠" "J",
            Card "♥" "J",
            Card "♦" "7",
            Card "♣" "5",
            Card "♠" "3"
          ],
        playerHands =
          [ [Card "♥" "Q", Card "♦" "4"],
            [Card "♣" "K", Card "♠" "2"]
          ],
        correctAnswer = 2 -- プレイヤー2が勝利（ジャックペア、キングキッカー）
      }
  ]