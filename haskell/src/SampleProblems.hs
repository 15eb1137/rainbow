module SampleProblems
  ( sampleProblems,
  )
where

import Lib

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
        correctAnswer = 1 -- プレイヤー1が勝利（エースハイストレート vs キングハイ）
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
        correctAnswer = 2 -- プレイヤー2が勝利（4カードvs フルハウス）
      },
    -- 新規追加問題: フラッシュ vs ストレート
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
        correctAnswer = 1 -- プレイヤー1が勝利（フラッシュ）
      },
    -- 新規追加問題: フルハウス vs フルハウス（キッカー勝負）
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
        correctAnswer = 2 -- プレイヤー2が勝利（キングキッカー）
      },
    -- 新規追加問題: 2ペア vs 2ペア（ハイカード勝負）
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
    -- 新規追加問題: ストレートフラッシュ vs フォーカード
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
    -- 新規追加問題: ロイヤルフラッシュ vs ストレートフラッシュ
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
    -- 新規追加問題: トリプス vs トリプス（キッカー勝負）
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
    -- 新規追加問題: フルハウス vs フラッシュ
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
        correctAnswer = 1 -- プレイヤー1が勝利（フルハウス）
      },
    -- 新規追加問題: ストレート vs ストレート（ハイカード勝負）
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
    -- 新規追加問題: ペア vs ハイカード
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
    -- 新規追加問題13: フォーカード vs フォーカード（キッカー勝負）
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
    -- 新規追加問題14: フラッシュ vs フラッシュ（ハイカード勝負）
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
    -- 新規追加問題15: ストレート vs 2ペア
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
    -- 新規追加問題16: フルハウス vs フルハウス（高位ペア勝負）
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
        correctAnswer = 1 -- プレイヤー1が勝利（エースフル）
      },
    -- 新規追加問題17: トリプス vs 2ペア
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
        correctAnswer = 1 -- プレイヤー1が勝利（トリプルテン）
      },
    -- 新規追加問題18: ストレートフラッシュ vs ストレートフラッシュ
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
        correctAnswer = 2 -- プレイヤー2が勝利（10ハイストレートフラッシュ）
      },
    -- 新規追加問題19: 3ペア可能状況での2ペア判定
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
        correctAnswer = 2 -- プレイヤー2が勝利（キングス・エース・クイーン）
      },
    -- 新規追加問題20: ボードでのフォーカード + キッカー勝負
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
    -- 新規追加問題21: 複雑なストレート状況
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
        correctAnswer = 1 -- プレイヤー1が勝利（キングハイストレート）
      },
    -- 新規追加問題22: ボードペアでのキッカー2枚勝負
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
    -- 新規追加問題23: フラッシュドロー完成判定
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
    -- 新規追加問題24: 微妙な2ペア状況
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
    -- 新規追加問題25: ストレート vs フラッシュ
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
    -- 新規追加問題26: 複雑なフルハウス状況
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
        correctAnswer = 1 -- プレイヤー1が勝利（キングスフル）
      },
    -- 新規追加問題27: エースローストレート
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
    -- 新規追加問題28: トリプス vs フラッシュ
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
    -- 新規追加問題29: 同じ2ペアでのキッカー勝負
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
    -- 新規追加問題30: ペア vs ペア（キッカー3枚勝負）
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
    -- 新規追加問題31: ハイカード勝負
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
    -- 新規追加問題32: フォーカード vs フルハウス
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
      }
  ]