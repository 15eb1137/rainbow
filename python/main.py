import streamlit as st
from st_supabase_connection import SupabaseConnection, execute_query
import random
import time
from dataclasses import dataclass
from typing import List, Tuple
import requests


@dataclass
class Card:
    suit: str
    rank: str

    def __str__(self):
        return f"[{self.suit}{self.rank}]"


@dataclass
class Problem:
    community_cards: List[Card]
    player_hands: List[Tuple[Card, Card]]
    correct_answer: int


def get_problems() -> List[Problem]:
    # Initialize Supabase connection
    conn = st.connection('supabase', type=SupabaseConnection, ttl=None)
    # Perform a query
    res = execute_query(conn.table('problems').select('*'), ttl=0)

    problems = [
        Problem(
            community_cards=[Card(suit=card[0], rank=card[1])
                             for card in row['community_cards'].split(',')],
            player_hands=[[Card(suit=card[0], rank=card[1]) for card in hand.split(',')]
                          for hand in row['player_hands'].split('|')],
            correct_answer=row['correct_answer']
        ) for row in res.data
    ]

    random.shuffle(problems)

    return problems


def initialize_session_state():
    if 'game_state' not in st.session_state:
        st.session_state.game_state = "start"
    if 'score' not in st.session_state:
        st.session_state.score = 0
    if 'problems_solved' not in st.session_state:
        st.session_state.problems_solved = 0
    if 'start_time' not in st.session_state:
        st.session_state.start_time = None
    if 'problem_list' not in st.session_state:
        st.session_state.problem_list = []
    if 'current_problem_index' not in st.session_state:
        st.session_state.current_problem_index = None
    if 'key_suffix' not in st.session_state:
        st.session_state.key_suffix = 0  # ボタンのキーを動的に変更するため


HIDE_ST_STYLE = """
            <style>
            div[data-testid="stToolbar"] {
            visibility: hidden;
            height: 0%;
            position: fixed;
            }
            div[data-testid="stDecoration"] {
            visibility: hidden;
            height: 0%;
            position: fixed;
            }
            #MainMenu {
            visibility: hidden;
            height: 0%;
            }
            header {
            visibility: hidden;
            height: 0%;
            }
            footer {
            visibility: hidden;
            height: 0%;
            }
                    .appview-container .main .block-container{
                        padding-top: 1rem;
                        padding-right: 3rem;
                        padding-left: 3rem;
                        padding-bottom: 1rem;
                    }  
                    .reportview-container {
                        padding-top: 0rem;
                        padding-right: 3rem;
                        padding-left: 3rem;
                        padding-bottom: 0rem;
                    }
                    header[data-testid="stHeader"] {
                        z-index: -1;
                    }
                    div[data-testid="stToolbar"] {
                    z-index: 100;
                    }
                    div[data-testid="stDecoration"] {
                    z-index: 100;
                    }
            </style>
"""


def start_screen():
    st.title("Quick Draw Poker Showdown")
    st.write("### ゲーム説明")
    st.write("""
    - 制限時間は60秒です
    - 正解で+10点、不正解で-5点です
    - できるだけ多くの問題を正確に解いてください
    """)

    if st.button("ゲームスタート", use_container_width=True):
        st.session_state.game_state = "playing"
        st.session_state.start_time = time.time()
        st.session_state.problem_list = get_problems()
        st.session_state.current_problem_index = 0
        st.rerun()


def game_screen():
    st.markdown(HIDE_ST_STYLE, unsafe_allow_html=True)

    st.title("Quick Draw Poker Showdown")

    # スコアと残り時間の表示
    col1, col2 = st.columns(2)
    elapsed_time = int(time.time() - st.session_state.start_time)
    remaining_time = max(60 - elapsed_time, 0)

    with col1:
        st.metric("残り時間", f"{remaining_time}秒")
    with col2:
        st.metric("現在のスコア", f"{st.session_state.score}点")

    if remaining_time > 0:
        # 問題表示
        st.subheader("コミュニティカード")
        st.write(" ".join(str(card)
                 for card in st.session_state.problem_list[st.session_state.current_problem_index].community_cards))

        st.subheader("プレイヤーの手札")
        for i, hand in enumerate(st.session_state.problem_list[st.session_state.current_problem_index].player_hands, 1):
            st.write(f"プレイヤー{i}: {str(hand[0])} {str(hand[1])}")

        # 回答ボタン
        st.write("### 勝者を選択してください：")
        cols = st.columns(3)

        # 動的なキーを使用してボタンを生成
        for i, label in enumerate(["プレイヤー1", "プレイヤー2", "スプリットポット"], 1):
            with cols[i-1]:
                if st.button(label, key=f"btn_{i}_{st.session_state.key_suffix}", use_container_width=True):
                    if i == st.session_state.problem_list[st.session_state.current_problem_index].correct_answer:
                        st.success("正解です！ +10点")
                        st.session_state.score += 10
                    else:
                        st.error("不正解です。 -5点")
                        st.session_state.score -= 5

                    st.session_state.problems_solved += 1
                    if st.session_state.current_problem_index + 1 < len(st.session_state.problem_list):
                        st.session_state.current_problem_index += 1
                    else:
                        st.session_state.current_problem_index = 0
                    st.session_state.key_suffix += 1  # キーを更新して新しいボタンを生成
                    time.sleep(0.5)
                    st.rerun()
    else:
        st.session_state.game_state = "end"
        st.rerun()


def end_screen():
    st.title("ゲーム終了！")
    st.write(f"### 最終スコア: {st.session_state.score}点")
    st.write(f"### 解答した問題数: {st.session_state.problems_solved}")

    col1, col2 = st.columns(2)
    with col1:
        if st.button("もう一度プレイ", use_container_width=True):
            st.session_state.clear()
            initialize_session_state()
            st.session_state.game_state = "playing"
            st.session_state.start_time = time.time()
            st.session_state.problem_list = get_problems()
            st.session_state.current_problem_index = 0
            st.rerun()

    with col2:
        if st.button("スタート画面に戻る", use_container_width=True):
            st.session_state.clear()
            initialize_session_state()
            st.rerun()


def main():
    st.set_page_config(page_title="Quick Draw Poker Showdown")
    initialize_session_state()

    if st.session_state.game_state == "start":
        start_screen()
    elif st.session_state.game_state == "playing":
        game_screen()
    else:
        end_screen()


if __name__ == "__main__":
    main()
