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
    # スコアと残り時間の表示用のコンテナを作成
    score_time_container = st.empty()
    
    # 経過時間を計算
    elapsed_time = int(time.time() - st.session_state.start_time)
    remaining_time = max(60 - elapsed_time, 0)
    
    # 残り時間とスコアの表示
    with score_time_container:
        col1, col2 = st.columns(2)
        with col1:
            st.write("### ⏳ 残り時間：", f"{remaining_time}秒")
        with col2:
            st.write("### 🎯 スコア：", f"{st.session_state.score}点")

    if remaining_time <= 0:
        st.session_state.game_state = "end"
        st.rerun()
        return
    
    st.write("")

    # コミュニティカード表示
    community_cards = ' '.join(str(card) for card in st.session_state.problem_list[st.session_state.current_problem_index].community_cards)
    st.write("🃏 コミュニティカード")
    st.write(f"##### {community_cards}")
    st.write("")
    st.write("👥 プレイヤーの手札")
    
    # プレイヤーの手札とボタンを表示するコンテナ
    game_container = st.container()
    
    with game_container:
        for i, hand in enumerate(st.session_state.problem_list[st.session_state.current_problem_index].player_hands, 1):
            cols = st.columns([2, 1])
            with cols[0]:
                st.write(f"##### プレイヤー{i}：{str(hand[0])} {str(hand[1])}")
            with cols[1]:
                if st.button(f"プレイヤー{i}を選択", key=f"btn_{i}_{st.session_state.key_suffix}", use_container_width=True):
                    if i == st.session_state.problem_list[st.session_state.current_problem_index].correct_answer:
                        st.session_state.score += 10
                        st.session_state.temp_message = {"type": "success", "text": "正解です！ +10点"}
                    else:
                        st.session_state.score -= 5
                        st.session_state.temp_message = {"type": "error", "text": "不正解です。 -5点"}



        # スプリットポットのボタンを同じ幅で配置
        cols = st.columns([2, 1])
        with cols[0]:
            st.write("")
        with cols[1]:
            if st.button("チョップ", key=f"btn_3_{st.session_state.key_suffix}", use_container_width=True):
                if 3 == st.session_state.problem_list[st.session_state.current_problem_index].correct_answer:
                    st.session_state.score += 10
                    st.session_state.temp_message = {"type": "success", "text": "正解です！ +10点"}
                else:
                    st.session_state.score -= 5
                    st.session_state.temp_message = {"type": "error", "text": "不正解です。 -5点"}
    
    st.write("")

    # メッセージ表示用のコンテナを作成
    message_container = st.empty()
    
    # メッセージを表示
    if hasattr(st.session_state, 'temp_message'):
        with message_container:
            if st.session_state.temp_message["type"] == "success":
                st.success(st.session_state.temp_message["text"])
            else:
                st.error(st.session_state.temp_message["text"])
        # 1秒後にメッセージを消去
        time.sleep(1)
        delattr(st.session_state, 'temp_message')
        message_container.empty()
        
        # 次の問題に進む
        st.session_state.problems_solved += 1
        if st.session_state.current_problem_index + 1 < len(st.session_state.problem_list):
            st.session_state.current_problem_index += 1
        else:
            st.session_state.current_problem_index = 0
        st.session_state.key_suffix += 1
        st.rerun()

    # 自動更新のためのrerun
    time.sleep(0.1)
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
            div[data-testid="stMainBlockContainer"] {
            padding: 0;
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


def main():

    st.set_page_config(page_title="Quick Draw Poker Showdown")
    st.markdown(HIDE_ST_STYLE, unsafe_allow_html=True)
    initialize_session_state()

    if st.session_state.game_state == "start":
        start_screen()
    elif st.session_state.game_state == "playing":
        game_screen()
    else:
        end_screen()


if __name__ == "__main__":
    main()
