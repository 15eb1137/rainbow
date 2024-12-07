import streamlit as st
from st_supabase_connection import SupabaseConnection, execute_query
import random
import time
from dataclasses import dataclass
from typing import List, Tuple
from datetime import datetime

GAME_DURATION = 60
CORRECT_POINTS = 10
INCORRECT_POINTS = 5


@dataclass
class Card:
    suit: str
    rank: str

    def __str__(self):
        return f"[{self.suit}{self.rank}]"


@dataclass
class Problem:
    id: int
    community_cards: List[Card]
    player_hands: List[Tuple[Card, Card]]
    correct_answer: int


@dataclass
class Answer:
    problem: Problem
    player_answer: int
    point: int
    elapsed_time: int


def get_problems() -> List[Problem]:
    try:
        # Initialize Supabase connection
        conn = st.connection('supabase', type=SupabaseConnection, ttl=None)
        # Perform a query
        res = execute_query(conn.table('problems').select('*'), ttl=0)

        problems = [
            Problem(
                id=row['id'],
                community_cards=[Card(suit=card[0], rank=card[1])
                                 for card in row['community_cards'].split(',')],
                player_hands=[[Card(suit=card[0], rank=card[1]) for card in hand.split(',')]
                              for hand in row['player_hands'].split('|')],
                correct_answer=row['correct_answer']
            ) for row in res.data
        ]

        random.shuffle(problems)

        return problems
    except Exception as e:
        st.error(f"エラーが発生しました: {e}")
        return []


def initialize_session_state() -> None:
    if 'game_state' not in st.session_state:
        st.session_state.game_state = "start"
    if 'score' not in st.session_state:
        st.session_state.score = 0
    if 'problems_solved' not in st.session_state:
        st.session_state.problems_solved = 0
    if 'game_session_start_time' not in st.session_state:
        st.session_state.game_session_start_time = None
    if 'current_problem_start_time' not in st.session_state:
        st.session_state.current_problem_start_time = None
    if 'problem_list' not in st.session_state:
        st.session_state.problem_list = []
    if 'current_problem_index' not in st.session_state:
        st.session_state.current_problem_index = None
    if 'key_suffix' not in st.session_state:
        st.session_state.key_suffix = 0  # ボタンのキーを動的に変更するため
    if 'answer_list' not in st.session_state:
        st.session_state.answer_list = []


def start_screen() -> None:
    st.title("Quick Draw Poker Showdown")
    st.write("### ゲーム説明")
    st.write(f"""
    - このゲームはポーカーのハンドの強さを判定するゲームです
    - 各問題にはコミュニティカードとプレイヤーの手札が表示されます
    - 勝っているプレイヤーを選択するか、引き分けの場合はチョップを選択してください
    - 正解で+{CORRECT_POINTS}点、不正解で-{INCORRECT_POINTS}点です
    - 制限時間は{GAME_DURATION}秒です
    - できるだけ多くの問題を正確に解いてください
    """)

    if st.button("ゲームスタート", use_container_width=True):
        st.session_state.game_state = "playing"
        st.session_state.problem_list = get_problems()
        st.session_state.current_problem_index = 0
        st.session_state.answer_list = []
        st.session_state.game_session_start_time = time.time()
        st.session_state.current_problem_start_time = time.time()
        st.rerun()


def game_screen() -> None:
    # スコアと残り時間の表示用のコンテナを作成
    score_time_container = st.empty()

    # 経過時間を計算
    gs_elapsed_time = int(
        time.time() - st.session_state.game_session_start_time)
    gs_remaining_time = max(GAME_DURATION - gs_elapsed_time, 0)

    # 残り時間とスコアの表示
    with score_time_container:
        col1, col2 = st.columns(2)
        with col1:
            st.write("### ⏳ 残り時間：", f"{gs_remaining_time}秒")
        with col2:
            st.write("### 🎯 スコア：", f"{st.session_state.score}点")

    if gs_remaining_time <= 0:
        st.session_state.game_state = "end"
        if hasattr(st.session_state, 'temp_message'):
            delattr(st.session_state, 'temp_message')
        st.session_state.current_problem_index = None
        st.rerun()
        return

    st.write("")

    # コミュニティカード表示
    community_cards = ' '.join(str(
        card) for card in st.session_state.problem_list[st.session_state.current_problem_index].community_cards)
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
                        st.session_state.score += CORRECT_POINTS
                        st.session_state.temp_message = {
                            "type": "success", "text": f"正解です！ +{CORRECT_POINTS}点"}
                        st.session_state.answer_list.append(
                            Answer(
                                problem=st.session_state.problem_list[
                                    st.session_state.current_problem_index],
                                player_answer=i,
                                point=CORRECT_POINTS,
                                elapsed_time=round(
                                    time.time() - st.session_state.current_problem_start_time, 1)
                            ))
                    else:
                        st.session_state.score -= INCORRECT_POINTS
                        st.session_state.temp_message = {
                            "type": "error", "text": f"不正解です。 -{INCORRECT_POINTS}点"}
                        st.session_state.answer_list.append(
                            Answer(
                                problem=st.session_state.problem_list[
                                    st.session_state.current_problem_index],
                                player_answer=i,
                                point=-INCORRECT_POINTS,
                                elapsed_time=round(
                                    time.time() - st.session_state.current_problem_start_time, 1)
                            ))

        # スプリットポットのボタンを同じ幅で配置
        cols = st.columns([2, 1])
        with cols[0]:
            st.write("")
        with cols[1]:
            if st.button("チョップ", key=f"btn_3_{st.session_state.key_suffix}", use_container_width=True):
                if 3 == st.session_state.problem_list[st.session_state.current_problem_index].correct_answer:
                    st.session_state.score += CORRECT_POINTS
                    st.session_state.temp_message = {
                        "type": "success", "text": f"正解です！ +{CORRECT_POINTS}点"}
                    st.session_state.answer_list.append(
                        Answer(
                            problem=st.session_state.problem_list[
                                st.session_state.current_problem_index], player_answer=3,
                            point=CORRECT_POINTS,
                            elapsed_time=round(
                                time.time() - st.session_state.current_problem_start_time, 1)
                        ))
                else:
                    st.session_state.score -= INCORRECT_POINTS
                    st.session_state.temp_message = {
                        "type": "error", "text": f"不正解です。 -{INCORRECT_POINTS}点"}
                    st.session_state.answer_list.append(
                        Answer(
                            problem=st.session_state.problem_list[
                                st.session_state.current_problem_index], player_answer=3,
                            point=-INCORRECT_POINTS,
                            elapsed_time=round(
                                time.time() - st.session_state.current_problem_start_time, 1)
                        ))

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
        st.session_state.current_problem_start_time = time.time()
        st.session_state.key_suffix += 1
        st.rerun()

    # 自動更新のためのrerun
    time.sleep(1)
    st.rerun()


def save_game_results() -> None:
    """ゲーム結果をSupabaseに保存する"""
    try:
        conn = st.connection('supabase', type=SupabaseConnection, ttl=None)

        # ゲームセッション情報の保存
        game_session_data = {
            'score': st.session_state.score,
            'problems_solved': st.session_state.problems_solved,
            'game_duration': GAME_DURATION,
            'played_at': datetime.now().isoformat()
        }

        # game_sessionsテーブルに保存してIDを取得
        result = execute_query(
            conn.table('game_sessions').insert(game_session_data),
            ttl=0
        )
        game_session_id = result.data[0]['id']

        # 各回答の保存
        for answer in st.session_state.answer_list:
            answer_data = {
                'game_session_id': game_session_id,
                'community_cards': ','.join(f"{card.suit}{card.rank}" for card in answer.problem.community_cards),
                'player_hands': '|'.join(','.join(f"{card.suit}{card.rank}" for card in hand)
                                         for hand in answer.problem.player_hands),
                'correct_answer': answer.problem.correct_answer,
                'player_answer': answer.player_answer,
                'point': answer.point,
                'elapsed_time': answer.elapsed_time,
                'answered_at': datetime.now().isoformat()
            }

            execute_query(
                conn.table('game_answers').insert(answer_data),
                ttl=0
            )

        return True

    except Exception as e:
        st.error(f"結果の保存中にエラーが発生しました: {e}")
        return False


def end_screen():
    st.title("ゲーム終了！")

    # 結果を保存
    if 'results_saved' not in st.session_state:
        st.session_state.results_saved = False

    if not st.session_state.results_saved:
        with st.spinner("結果を集計中..."):
            if save_game_results():
                st.session_state.results_saved = True
                st.success("お疲れ様でした！")

    st.write(f"### 最終スコア: {st.session_state.score}点")
    st.write(f"### 解答した問題数: {st.session_state.problems_solved}")

    col1, col2 = st.columns(2)
    with col1:
        if st.button("もう一度プレイ", use_container_width=True): #前の要素が残ってしまうバグあり
            st.session_state.clear()
            initialize_session_state()
            st.session_state.game_state = "playing"
            st.session_state.score = 0
            st.session_state.problems_solved = 0
            st.session_state.answer_list = []
            st.session_state.problem_list = get_problems()
            st.session_state.current_problem_index = 0
            st.session_state.game_session_start_time = time.time()
            st.session_state.current_problem_start_time = time.time()
            st.session_state.key_suffix = 0
            if hasattr(st.session_state, 'temp_message'):
                delattr(st.session_state, 'temp_message')
            st.rerun()

    with col2:
        if st.button("スタート画面に戻る", use_container_width=True):
            st.session_state.clear()
            initialize_session_state()
            st.rerun()

    st.write("")
    st.write("### 📝 解答の振り返り")

    if len(st.session_state.answer_list) > 0:
        for i, answer in enumerate(st.session_state.answer_list, 1):
            with st.expander(f"問題 {i} ({'+' if answer.point > 0 else ''}{answer.point}点)"):
                # コミュニティカードの表示
                community_cards = ' '.join(
                    str(card) for card in answer.problem.community_cards)
                st.write("🃏 コミュニティカード")
                st.write(f"##### {community_cards}")

                # プレイヤーの手札を表示
                st.write("👥 プレイヤーの手札")
                for j, hand in enumerate(answer.problem.player_hands, 1):
                    hand_str = f"{str(hand[0])} {str(hand[1])}"
                    if j == answer.problem.correct_answer:
                        st.write(f"##### プレイヤー{j}：{hand_str} ✅ (正解)")
                    else:
                        st.write(f"##### プレイヤー{j}：{hand_str}")

                # プレイヤーの回答を表示
                st.write("")
                st.write("あなたの回答:")
                if answer.player_answer == 3:
                    answer_text = "チョップ"
                else:
                    answer_text = f"プレイヤー{answer.player_answer}"

                if answer.point > 0:
                    st.success(f"✅ {answer_text} (正解)")
                else:
                    if answer.problem.correct_answer == 3:
                        correct_text = "チョップ"
                    else:
                        correct_text = f"プレイヤー{answer.problem.correct_answer}"
                    st.error(f"❌ {answer_text} (正解は {correct_text})")
    else:
        st.info("解答履歴がありません")


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
