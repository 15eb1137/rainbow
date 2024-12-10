import time
import streamlit as st
from src.ui_components import answer_history, community_cards, game_container, score_time_container
from src.constants import GAME_DURATION, CORRECT_POINTS, INCORRECT_POINTS
from src.session_state import correct_answer_update_session_state, goto_start_screen_update_session_state, incorrect_answer_update_session_state, game_start_update_session_state, game_end_update_session_state, next_problem_update_session_state, reset_game_update_session_state, save_results_update_session_state


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
        game_start_update_session_state()


def game_screen() -> None:
    # 経過時間を計算
    gs_elapsed_time = int(
        time.time() - st.session_state.game_session_start_time)
    gs_remaining_time = max(GAME_DURATION - gs_elapsed_time, 0)

    score_time_container(gs_remaining_time)

    if gs_remaining_time <= 0:
        game_end_update_session_state()
        return

    st.write("")

    community_cards()

    game_container()

    # 自動更新のためのrerun
    time.sleep(1)
    st.rerun()


def end_screen():
    st.title("ゲーム終了！")

    save_results_update_session_state()

    st.write(f"### 最終スコア: {st.session_state.score}点")
    st.write(f"### 解答した問題数: {st.session_state.problems_solved}")

    col1, col2 = st.columns(2)
    with col1:
        if st.button("もう一度プレイ", use_container_width=True):  # 前の要素が残ってしまうバグあり
            reset_game_update_session_state()

    with col2:
        if st.button("スタート画面に戻る", use_container_width=True):
            goto_start_screen_update_session_state()

    st.write("")
    st.write("### 📝 解答の振り返り")

    if len(st.session_state.answer_list) > 0:
        for i, answer in enumerate(st.session_state.answer_list, 1):
            answer_history(i, answer)
    else:
        st.info("解答履歴がありません")
