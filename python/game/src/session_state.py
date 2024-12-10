import streamlit as st
import time
from src.data_manager import save_game_results
from src.game_logic import get_problems
from src.models import Answer
from src.constants import CORRECT_POINTS, INCORRECT_POINTS


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


def game_start_update_session_state() -> None:
    st.session_state.game_state = "playing"
    st.session_state.problem_list = get_problems()
    st.session_state.current_problem_index = 0
    st.session_state.answer_list = []
    st.session_state.game_session_start_time = time.time()
    st.session_state.current_problem_start_time = time.time()
    st.rerun()


def game_end_update_session_state() -> None:
    st.session_state.game_state = "end"
    if hasattr(st.session_state, 'temp_message'):
        delattr(st.session_state, 'temp_message')
    st.session_state.current_problem_index = None
    st.rerun()


def correct_answer_update_session_state(i: int) -> None:
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


def incorrect_answer_update_session_state(i: int) -> None:
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


def next_problem_update_session_state() -> None:
    st.session_state.problems_solved += 1
    if st.session_state.current_problem_index + 1 < len(st.session_state.problem_list):
        st.session_state.current_problem_index += 1
    else:
        st.session_state.current_problem_index = 0
    st.session_state.current_problem_start_time = time.time()
    st.session_state.key_suffix += 1
    st.rerun()


def save_results_update_session_state() -> None:
    # 結果を保存
    if 'results_saved' not in st.session_state:
        st.session_state.results_saved = False

    if not st.session_state.results_saved:
        with st.spinner("結果を集計中..."):
            if save_game_results():
                st.session_state.results_saved = True
                st.success("お疲れ様でした！")


def reset_game_update_session_state() -> None:
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


def goto_start_screen_update_session_state() -> None:
    st.session_state.clear()
    initialize_session_state()
    st.rerun()
