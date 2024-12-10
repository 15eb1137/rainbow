import time
import streamlit as st

from src.models import Answer
from src.session_state import correct_answer_update_session_state, incorrect_answer_update_session_state, next_problem_update_session_state


def score_time_container(gs_remaining_time: int):
    # スコアと残り時間の表示用のコンテナを作成
    score_time_container = st.empty()

    # 残り時間とスコアの表示
    with score_time_container:
        col1, col2 = st.columns(2)
        with col1:
            st.write("### ⏳ 残り時間：", f"{gs_remaining_time}秒")
        with col2:
            st.write("### 🎯 スコア：", f"{st.session_state.score}点")


def community_cards():
    # コミュニティカード表示
    community_cards = ' '.join(str(
        card) for card in st.session_state.problem_list[st.session_state.current_problem_index].community_cards)
    st.write("🃏 コミュニティカード")
    st.write(f"##### {community_cards}")
    st.write("")
    st.write("👥 プレイヤーの手札")


def game_container():
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
                        correct_answer_update_session_state(i)
                    else:
                        incorrect_answer_update_session_state(i)

        # スプリットポットのボタンを同じ幅で配置
        cols = st.columns([2, 1])
        with cols[0]:
            st.write("")
        with cols[1]:
            if st.button("チョップ", key=f"btn_3_{st.session_state.key_suffix}", use_container_width=True):
                if 3 == st.session_state.problem_list[st.session_state.current_problem_index].correct_answer:
                    correct_answer_update_session_state(3)
                else:
                    incorrect_answer_update_session_state(3)

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
        next_problem_update_session_state()

def answer_history(i:int, answer:Answer):
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