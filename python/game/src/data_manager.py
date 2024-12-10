import streamlit as st
from datetime import datetime
from st_supabase_connection import SupabaseConnection, execute_query, APIResponse
from src.constants import GAME_DURATION


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


def get_problems_from_supabase() -> APIResponse:
    """Supabaseから問題を取得する"""
    try:
        # Initialize Supabase connection
        conn = st.connection('supabase', type=SupabaseConnection, ttl=None)
        # Perform a query
        res = execute_query(conn.table('problems').select('*'), ttl=0)
        return res
    except Exception as e:
        st.error(f"エラーが発生しました: {e}")
        return []
