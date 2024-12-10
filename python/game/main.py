import streamlit as st
from src.ui_screen import start_screen, game_screen, end_screen
from src.session_state import initialize_session_state
from src.style import HIDE_ST_STYLE


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
