import streamlit as st
from st_supabase_connection import SupabaseConnection, execute_query
import pandas as pd
import plotly.express as px

# Initialize Supabase connection
conn = st.connection('supabase', type=SupabaseConnection, ttl=None)

def create_count_graph(df, x_col, chart_type="棒グラフ"):
    """件数を集計してグラフを作成する関数"""
    count_df = df[x_col].value_counts().reset_index()
    count_df.columns = [x_col, '件数']
    
    if chart_type == "折れ線グラフ":
        fig = px.line(count_df, x=x_col, y='件数')
    elif chart_type == "棒グラフ":
        fig = px.bar(count_df, x=x_col, y='件数')
    else:  # ヒストグラム
        fig = px.histogram(df, x=x_col)
    
    # グラフのレイアウトを暗いテーマに設定
    fig.update_layout(
        plot_bgcolor='rgba(0,0,0,0.8)',
        paper_bgcolor='rgba(0,0,0,0.8)',
        font_color='white',
        xaxis=dict(gridcolor='rgba(128,128,128,0.2)'),
        yaxis=dict(gridcolor='rgba(128,128,128,0.2)')
    )
    
    return fig, count_df

def main():
    st.title("データ分析ダッシュボード")
    
    # サイドバーにテーブル選択を追加
    table_name = st.sidebar.selectbox(
        "分析するテーブルを選択してください",
        ["problems", "hand_rankings", "winning_hands", "game_sessions", "game_answers"]
    )
    
    # データの取得
    try:
        # 選択されたテーブルのデータを取得
        res = execute_query(conn.table(table_name).select('*'), ttl=0)
        df = pd.DataFrame(res.data)
        
        # データの基本情報を表示
        st.subheader("データの基本情報")
        st.write(f"レコード数: {len(df)}")
        st.write(f"カラム: {', '.join(df.columns)}")
        
        # problemsテーブルが選択された場合、正解の分布を表示
        if table_name == "problems" and 'correct_answer' in df.columns:
            st.subheader("正解の分布")
            default_fig, count_df = create_count_graph(df, 'correct_answer')
            st.plotly_chart(default_fig)
            st.write("正解ごとの件数")
            st.dataframe(count_df)
        
        # データプレビュー
        st.subheader("データプレビュー")
        st.dataframe(df.head())
        
        # 追加の分析セクション
        st.subheader("追加の分析")
        
        # グラフタイプの選択
        chart_type = st.selectbox(
            "グラフタイプを選択",
            ["折れ線グラフ", "棒グラフ", "散布図", "ヒストグラム"]
        )
        
        # X軸の選択
        x_col = st.selectbox("分析するカラムを選択", df.columns)
        
        # グラフの種類を選択
        graph_mode = st.radio(
            "分析タイプを選択",
            ["件数を集計", "数値を分析"]
        )
        
        if graph_mode == "件数を集計":
            if chart_type != "散布図":
                fig, count_df = create_count_graph(df, x_col, chart_type)
                st.plotly_chart(fig)
                st.write("集計結果")
                st.dataframe(count_df)
            else:
                st.warning("散布図は件数の集計には適していません。別のグラフタイプを選択してください。")
        
        else:  # 数値を分析
            numeric_cols = df.select_dtypes(include=['float64', 'int64']).columns
            if len(numeric_cols) > 0:
                st.subheader("数値データの統計情報")
                st.dataframe(df[numeric_cols].describe())
                
                y_col = st.selectbox("Y軸を選択（数値カラム）", numeric_cols)
                
                if chart_type == "折れ線グラフ":
                    fig = px.line(df, x=x_col, y=y_col)
                elif chart_type == "棒グラフ":
                    fig = px.bar(df, x=x_col, y=y_col)
                elif chart_type == "散布図":
                    fig = px.scatter(df, x=x_col, y=y_col)
                else:  # ヒストグラム
                    fig = px.histogram(df, x=x_col)
                
                # グラフのレイアウトを暗いテーマに設定
                fig.update_layout(
                    plot_bgcolor='rgba(0,0,0,0.8)',
                    paper_bgcolor='rgba(0,0,0,0.8)',
                    font_color='white',
                    xaxis=dict(gridcolor='rgba(128,128,128,0.2)'),
                    yaxis=dict(gridcolor='rgba(128,128,128,0.2)')
                )
                
                st.plotly_chart(fig)
            else:
                st.warning("数値データのカラムが存在しません")
        
        # データのフィルタリング機能
        st.subheader("データフィルタリング")
        filter_col = st.selectbox("フィルターするカラムを選択", df.columns)
        if df[filter_col].dtype in ['float64', 'int64']:
            min_val = float(df[filter_col].min())
            max_val = float(df[filter_col].max())
            filter_range = st.slider(
                f"{filter_col}の範囲を選択",
                min_val,
                max_val,
                (min_val, max_val)
            )
            filtered_df = df[
                (df[filter_col] >= filter_range[0]) & 
                (df[filter_col] <= filter_range[1])
            ]
        else:
            unique_values = df[filter_col].unique()
            selected_values = st.multiselect(
                f"{filter_col}の値を選択",
                unique_values,
                default=list(unique_values)
            )
            filtered_df = df[df[filter_col].isin(selected_values)]
        
        st.write("フィルター適用後のデータ")
        st.dataframe(filtered_df)
            
    except Exception as e:
        st.error(f"エラーが発生しました: {str(e)}")

if __name__ == "__main__":
    main()