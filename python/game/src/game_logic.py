from typing import List
import random
from src.data_manager import get_problems_from_supabase
from src.models import Card, Problem


def get_problems() -> List[Problem]:
    res = get_problems_from_supabase()
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
