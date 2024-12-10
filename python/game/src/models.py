from dataclasses import dataclass
from typing import List, Tuple


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
