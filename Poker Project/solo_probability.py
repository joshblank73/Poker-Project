from itertools import combinations
from poker_basics import Card, Hand, hand_hierarchy
import copy

def best_hand(hands: list[Hand]) -> Hand:
    """Identifies the best hand from a list of hands.

    Returns the best hand.
    """
    return max(hands)

def post_flop(deck: list, current_board: list, player: list, num_opponents: int) -> float:
    """Returns the probabilty of the player winning or tying after the flop."""
    turn_river = list(combinations(deck, 2))
    player_wins_ties = 0
    heads_up = 0

    for pair in turn_river:
        edited_deck = copy.deepcopy(deck)
        edited_deck.remove(pair[0])
        edited_deck.remove(pair[1])
        board = current_board + list(pair)

        player_cards = player + board
        player_hands_list = list(combinations(player_cards, 5))
        player_hands = []

        for hand in player_hands_list:
            new_hand = Hand(hand[0], hand[1], hand[2], hand[3], hand[4])
            hand_hierarchy(new_hand)
            player_hands.append(new_hand)

        best_player_hand = best_hand(player_hands)
        opponent_cards_list = list(combinations(edited_deck, 2))

        for opponent_cards in opponent_cards_list:
            opponent_cards = list(opponent_cards) + board
            opponent_hands_list = list(combinations(opponent_cards, 5))
            opponent_hands = []

            for hand in opponent_hands_list:
                new_hand = Hand(hand[0], hand[1], hand[2], hand[3], hand[4])
                hand_hierarchy(new_hand)
                opponent_hands.append(new_hand)

            best_opponent_hand = best_hand(opponent_hands)
            best_overall_hand = best_hand([best_player_hand, best_opponent_hand])

            if best_player_hand == best_overall_hand:
                player_wins_ties += 1
                heads_up += 1

            else:
                heads_up += 1

    probability = ((player_wins_ties / heads_up) ** num_opponents) * 100

    return round(probability, 3)

def post_turn(deck: list, current_board: list, player: list, num_opponents: int) -> float:
    """Returns the probabilty of the player winning or tying after the flop."""
    player_wins_ties = 0
    heads_up = 0

    for card in deck:
        edited_deck = copy.deepcopy(deck)
        edited_deck.remove(card)
        board = current_board + [card]

        player_cards = player + board
        player_hands_list = list(combinations(player_cards, 5))
        player_hands = []

        for hand in player_hands_list:
            new_hand = Hand(hand[0], hand[1], hand[2], hand[3], hand[4])
            hand_hierarchy(new_hand)
            player_hands.append(new_hand)

        best_player_hand = best_hand(player_hands)
        opponent_cards_list = list(combinations(edited_deck, 2))

        for opponent_cards in opponent_cards_list:
            opponent_cards = list(opponent_cards) + board
            opponent_hands_list = list(combinations(opponent_cards, 5))
            opponent_hands = []

            for hand in opponent_hands_list:
                new_hand = Hand(hand[0], hand[1], hand[2], hand[3], hand[4])
                hand_hierarchy(new_hand)
                opponent_hands.append(new_hand)

            best_opponent_hand = best_hand(opponent_hands)
            best_overall_hand = best_hand([best_player_hand, best_opponent_hand])

            if best_player_hand == best_overall_hand:
                player_wins_ties += 1
                heads_up += 1

            else:
                heads_up += 1

    probability = ((player_wins_ties / heads_up) ** num_opponents) * 100

    return round(probability, 3)

def post_river(deck: list, board: list, player: list, num_opponents: int) -> float:
    """Returns the probabilty of the player winning or tying after the flop."""
    player_wins_ties = 0
    heads_up = 0

    player_cards = player + board
    player_hands_list = list(combinations(player_cards, 5))
    player_hands = []

    for hand in player_hands_list:
        new_hand = Hand(hand[0], hand[1], hand[2], hand[3], hand[4])
        hand_hierarchy(new_hand)
        player_hands.append(new_hand)

    best_player_hand = best_hand(player_hands)
    opponent_cards_list = list(combinations(deck, 2))

    for opponent_cards in opponent_cards_list:
        opponent_cards = list(opponent_cards) + board
        opponent_hands_list = list(combinations(opponent_cards, 5))
        opponent_hands = []

        for hand in opponent_hands_list:
            new_hand = Hand(hand[0], hand[1], hand[2], hand[3], hand[4])
            hand_hierarchy(new_hand)
            opponent_hands.append(new_hand)

        best_opponent_hand = best_hand(opponent_hands)
        best_overall_hand = best_hand([best_player_hand, best_opponent_hand])

        if best_player_hand == best_overall_hand:
            player_wins_ties += 1
            heads_up += 1

        else:
            heads_up += 1

    probability = ((player_wins_ties / heads_up) ** num_opponents) * 100

    return round(probability, 3)


def main():
    """Main function that returns the probability of the player winning."""
    deck = [Card('2', 'S'), Card('2', 'D'), Card('2', 'C'), Card('2', 'H'),
            Card('3', 'S'), Card('3', 'D'), Card('3', 'C'), Card('3', 'H'),
            Card('4', 'S'), Card('4', 'D'), Card('4', 'C'), Card('4', 'H'),
            Card('5', 'S'), Card('5', 'D'), Card('5', 'C'), Card('5', 'H'),
            Card('6', 'S'), Card('6', 'D'), Card('6', 'C'), Card('6', 'H'), 
            Card('7', 'S'), Card('7', 'D'), Card('7', 'C'), Card('7', 'H'),
            Card('8', 'S'), Card('8', 'D'), Card('8', 'C'), Card('8', 'H'),
            Card('9', 'S'), Card('9', 'D'), Card('9', 'C'), Card('9', 'H'),
            Card('10', 'S'), Card('10', 'D'), Card('10', 'C'), Card('10', 'H'),
            Card('J', 'S'), Card('J', 'D'), Card('J', 'C'), Card('J', 'H'),
            Card('Q', 'S'), Card('Q', 'D'), Card('Q', 'C'), Card('Q', 'H'),
            Card('K', 'S'), Card('K', 'D'), Card('K', 'C'), Card('K', 'H'),
            Card('A', 'S'), Card('A', 'D'), Card('A', 'C'), Card('A', 'H')]
    board = []
    player = []

    for i in range (1, 3):
        card_str = input(f'Enter card {i} dealt to you: ')
        card_list = card_str.split()
        card = Card(card_list[0], card_list[1])
        player.append(card)
        deck.remove(card)

    for i in range(1, 4):
        card_str = input(f'Enter card {i} from the flop: ')
        card_list = card_str.split()
        card = Card(card_list[0], card_list[1])
        board.append(card)
        deck.remove(card)

    num_opponents = int(input('Enter number of opponents left in the hand: '))

    print(f'{post_flop(deck, board, player, num_opponents)}%')

    card_str = input(f'Enter card turn card: ')
    card_list = card_str.split()
    card = Card(card_list[0], card_list[1])
    board.append(card)
    deck.remove(card)

    num_opponents = int(input('Enter number of opponents left in the hand: '))

    print(f'{post_turn(deck, board, player, num_opponents)}%')

    card_str = input(f'Enter card river card: ')
    card_list = card_str.split()
    card = Card(card_list[0], card_list[1])
    board.append(card)
    deck.remove(card)

    num_opponents = int(input('Enter number of opponents left in the hand: '))

    print(f'{post_river(deck, board, player, num_opponents)}%')

if __name__ == '__main__':
    main()
