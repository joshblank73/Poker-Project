from collections import defaultdict
from itertools import combinations
from poker_basics import Card, hand_hierarchy
from typing import Optional

class Hand:
    """Represents a poker hand.

    Attributes:
        player: Player number which the hand belongs to. Board is player 0.
        card1: First Card object in the hand.
        card2: Second Card object in the hand.
        card3: Third Card object in the hand.
        card4: Fourth Card object in the hand.
        card5: Fifth Card object in the hand.
        hierarchy: The hierarchy of the hand.
    """
    def __init__(
            self,
            player: int,
            card1: Card,
            card2: Card,
            card3: Card,
            card4: Card,
            card5: Card,
            hierarchy: Optional[list] = None):
        self.player = player
        self.card1 = card1
        self.card2 = card2
        self.card3 = card3
        self.card4 = card4
        self.card5 = card5
        self.hierarchy = hierarchy

    def __eq__(self, other) -> bool:
        """Returns True if and only if self and other are equal."""
        return (
            isinstance(other, Hand) and
            self.hierarchy == other.hierarchy
        )

    def __lt__(self, other) -> bool:
        """Returns True if and only if self < other."""
        if (isinstance(other, Hand) and
                (self.hierarchy[1] < other.hierarchy[1] or
                 (self.hierarchy[1] == other.hierarchy[1] and
                  self.hierarchy[2] < other.hierarchy[2]))):
            return True

        else:
            return False

    def __repr__(self):
        return '%r %r %r %r %r' % (self.card1, self.card2, self.card3,
                                   self.card4, self.card5)

def card_nums_sort(hand: Hand) -> list:
    """Coverts a hand to a list of card values in sorted numeric form.

    Returns a list of the card values from the hand in sorted numeric form.
    """
    card_vals = [hand.card1.val, hand.card2.val, hand.card3.val,
                 hand.card4.val, hand.card5.val]
    card_nums = []

    for card_val in card_vals:
        if card_val == '10':
            card_nums.append(10)

        elif ord(card_val) >= 65:
            if card_val == 'J':
                card_nums.append(11)

            elif card_val == 'Q':
                card_nums.append(12)

            elif card_val == 'K':
                card_nums.append(13)

            elif card_val == 'A':
                card_nums.append(14)

        else:
            card_nums.append(int(card_val))

    card_nums.sort()

    return card_nums

def best_hands(hands: list[Hand]) -> list[Hand]:
    """Identifies the best hand from a list of hands.

    Returns the best hand.
    """
    best = max(hands)
    best_hands = []

    indices = [i for i, value in enumerate(hands) if value == best]
    for i in indices:
        best_hands.append(hands[i])

    return best_hands

def main():
    """Main function that returns the winning probabilitis for each player."""
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
    player_cards = defaultdict(list)
    board = []

    num_players = int(input('Enter number of players (9 max): '))

    for i in range(1, num_players + 1):
        card1_str = input(f'Enter card 1 for player {i}: ')
        card2_str = input(f'Enter card 2 for player {i}: ')
        card1_list = card1_str.split()
        card1 = Card(card1_list[0], card1_list[1])
        card2_list = card2_str.split()
        card2 = Card(card2_list[0], card2_list[1])
        player_cards[i] = [card1, card2]
        deck.remove(card1)
        deck.remove(card2)

    for i in range(1, 4):
        card_str = input(f'Enter card {i} from the flop: ')
        card_list = card_str.split()
        card = Card(card_list[0], card_list[1])
        board.append(card)
        deck.remove(card)

    players_str = input('Enter each player left in the game: ')
    players_list = players_str.split()
    players_list = [int(i) for i in players_list]
    folds = []

    for i in range(1, num_players + 1):
        if i not in players_list and i not in folds:
            player_cards.pop(i)
            folds.append(i)

    total_count = 0
    player_count = defaultdict(int)
    player_percent = defaultdict(float)
    turn_river = list(combinations(deck, 2))
    for key in players_list:
        player_count[key] = 0

    for cards in turn_river:
        hands = []
        board_hand = Hand(0, board[0], board[1], board[2], cards[0], cards[1])
        hand_hierarchy(board_hand)
        hands.append(board_hand)

        for key in player_cards:
            all_cards = [board[0], board[1], board[2], cards[0], cards[1],
                         player_cards[key][0], player_cards[key][1]]
            player_hands_list = list(combinations(all_cards, 5))
            player_hands = []

            for card_list in player_hands_list:
                if card_list != [board[0], board[1], board[2], cards[0],
                                 cards[1]]:
                    hand = Hand(key, card_list[0], card_list[1], card_list[2],
                                card_list[3], card_list[4])
                    hand_hierarchy(hand)
                    player_hands.append(hand)

            best = best_hands(player_hands)
            hands.append(best[0])

        best = best_hands(hands)
        total_count += 1

        str_key = ''
        if len(best) > 1:
            for i in range(len(best)):
                str_key += str(best[i].player)
            player_count[str_key] += 1

        else:
            player_count[best[0].player] += 1

    for key in player_count:
        player_percent[key] = (player_count[key] / total_count) * 100
        if isinstance(key, str):
            lst = [*key]

            if lst[0] == '0':
                print(f'Split Pot Between Everyone: {round(player_percent[key], 3)}%')

            elif len(lst) == 2:
                print(f'Split Pot between Players {lst[0]} & {lst[1]}: {round(player_percent[key], 3)}%')

            elif len(lst) == 3:
                print(f'Split Pot between Players {lst[0]}, {lst[1]}, & {lst[2]}: {round(player_percent[key], 3)}%')

            elif len(lst) == 4:
                print(f'Split Pot between Players {lst[0]}, {lst[1]}, {lst[2]}, & {lst[3]}: {round(player_percent[key], 3)}%')

        else:
            print(f'Player {key}: {round(player_percent[key], 3)}%')

    turn_str = input('Enter turn card: ')
    turn_list = turn_str.split()
    turn = Card(turn_list[0], turn_list[1])
    board.append(turn)
    deck.remove(turn)

    players_str = input('Enter each player left in the game: ')
    players_list = players_str.split()
    players_list = [int(i) for i in players_list]

    for i in range(1, num_players + 1):
        if i not in players_list and i not in folds:
            player_cards.pop(i)
            folds.append(i)

    total_count = 0
    player_count = defaultdict(int)
    player_percent = defaultdict(float)
    for key in players_list:
        player_count[key] = 0

    for card in deck:
        hands = []
        board_hand = Hand(0, board[0], board[1], board[2], board[3], card)
        hand_hierarchy(board_hand)
        hands.append(board_hand)

        for key in player_cards:
            all_cards = [board[0], board[1], board[2], board[3], card,
                         player_cards[key][0], player_cards[key][1]]
            player_hands_list = list(combinations(all_cards, 5))
            player_hands = []

            for card_list in player_hands_list:
                if card_list != [board[0], board[1], board[2], board[3],
                                 card]:
                    hand = Hand(key, card_list[0], card_list[1], card_list[2],
                                card_list[3], card_list[4])
                    hand_hierarchy(hand)
                    player_hands.append(hand)

            best = best_hands(player_hands)
            hands.append(best[0])

        best = best_hands(hands)
        total_count += 1

        str_key = ''
        if len(best) > 1:
            for i in range(len(best)):
                str_key += str(best[i].player)
            player_count[str_key] += 1

        else:
            player_count[best[0].player] += 1

    for key in player_count:
        player_percent[key] = (player_count[key] / total_count) * 100
        if isinstance(key, str):
            lst = [*key]

            if lst[0] == '0':
                print(f'Split Pot Between Everyone: {round(player_percent[key], 3)}%')

            elif len(lst) == 2:
                print(f'Split Pot between Players {lst[0]} & {lst[1]}: {round(player_percent[key], 3)}%')

            elif len(lst) == 3:
                print(f'Split Pot between Players {lst[0]}, {lst[1]}, & {lst[2]}: {round(player_percent[key], 3)}%')

            elif len(lst) == 4:
                print(f'Split Pot between Players {lst[0]}, {lst[1]}, {lst[2]}, & {lst[3]}: {round(player_percent[key], 3)}%')

        else:
            print(f'Player {key}: {round(player_percent[key], 3)}%')

    river_str = input('Enter river card: ')
    river_list = river_str.split()
    river = Card(river_list[0], river_list[1])
    board.append(river)
    deck.remove(river)

    players_str = input('Enter each player left in the game: ')
    players_list = players_str.split()
    players_list = [int(i) for i in players_list]

    for i in range(1, num_players + 1):
        if i not in players_list and i not in folds:
            player_cards.pop(i)
            folds.append(i)

    hands = []
    player_win = defaultdict(bool)
    for key in players_list:
        player_win[key] = False

    board_hand = Hand(0, board[0], board[1], board[2], board[3], board[4])
    hand_hierarchy(board_hand)
    hands.append(board_hand)

    for key in player_cards:
        all_cards = [board[0], board[1], board[2], board[3], board[4],
                     player_cards[key][0], player_cards[key][1]]
        player_hands_list = list(combinations(all_cards, 5))
        player_hands = []

        for card_list in player_hands_list:
            if card_list != [board[0], board[1], board[2], board[3],
                             board[4]]:
                hand = Hand(key, card_list[0], card_list[1], card_list[2],
                            card_list[3], card_list[4])
                hand_hierarchy(hand)
                player_hands.append(hand)

        best = best_hands(player_hands)
        hands.append(best[0])

    best = best_hands(hands)

    str_key = ''
    if len(best) > 1:
        for i in range(len(best)):
            str_key += str(best[i].player)
        player_win[str_key] = True

    else:
        player_win[best[0].player] = True

    for key in player_win:
        if isinstance(key, str):
            lst = [*key]

            if lst[0] == '0':
                print(f'Split Pot Between Everyone: {round(player_percent[key], 3)}%')

            elif len(lst) == 2:
                print(f'Split Pot between Players {lst[0]} & {lst[1]}: 100%')

            elif len(lst) == 3:
                print(f'Split Pot between Players {lst[0]}, {lst[1]}, & {lst[2]}: 100%')

            elif len(lst) == 4:
                print(f'Split Pot between Players {lst[0]}, {lst[1]}, {lst[2]}, & {lst[3]}: 100%')

        else:
            if player_win[key] == False:
                print(f'Player {key}: 0%')

            elif player_win[key] == True:
                print(f'Player {key}: 100%')

if __name__ == '__main__':
    main()
