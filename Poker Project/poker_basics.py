from typing import Optional
from math import comb

class Card:
    """Represents a poker card.

    Attributes:
        val: Value of card (A, 2, 3,...K)
        suit: Suit of card (S, D, C, H)
    """
    def __init__(self, val: str, suit: str):
        self.val = val
        self.suit = suit

    def __eq__(self, other) -> bool:
        """Returns True if and only if self and other are equal."""
        return (
            isinstance(other, Card) and
            self.val == other.val and
            self.suit == other.suit
        )

    def __repr__(self):
        if self.suit == 'S':
            symbol = '\u2660'
        elif self.suit == 'D':
            symbol = '\u2666'
        elif self.suit == 'C':
            symbol = '\u2663'
        elif self.suit == 'H':
            symbol = '\u2665'

        return '%r%r' % (self.val, symbol)

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
            card1: Card,
            card2: Card,
            card3: Card,
            card4: Card,
            card5: Card,
            hierarchy: Optional[list] = None):
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

def hand_hierarchy(hand: Hand) -> None:
    """Identifies the hierarchy of the given hand."""
    card_nums = card_nums_sort(hand)
    card_set = list(set(card_nums))
    card_set.sort()
    max_card = card_nums[4]
    min_card = card_nums[0]

    if len(card_set) == 5 and (max_card - min_card == 4 or\
            card_nums == [2, 3, 4, 5, 14]) and\
            hand.card1.suit == hand.card2.suit == hand.card3.suit ==\
            hand.card4.suit == hand.card5.suit:
        if card_nums == [10, 11, 12, 13, 14]:
            hand.hierarchy = ['Royal Flush', 9, 0]

        else:
            straights = [[2, 3, 4, 5, 14], [2, 3, 4, 5, 6], [3, 4, 5, 6, 7],
                         [4, 5, 6, 7, 8], [5, 6, 7, 8, 9], [6, 7, 8, 9, 10],
                         [7, 8, 9, 10, 11], [8, 9, 10, 11, 12],
                         [9, 10, 11, 12, 13]]

            for i in range(len(straights)):
                if card_nums == straights[i]:
                    hand.hierarchy = ['Straight Flush', 8, i]

    elif len(card_set) == 5 and (max_card - min_card == 4 or\
            card_nums == [2, 3, 4, 5, 14]):
        straights = [[2, 3, 4, 5, 14], [2, 3, 4, 5, 6], [3, 4, 5, 6, 7],
                     [4, 5, 6, 7, 8], [5, 6, 7, 8, 9], [6, 7, 8, 9, 10],
                     [7, 8, 9, 10, 11], [8, 9, 10, 11, 12],
                     [9, 10, 11, 12, 13], [10, 11, 12, 13, 14]]

        for i in range(len(straights)):
            if card_nums == straights[i]:
                hand.hierarchy = ['Straight', 4, i]

    elif hand.card1.suit == hand.card2.suit == hand.card3.suit ==\
            hand.card4.suit == hand.card5.suit:
        max_list = [7, 8, 9, 10, 11, 12, 13, 14]

        for i in range(len(max_list)):
            if max_card == max_list[i]:
                hand.hierarchy = ['Flush', 5, i]

    elif len(card_set) == 2:
        if card_nums.count(card_set[0]) == 4:
            val0 = (card_set[0] - 2) * 12
            val1 = card_set[1] - 3
            hand.hierarchy = ['Four of a Kind', 7, val0 + val1]

        elif card_nums.count(card_set[0]) == 1:
            val0 = card_set[1] - 2
            val1 = (card_set[0] - 2) * 12
            hand.hierarchy = ['Four of a Kind', 7, val0 + val1]

        else:
            if card_nums.count(card_set[0]) == 3:
                for i in range(2, 14):
                    for j in range(i + 1, 15):
                        if card_set[0] == i and card_set[1] == j:
                            hand.hierarchy = ['Full House', 6,
                                              (i - 2) * 12 + j - 3]

            else:
                for i in range(3, 15):
                    for j in range(2, i):
                        if card_set[0] == j and card_set[1] == i:
                            hand.hierarchy = ['Full House', 6,
                                              (i - 2) * 12 + j - 2]

    elif len(card_set) == 3:
        if (card_nums.count(card_set[0]) != 3 and
                card_nums.count(card_set[1]) != 3 and
                card_nums.count(card_set[2]) != 3):
            if card_nums.count(card_set[2]) == 1:
                val0 = (card_set[0] - 2) * 11
                val1 = 0
                for i in range(3, card_set[1]):
                    val1 += (i - 2) * 11
                val2 = card_set[2] - 4

                hand.hierarchy = ['Two Pair', 2, val0 + val1 + val2]

            elif card_nums.count(card_set[1]) == 1:
                val0 = (card_set[0] - 2) * 11
                val1 = card_set[1] - 3
                val2 = 0
                for i in range(3, card_set[2]):
                    val2 += (i - 2) * 11

                hand.hierarchy = ['Two Pair', 2, val0 + val1 + val2]

            elif card_nums.count(card_set[0]) == 1:
                val0 = card_set[0] - 2
                val1 = (card_set[1] - 2) * 11
                val2 = 0
                for i in range(3, card_set[2]):
                    val2 += (i - 2) * 11

                hand.hierarchy = ['Two Pair', 2, val0 + val1 + val2]

        else:
            if card_nums.count(card_set[0]) == 3:
                val0 = (card_set[0] - 2) * 66
                val1 = card_set[1] - 3
                val2 = 0
                for i in range(4, card_set[2]):
                    val2 += i - 3

                hand.hierarchy = ['Three of a Kind', 3, val0 + val1 + val2]

            elif card_nums.count(card_set[1]) == 3:
                val0 = card_set[0] - 2
                val1 = (card_set[1] - 2) * 66
                val2 = 0
                for i in range(4, card_set[2]):
                    val2 += i - 3

                hand.hierarchy = ['Three of a Kind', 3, val0 + val1 + val2]

            elif card_nums.count(card_set[2]) == 3:
                val0 = card_set[0] - 2
                val1 = 0
                for i in range(3, card_set[1]):
                    val1 += i - 2
                val2 = (card_set[2] - 2) * 66

                hand.hierarchy = ['Three of a Kind', 3, val0 + val1 + val2]

    elif len(card_set) == 4:
        if card_nums.count(card_set[0]) == 2:
            val0 = (card_set[0] - 2) * 220
            val1 = card_set[1] - 3
            val2 = comb(card_set[2] - 3, 2)
            val3 = 0
            for i in range(5, card_set[3]):
                val3 += comb(i - 3, 2)

            hand.hierarchy = ['Pair', 1, val0 + val1 + val2 + val3]

        elif card_nums.count(card_set[1]) == 2:
            val0 = card_set[0] - 2
            val1 = (card_set[1] - 2) * 220
            val2 = comb(card_set[2] - 3, 2)
            val3 = 0
            for i in range(5, card_set[3]):
                val3 += comb(i - 3, 2)

            hand.hierarchy = ['Pair', 1, val0 + val1 + val2 + val3]

        elif card_nums.count(card_set[2]) == 2:
            val0 = card_set[0] - 2
            val1 = comb(card_set[1] - 2, 2)
            val2 = (card_set[2] - 2) * 220
            val3 = 0
            for i in range(5, card_set[3]):
                val3 += comb(i - 3, 2)

            hand.hierarchy = ['Pair', 1, val0 + val1 + val2 + val3]

        elif card_nums.count(card_set[3]) == 2:
            val0 = card_set[0] - 2
            val1 = comb(card_set[1] - 2, 2)
            val2 = 0
            for i in range(4, card_set[2]):
                val2 += comb(i - 2, 2)
            val3 = (card_set[3] - 2) * 220

            hand.hierarchy = ['Pair', 1, val0 + val1 + val2 + val3]

    else:
        val0 = card_nums[0] - 2
        val1 = 0
        for i in range(3, card_nums[1]):
            val1 += comb(i - 2, 1)
        val2 = 0
        for i in range(4, card_nums[2]):
            val2 += comb(i - 2, 2)
        val3 = 0
        for i in range(5, card_nums[3]):
            val3 += comb(i - 2, 3)
        val4 = 0
        for i in range(6, card_nums[4]):
            val4 += comb(i - 2, 4)

        hand.hierarchy = ['High Card', 0, val0 + val1 + val2 + val3 + val4]
