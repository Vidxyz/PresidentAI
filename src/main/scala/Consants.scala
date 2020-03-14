import FaceValue.{ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, JACK, QUEEN, KING}
import Suits.{Diamond, Club, Spade, Heart}

object Consants {

  val numberToCardMap = Map(
    0 -> NormalCard(THREE, Diamond),
    1 -> NormalCard(THREE, Club),
    2 -> NormalCard(THREE, Heart),
    3 -> NormalCard(THREE, Spade),

    4 -> NormalCard(FOUR, Diamond),
    5 -> NormalCard(FOUR, Club),
    6 -> NormalCard(FOUR, Heart),
    7 -> NormalCard(FOUR, Spade),

    8 -> NormalCard(FIVE, Diamond),
    9 -> NormalCard(FIVE, Club),
    10 -> NormalCard(FIVE, Heart),
    11 -> NormalCard(FIVE, Spade),

    12 -> NormalCard(SIX, Diamond),
    13 -> NormalCard(SIX, Club),
    14 -> NormalCard(SIX, Heart),
    15 -> NormalCard(SIX, Spade),

    16 -> NormalCard(SEVEN, Diamond),
    17 -> NormalCard(SEVEN, Club),
    18 -> NormalCard(SEVEN, Heart),
    19 -> NormalCard(SEVEN, Spade),

    20 -> NormalCard(EIGHT, Diamond),
    21 -> NormalCard(EIGHT, Club),
    22 -> NormalCard(EIGHT, Heart),
    23 -> NormalCard(EIGHT, Spade),

    24 -> NormalCard(NINE, Diamond),
    25 -> NormalCard(NINE, Club),
    26 -> NormalCard(NINE, Heart),
    27 -> NormalCard(NINE, Spade),

    28 -> NormalCard(TEN, Diamond),
    29 -> NormalCard(TEN, Club),
    30 -> NormalCard(TEN, Heart),
    31 -> NormalCard(TEN, Spade),

    32 -> NormalCard(JACK, Diamond),
    33 -> NormalCard(JACK, Club),
    34 -> NormalCard(JACK, Heart),
    35 -> NormalCard(JACK, Spade),

    36 -> NormalCard(QUEEN, Diamond),
    37 -> NormalCard(QUEEN, Club),
    38 -> NormalCard(QUEEN, Heart),
    39 -> NormalCard(QUEEN, Spade),

    40 -> NormalCard(KING, Diamond),
    41 -> NormalCard(KING, Club),
    42 -> NormalCard(KING, Heart),
    43 -> NormalCard(KING, Spade),

    44 -> NormalCard(ACE, Diamond),
    45 -> NormalCard(ACE, Club),
    46 -> NormalCard(ACE, Heart),
    47 -> NormalCard(ACE, Spade),

    48 -> NormalCard(TWO, Diamond),
    49 -> NormalCard(TWO, Club),
    50 -> NormalCard(TWO, Heart),
    51 -> NormalCard(TWO, Spade),

    52 -> Joker,
    53 -> Joker
  )
}
