import FaceValue.{ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, JACK, QUEEN, KING}
import Suits.{Diamond, Club, Spade, Heart}

object Consants {

  val numberToCardMap = Map(
    0 -> NormalCard(ACE, Diamond),
    1 -> NormalCard(TWO, Diamond),
    2 -> NormalCard(THREE, Diamond),
    3 -> NormalCard(FOUR, Diamond),
    4 -> NormalCard(FIVE, Diamond),
    5 -> NormalCard(SIX, Diamond),
    6 -> NormalCard(SEVEN, Diamond),
    7 -> NormalCard(EIGHT, Diamond),
    8 -> NormalCard(NINE, Diamond),
    9 -> NormalCard(TEN, Diamond),
    10 -> NormalCard(JACK, Diamond),
    11 -> NormalCard(QUEEN, Diamond),
    12 -> NormalCard(KING, Diamond),

    13 -> NormalCard(ACE, Club),
    14 -> NormalCard(TWO, Club),
    15 -> NormalCard(THREE, Club),
    16 -> NormalCard(FOUR, Club),
    17 -> NormalCard(FIVE, Club),
    18 -> NormalCard(SIX, Club),
    19 -> NormalCard(SEVEN, Club),
    20 -> NormalCard(EIGHT, Club),
    21 -> NormalCard(NINE, Club),
    22 -> NormalCard(TEN, Club),
    23 -> NormalCard(JACK, Club),
    24 -> NormalCard(QUEEN, Club),
    25 -> NormalCard(KING, Club),

    26 -> NormalCard(ACE, Heart),
    27-> NormalCard(TWO, Heart),
    28 -> NormalCard(THREE, Heart),
    29 -> NormalCard(FOUR, Heart),
    30 -> NormalCard(FIVE, Heart),
    31 -> NormalCard(SIX, Heart),
    32 -> NormalCard(SEVEN, Heart),
    33 -> NormalCard(EIGHT, Heart),
    34 -> NormalCard(NINE, Heart),
    35 -> NormalCard(TEN, Heart),
    36 -> NormalCard(JACK, Heart),
    37 -> NormalCard(QUEEN, Heart),
    38 -> NormalCard(KING, Heart),

    39 -> NormalCard(ACE, Spade),
    40 -> NormalCard(TWO, Spade),
    41 -> NormalCard(THREE, Spade),
    42 -> NormalCard(FOUR, Spade),
    43 -> NormalCard(FIVE, Spade),
    44 -> NormalCard(SIX, Spade),
    45 -> NormalCard(SEVEN, Spade),
    46 -> NormalCard(EIGHT, Spade),
    47 -> NormalCard(NINE, Spade),
    48 -> NormalCard(TEN, Spade),
    49 -> NormalCard(JACK, Spade),
    50 -> NormalCard(QUEEN, Spade),
    51 -> NormalCard(KING, Spade),

    52 -> Joker,
    53 -> Joker
  )

}
