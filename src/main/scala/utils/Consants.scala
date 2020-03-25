package utils

import game.FaceValue._
import game.Suits._
import game.{Card, Hand, Joker, NormalCard, SpecialCard, Value, WildCard}

object Consants {

  val totalNumberOfCards = 54

  val maxMoveSize = 4

  val numberToCardMap: Map[Int, Card] = Map(
    0 -> WildCard(THREE, Diamond),
    1 -> WildCard(THREE, Club),
    2 -> WildCard(THREE, Heart),
    3 -> WildCard(THREE, Spade),

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

    48 -> SpecialCard(TWO, Diamond),
    49 -> SpecialCard(TWO, Club),
    50 -> SpecialCard(TWO, Heart),
    51 -> SpecialCard(TWO, Spade),

    52 -> Joker,
    53 -> Joker
  )

  val sortedHandWithAllCards: Hand = Hand(List(
    WildCard(THREE, Diamond),
    WildCard(THREE, Club),
    WildCard(THREE, Heart),
    WildCard(THREE, Spade),
    NormalCard(FOUR, Diamond),
    NormalCard(FOUR, Club),
    NormalCard(FOUR, Heart),
    NormalCard(FOUR, Spade),
    NormalCard(FIVE, Diamond),
    NormalCard(FIVE, Club),
    NormalCard(FIVE, Heart),
    NormalCard(FIVE, Spade),
    NormalCard(SIX, Diamond),
    NormalCard(SIX, Club),
    NormalCard(SIX, Heart),
    NormalCard(SIX, Spade),
    NormalCard(SEVEN, Diamond),
    NormalCard(SEVEN, Club),
    NormalCard(SEVEN, Heart),
    NormalCard(SEVEN, Spade),
    NormalCard(EIGHT, Diamond),
    NormalCard(EIGHT, Club),
    NormalCard(EIGHT, Heart),
    NormalCard(EIGHT, Spade),
    NormalCard(NINE, Diamond),
    NormalCard(NINE, Club),
    NormalCard(NINE, Heart),
    NormalCard(NINE, Spade),
    NormalCard(TEN, Diamond),
    NormalCard(TEN, Club),
    NormalCard(TEN, Heart),
    NormalCard(TEN, Spade),
    NormalCard(JACK, Diamond),
    NormalCard(JACK, Club),
    NormalCard(JACK, Heart),
    NormalCard(JACK, Spade),
    NormalCard(QUEEN, Diamond),
    NormalCard(QUEEN, Club),
    NormalCard(QUEEN, Heart),
    NormalCard(QUEEN, Spade),
    NormalCard(KING, Diamond),
    NormalCard(KING, Club),
    NormalCard(KING, Heart),
    NormalCard(KING, Spade),
    NormalCard(ACE, Diamond),
    NormalCard(ACE, Club),
    NormalCard(ACE, Heart),
    NormalCard(ACE, Spade),
    SpecialCard(TWO, Diamond),
    SpecialCard(TWO, Club),
    SpecialCard(TWO, Heart),
    SpecialCard(TWO, Spade),
    Joker,
    Joker,
  ))

  val numberToFaceValueMap: Map[Int, Value] = Map(
    4 -> FOUR,
    5 -> FIVE,
    6 -> SIX,
    7 -> SEVEN,
    8 -> EIGHT,
    9 -> NINE,
    10 -> TEN,
    11-> JACK,
    12 -> QUEEN,
    13 -> KING,
    14 -> ACE
  )

}
