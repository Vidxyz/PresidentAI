package utils

import game.FaceValue._
import game.Suits._
import game.{Card, Hand, Joker, NormalCard, SpecialCard, Value, WildCard}
import javax.swing.ImageIcon
import scala.swing.SimpleSwingApplication

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

  def getImageResource(card: Card, app: SimpleSwingApplication): ImageIcon = {
    card match {
      case THREE_Diamond => new ImageIcon(app.resourceFromClassloader("images/3_of_diamonds"))
      case THREE_Club => new ImageIcon(app.resourceFromClassloader("images/3_of_clubs"))
      case THREE_Heart => new ImageIcon(app.resourceFromClassloader("images/3_of_hearts"))
      case THREE_Spade => new ImageIcon(app.resourceFromClassloader("images/3_of_spades"))
      case FOUR_Diamond => new ImageIcon(app.resourceFromClassloader("images/4_of_diamonds"))
      case FOUR_Club => new ImageIcon(app.resourceFromClassloader("images/4_of_clubs"))
      case FOUR_Heart => new ImageIcon(app.resourceFromClassloader("images/4_of_hearts"))
      case FOUR_Spade => new ImageIcon(app.resourceFromClassloader("images/4_of_spades"))
      case FIVE_Diamond => new ImageIcon(app.resourceFromClassloader("images/5_of_diamonds"))
      case FIVE_Club => new ImageIcon(app.resourceFromClassloader("images/5_of_clubs"))
      case FIVE_Heart => new ImageIcon(app.resourceFromClassloader("images/5_of_hearts"))
      case FIVE_Spade => new ImageIcon(app.resourceFromClassloader("images/5_of_spades"))
      case SIX_Diamond => new ImageIcon(app.resourceFromClassloader("images/6_of_diamonds"))
      case SIX_Club => new ImageIcon(app.resourceFromClassloader("images/6_of_clubs"))
      case SIX_Heart => new ImageIcon(app.resourceFromClassloader("images/6_of_hearts"))
      case SIX_Spade => new ImageIcon(app.resourceFromClassloader("images/6_of_spades"))
      case SEVEN_Diamond => new ImageIcon(app.resourceFromClassloader("images/7_of_diamonds"))
      case SEVEN_Club => new ImageIcon(app.resourceFromClassloader("images/7_of_clubs"))
      case SEVEN_Heart => new ImageIcon(app.resourceFromClassloader("images/7_of_hearts"))
      case SEVEN_Spade => new ImageIcon(app.resourceFromClassloader("images/7_of_spades"))
      case EIGHT_Diamond => new ImageIcon(app.resourceFromClassloader("images/8_of_diamonds"))
      case EIGHT_Club => new ImageIcon(app.resourceFromClassloader("images/8_of_clubs"))
      case EIGHT_Heart => new ImageIcon(app.resourceFromClassloader("images/8_of_hearts"))
      case EIGHT_Spade => new ImageIcon(app.resourceFromClassloader("images/8_of_spades"))
      case NINE_Diamond => new ImageIcon(app.resourceFromClassloader("images/9_of_diamonds"))
      case NINE_Club => new ImageIcon(app.resourceFromClassloader("images/9_of_clubs"))
      case NINE_Heart => new ImageIcon(app.resourceFromClassloader("images/9_of_hearts"))
      case NINE_Spade => new ImageIcon(app.resourceFromClassloader("images/9_of_spades"))
      case TEN_Diamond => new ImageIcon(app.resourceFromClassloader("images/10_of_diamonds"))
      case TEN_Club => new ImageIcon(app.resourceFromClassloader("images/10_of_clubs"))
      case TEN_Heart => new ImageIcon(app.resourceFromClassloader("images/10_of_hearts"))
      case TEN_Spade => new ImageIcon(app.resourceFromClassloader("images/10_of_spades"))
      case JACK_Diamond => new ImageIcon(app.resourceFromClassloader("images/jack_of_diamonds"))
      case JACK_Club => new ImageIcon(app.resourceFromClassloader("images/jack_of_clubs"))
      case JACK_Heart => new ImageIcon(app.resourceFromClassloader("images/jack_of_hearts"))
      case JACK_Spade => new ImageIcon(app.resourceFromClassloader("images/jack_of_spades"))
      case QUEEN_Diamond => new ImageIcon(app.resourceFromClassloader("images/queen_of_diamonds"))
      case QUEEN_Club => new ImageIcon(app.resourceFromClassloader("images/queen_of_clubs"))
      case QUEEN_Heart => new ImageIcon(app.resourceFromClassloader("images/queen_of_hearts"))
      case QUEEN_Spade => new ImageIcon(app.resourceFromClassloader("images/queen_of_spades"))
      case KING_Diamond => new ImageIcon(app.resourceFromClassloader("images/king_of_diamonds"))
      case KING_Club => new ImageIcon(app.resourceFromClassloader("images/king_of_clubs"))
      case KING_Heart => new ImageIcon(app.resourceFromClassloader("images/king_of_hearts"))
      case KING_Spade => new ImageIcon(app.resourceFromClassloader("images/king_of_spades"))
      case ACE_Diamond => new ImageIcon(app.resourceFromClassloader("images/ace_of_diamonds"))
      case ACE_Club => new ImageIcon(app.resourceFromClassloader("images/ace_of_clubs"))
      case ACE_Heart => new ImageIcon(app.resourceFromClassloader("images/ace_of_hearts"))
      case ACE_Spade => new ImageIcon(app.resourceFromClassloader("images/ace_of_spades"))
      case TWO_Diamond => new ImageIcon(app.resourceFromClassloader("images/two_of_diamonds"))
      case TWO_Club => new ImageIcon(app.resourceFromClassloader("images/two_of_clubs"))
      case TWO_Heart => new ImageIcon(app.resourceFromClassloader("images/two_of_hearts"))
      case TWO_Spade => new ImageIcon(app.resourceFromClassloader("images/two_of_spades"))
      case Joker => new ImageIcon(app.resourceFromClassloader("images/black_joker"))
    }
  }

  /*
  These are all shorthand method for defining cards
  Aids in clarity of code later
   */
  def THREE_Diamond: WildCard = WildCard(THREE, Diamond)
  def THREE_Club: WildCard = WildCard(THREE, Club)
  def THREE_Heart: WildCard = WildCard(THREE, Heart)
  def THREE_Spade: WildCard = WildCard(THREE, Spade)

  def THREE_Diamond(assumedValue: Int): WildCard = WildCard(THREE, Diamond, assumedValue)
  def THREE_Club(assumedValue: Int): WildCard = WildCard(THREE, Club, assumedValue)
  def THREE_Heart(assumedValue: Int): WildCard = WildCard(THREE, Heart, assumedValue)
  def THREE_Spade(assumedValue: Int): WildCard = WildCard(THREE, Spade, assumedValue)

  def FOUR_Diamond: NormalCard = NormalCard(FOUR, Diamond)
  def FOUR_Club: NormalCard = NormalCard(FOUR, Club)
  def FOUR_Heart: NormalCard = NormalCard(FOUR, Heart)
  def FOUR_Spade: NormalCard = NormalCard(FOUR, Spade)

  def FIVE_Diamond: NormalCard = NormalCard(FIVE, Diamond)
  def FIVE_Club: NormalCard = NormalCard(FIVE, Club)
  def FIVE_Heart: NormalCard = NormalCard(FIVE, Heart)
  def FIVE_Spade: NormalCard = NormalCard(FIVE, Spade)

  def SIX_Diamond: NormalCard = NormalCard(SIX, Diamond)
  def SIX_Club: NormalCard = NormalCard(SIX, Club)
  def SIX_Heart: NormalCard = NormalCard(SIX, Heart)
  def SIX_Spade: NormalCard = NormalCard(SIX, Spade)

  def SEVEN_Diamond: NormalCard = NormalCard(SEVEN, Diamond)
  def SEVEN_Club: NormalCard = NormalCard(SEVEN, Club)
  def SEVEN_Heart: NormalCard = NormalCard(SEVEN, Heart)
  def SEVEN_Spade: NormalCard = NormalCard(SEVEN, Spade)

  def EIGHT_Diamond: NormalCard = NormalCard(EIGHT, Diamond)
  def EIGHT_Club: NormalCard = NormalCard(EIGHT, Club)
  def EIGHT_Heart: NormalCard = NormalCard(EIGHT, Heart)
  def EIGHT_Spade: NormalCard = NormalCard(EIGHT, Spade)

  def NINE_Diamond: NormalCard = NormalCard(NINE, Diamond)
  def NINE_Club: NormalCard = NormalCard(NINE, Club)
  def NINE_Heart: NormalCard = NormalCard(NINE, Heart)
  def NINE_Spade: NormalCard = NormalCard(NINE, Spade)

  def TEN_Diamond: NormalCard = NormalCard(TEN, Diamond)
  def TEN_Club: NormalCard = NormalCard(TEN, Club)
  def TEN_Heart: NormalCard = NormalCard(TEN, Heart)
  def TEN_Spade: NormalCard = NormalCard(TEN, Spade)

  def JACK_Diamond: NormalCard = NormalCard(JACK, Diamond)
  def JACK_Club: NormalCard = NormalCard(JACK, Club)
  def JACK_Heart: NormalCard = NormalCard(JACK, Heart)
  def JACK_Spade: NormalCard = NormalCard(JACK, Spade)

  def QUEEN_Diamond: NormalCard = NormalCard(QUEEN, Diamond)
  def QUEEN_Club: NormalCard = NormalCard(QUEEN, Club)
  def QUEEN_Heart: NormalCard = NormalCard(QUEEN, Heart)
  def QUEEN_Spade: NormalCard = NormalCard(QUEEN, Spade)

  def KING_Diamond: NormalCard = NormalCard(KING, Diamond)
  def KING_Club: NormalCard = NormalCard(KING, Club)
  def KING_Heart: NormalCard = NormalCard(KING, Heart)
  def KING_Spade: NormalCard = NormalCard(KING, Spade)

  def ACE_Diamond: NormalCard = NormalCard(ACE, Diamond)
  def ACE_Club: NormalCard = NormalCard(ACE, Club)
  def ACE_Heart: NormalCard = NormalCard(ACE, Heart)
  def ACE_Spade: NormalCard = NormalCard(ACE, Spade)

  def TWO_Diamond: SpecialCard = SpecialCard(TWO, Diamond)
  def TWO_Club: SpecialCard = SpecialCard(TWO, Club)
  def TWO_Heart: SpecialCard = SpecialCard(TWO, Heart)
  def TWO_Spade: SpecialCard = SpecialCard(TWO, Spade)

}
