import game.{Hand, NormalCard, WildCard}
import org.scalatest.FunSpec
import game.FaceValue._
import game.Suits._

class GameEssentialsTest extends FunSpec {

  describe("Tests for Hand case class") {

    describe("Tests for weaknessFactor"){

      it("should return a weaknessFactor of 0 when hand is of size 1") {
        val hand = Hand(List(NormalCard(FOUR, Diamond)))
        assert(hand.weaknessFactor == 0)
      }

      it("should return a weaknessFactor of 1") {
        val hand = Hand(List(
          NormalCard(FOUR, Diamond),
          NormalCard(FIVE, Club), NormalCard(FIVE, Heart),
          NormalCard(SIX, Diamond),
          NormalCard(SEVEN, Diamond), NormalCard(SEVEN, Club), NormalCard(SEVEN, Heart), NormalCard(SEVEN, Spade),
          NormalCard(EIGHT, Diamond),
          NormalCard(NINE, Diamond), NormalCard(NINE, Heart), NormalCard(NINE, Spade)))
        assert(hand.weaknessFactor == 1)
      }

      it("should return a weaknessFactor of 3") {
        val hand = Hand(List(
          WildCard(THREE, Diamond),
          NormalCard(FOUR, Club), NormalCard(FOUR, Spade),
          NormalCard(FIVE, Club),
          NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Spade),
          NormalCard(JACK, Club),
          NormalCard(QUEEN, Diamond),
          NormalCard(KING, Diamond), NormalCard(KING, Heart), NormalCard(KING, Spade),
        ))
        assert(hand.weaknessFactor == 3)
      }

      it("should return a weaknessFactor of 10 and not include threes in the calculation") {
        val hand = Hand(List(
          WildCard(THREE, Diamond), WildCard(THREE, Club), WildCard(THREE, Spade),
          NormalCard(FOUR, Diamond), NormalCard(FOUR, Club), NormalCard(FOUR, Spade),
          NormalCard(ACE, Diamond), NormalCard(ACE, Heart), NormalCard(ACE, Spade)
        ))
        assert(hand.weaknessFactor == 10)
      }

    }
  }
}
