import game.{Hand, Joker, Move, NormalCard, WildCard}
import org.scalatest.FunSpec
import game.FaceValue._
import game.Suits._
import utils.Consants._

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



  describe("Tests for Move case class") {

    describe("Tests for highestCard()") {

      describe("When move size is 1") {
        it("Returns the only card in hand") {
          val move = Move(List(NINE_Spade))
          val move2 = Move(List(THREE_Diamond(14)))
          val move3 = Move(List(TWO_Spade))
          val move4 = Move(List(Joker))
          assert(move.highestCard == NINE_Spade)
          assert(move2.highestCard == THREE_Diamond(14))
          assert(move3.highestCard == TWO_Spade)
          assert(move4.highestCard == Joker)
        }
      }

      describe("When move size is 4") {
        describe("When highest card is a WildCard assuming a value") {
          it("Should return the WildCard") {
            val move = Move(List(THREE_Spade(10), TEN_Diamond, TEN_Club, TEN_Heart))
            assert(move.highestCard == THREE_Spade(10))
          }
        }

        describe("When highest card is a NormalCard") {
          it("Should return the NormalCard") {
            val move = Move(List(TEN_Diamond, TEN_Club, TEN_Heart, TEN_Spade))
            assert(move.highestCard == TEN_Spade)
          }
        }

        describe("When highest card is a SpecialCard") {
          it("Should return the SpecialCard") {
            val move = Move(List(TWO_Diamond, TWO_Club, TWO_Heart, TWO_Spade))
            assert(move.highestCard == TWO_Spade)
          }
        }


        describe("When highest card is a tie between a WildCard and a NormalCard") {
          it("Should return the NormalCard") {
            val move = Move(List(THREE_Spade(10), TEN_Diamond, TEN_Club, TEN_Spade))
            assert(move.highestCard == THREE_Spade(10))
          }
        }
      }

    }

  }


}
