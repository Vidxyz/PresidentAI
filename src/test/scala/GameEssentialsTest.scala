import game.{Hand, Joker, Move, NormalCard, WildCard}
import org.scalatest.FunSpec
import game.FaceValue._
import game.Suits._
import utils.Constants._

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

    describe("Tests for numberOfNormalCards()") {

      describe("When numberOfNormalCards is 0") {
        it("Should return 0 when move is comprised of special cards") {
          val move = Move(List(TWO_Heart, TWO_Spade))
          val move2 = Move(List(Joker))
          assert(move.numberOfNormalcards == 0)
          assert(move2.numberOfNormalcards == 0)
        }

        it("Should return 0 when move is comprised of WildCards") {
          val move = Move(List(THREE_Diamond(12), THREE_Club(12)))
          assert(move.numberOfNormalcards == 0)
        }
      }

      describe("When numberOfNormalCards is 1") {
        it("Should return 1") {
          val move = Move(List(THREE_Diamond(11), THREE_Club(11), THREE_Heart(11), JACK_Spade))
          assert(move.numberOfNormalcards == 1)
        }
      }

      describe("When numberOfNormalCards is 2") {
        it("Should return 2") {
          val move = Move(List(THREE_Diamond(11), THREE_Club(11), JACK_Heart, JACK_Spade))
          assert(move.numberOfNormalcards == 2)
        }
      }

      describe("When numberOfNormalCards is 3") {
        it("Should return 3") {
          val move = Move(List(THREE_Diamond(11), JACK_Club, JACK_Heart, JACK_Spade))
          assert(move.numberOfNormalcards == 3)
        }
      }

      describe("When numberOfNormalCards is 4") {
        it("Should return 4") {
          val move = Move(List(JACK_Diamond, JACK_Club, JACK_Heart, JACK_Spade))
          assert(move.numberOfNormalcards == 4)
        }
      }
    }

    describe("Tests for moveFacevalue") {

      describe("When move.cards is empty") {
        it("Should return 0") {
          assert(Move(List.empty).moveFaceValue == 0)
        }
      }

      describe("When highestCard is a WildCard") {
        it("Should return the assumed value") {
          val move = Move(List(THREE_Spade(13), KING_Heart))
          assert(move.moveFaceValue == 13)
        }
      }

      describe("When highestCard is a NormalCard") {
        it("Should return the integer value") {
          val move = Move(List(SIX_Club, SIX_Heart, SIX_Spade))
          assert(move.moveFaceValue == SIX_Spade.intValue)
        }
      }

      describe("When highestCard is a SpecialCard") {
        it("Should return the integer value") {
          val move = Move(List(TWO_Heart, TWO_Spade))
          assert(move.moveFaceValue == TWO_Spade.intValue)
        }
      }

      describe("When highestCard is a Joker") {
        it("Should return -1") {
          val move = Move(List(Joker))
          assert(move.moveFaceValue == Joker.intValue)
        }
      }

    }

  }


}
