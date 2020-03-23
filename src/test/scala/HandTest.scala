import game.{Hand, NormalCard}
import org.scalatest.FunSpec
import game.FaceValue._
import game.Suits._

class HandTest extends FunSpec {

  describe("HandTest") {

    it("should weaknessFactor") {
      val hand = Hand(List(
        NormalCard(FOUR, Diamond), NormalCard(FOUR, Club), NormalCard(FOUR, Spade),
        NormalCard(FIVE, Club),
        NormalCard(EIGHT, Diamond), NormalCard(EIGHT, Spade),
        NormalCard(JACK, Club),
        NormalCard(QUEEN, Diamond),
        NormalCard(KING, Diamond), NormalCard(KING, Heart), NormalCard(KING, Spade),
      ))

      assert(hand.weaknessFactor == 3)

    }

  }
}
