import org.scalatest.FunSpec

class PlayerTest extends FunSpec {

  describe("PlayerTest") {

    // TODO - Refine these tests and clean up the nonsense
    it("should getNewHand") {
      val player = Player("test", GameUtilities.dealNewHand(54, Consants.totalNumberOfCards))
//      println(player.highCardModifier)
//      println(Player.applyCustomHighCardModifier(7))
    }

    it("should playNextMove") {

    }

  }
}
